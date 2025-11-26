{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Loading Swarm modules from disk or network, recursively loading
-- any imports.
module Swarm.Language.Load where

import Control.Algebra (Has)
import Control.Carrier.Accum.Strict (runAccum)
import Control.Carrier.State.Strict (runState)
import Control.Effect.Accum (Accum, add)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.State (State, get, modify)
import Control.Effect.Throw (Throw, throwError)
import Control.Lens ((?~))
import Control.Monad (forM_)
import Data.Data (Data, Typeable)
import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Time.Clock
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import Swarm.Failure (Asset (..), AssetData (..), Entry (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig, importLoc, runParser)
import Swarm.Language.Parser.Import (parseImportLocationRaw)
import Swarm.Language.Syntax
import Swarm.Language.Syntax.Import hiding (ImportPhase (..))
import Swarm.Language.Syntax.Import qualified as Import
import Swarm.Language.Syntax.Util (Erasable (..))
import Swarm.Language.Types (TCtx, TDCtx, UCtx)
import Swarm.Pretty (prettyText)
import Swarm.Util (Encoding (SystemLocale), getModificationTimeMay, readFileMayT, showT)
import Swarm.Util.Graph (findCycle)
import Swarm.Util.InternCache
import Swarm.Util.InternCache qualified as IC
import System.Directory (getModificationTime)
import System.IO.Unsafe (unsafePerformIO)
import Witch (into)

type ResLoc = ImportLoc Import.Resolved

-- | The context for a module, containing names and types of things
--   defined in the module (once typechecking has run).
type family ModuleCtx (phase :: Phase) where
  ModuleCtx Raw = ()
  ModuleCtx Resolved = ()
  ModuleCtx Inferred = (UCtx, TDCtx)
  ModuleCtx Typed = (TCtx, TDCtx)
  ModuleCtx Elaborated = (TCtx, TDCtx)
  ModuleCtx Instantiated = (TCtx, TDCtx)

-- | A module only needs to record its imports during resolution, so
--   we can do cyclic import detection.  After that we no longer
--   require the information.
type family ModuleImports (phase :: Phase) where
  ModuleImports Raw = ()
  ModuleImports Resolved = Set (ImportLoc Import.Resolved)
  ModuleImports Inferred = ()
  ModuleImports Typed = ()
  ModuleImports Elaborated = ()
  ModuleImports Instantiated = ()

-- | A 'Module' is a (possibly empty) AST, along with a context for
--   any definitions contained in it, and a list of transitive,
--   canonicalized imports.
data Module phase = Module
  { moduleTerm :: Maybe (Syntax phase)
  -- ^ The contents of the module.
  , moduleCtx :: ModuleCtx phase
  -- ^ The context of names defined in this module and their types.
  , moduleImports :: ModuleImports phase
  -- ^ The moduleImports are mostly for convenience, e.g. for checking modules for cycles.
  , moduleTimestamp :: Maybe UTCTime
  -- ^ The time at which the module was loaded
  }
  deriving (Generic)

deriving instance (Show (Anchor (ImportPhaseFor phase)), Show (SwarmType phase), Show (ModuleCtx phase), Show (ModuleImports phase)) => Show (Module phase)
deriving instance (Eq (ModuleImports phase), Eq (ModuleCtx phase), Eq (SwarmType phase), Eq (Anchor (ImportPhaseFor phase))) => Eq (Module phase)
deriving instance (Eq (Anchor (ImportPhaseFor phase)), Data (Anchor (ImportPhaseFor phase)), Typeable phase, Typeable (ImportPhaseFor phase), Data (ModuleCtx phase), Data (ModuleImports phase), Data (SwarmType phase)) => Data (Module phase)
deriving instance (Hashable (ModuleImports phase), Hashable (ModuleCtx phase), Hashable (SwarmType phase), Hashable (Anchor (ImportPhaseFor phase)), Generic (Anchor (ImportPhaseFor phase))) => Hashable (Module phase)

instance Erasable Module where
  erase (Module t _ _ time) = Module (erase <$> t) () S.empty time
  eraseRaw (Module t _ _ time) = Module (eraseRaw <$> t) () () time

-- | A SourceMap associates canonical 'ImportLocation's to modules.
type SourceMap phase = Map (ImportLoc (ImportPhaseFor phase)) (Module phase)

-- | An AST paired with information about its recursive imports and
--   its provenance.
data SyntaxWithImports phase = SyntaxWithImports
  { getProvenance :: Maybe FilePath
  , getSourceMap :: SourceMap phase
  , getSyntax :: Syntax phase
  }
  deriving (Generic)

deriving instance (Show (Anchor (ImportPhaseFor phase)), Show (SwarmType phase), Show (ModuleCtx phase), Show (ModuleImports phase)) => Show (SyntaxWithImports phase)
deriving instance (Eq (Anchor (ImportPhaseFor phase)), Eq (SwarmType phase), Eq (ModuleCtx phase), Eq (ModuleImports phase)) => Eq (SyntaxWithImports phase)
deriving instance (Ord (Anchor (ImportPhaseFor phase)), Data (Anchor (ImportPhaseFor phase)), Typeable phase, Typeable (ImportPhaseFor phase), Data (ModuleCtx phase), Data (ModuleImports phase), Data (SwarmType phase)) => Data (SyntaxWithImports phase)

-- | Recursively resolve and load all the imports contained in raw
--   syntax, returning the same syntax with resolved/canonicalized
--   imports as well as a SourceMap containing all the loaded imports.
resolve ::
  (Has (Lift IO) sig m, Has (Throw SystemFailure) sig m) =>
  -- | Provenance of the source, and location relative to which
  --   imports should be interpreted.  If Nothing, use the current
  --   working directory.
  Maybe FilePath ->
  -- | Raw syntax to be resolved.
  Syntax Raw ->
  m (SyntaxWithImports Resolved)
resolve prov s = do
  cur <- sendIO . resolveImportDir $
    case prov of
      Nothing -> currentDir
      Just fp -> case runParser parseImportLocationRaw (into @Text fp) of
        Left _ -> currentDir
        Right (loc, _) -> importDir loc
  (resMap, (_, s')) <- runState mempty . resolveImports cur $ s
  checkImportCycles resMap
  pure $ SyntaxWithImports prov resMap s'

-- | Resolve a term without requiring any I/O, throwing an error if
--   any 'import' statements are encountered.
resolve' ::
  (Has (Throw SystemFailure) sig m) => Syntax Raw -> m (Syntax Resolved)
resolve' = traverseSyntax pure (throwError . DisallowedImport)

-- | Erase type annotations from a fully processed 'SourceMap'.
eraseSourceMap :: SourceMap Elaborated -> SourceMap Resolved
eraseSourceMap = M.map erase

-- | Convert a 'SourceMap' into a suitable form for 'findCycle'.
toImportGraph :: SourceMap Resolved -> [(ResLoc, ResLoc, [ResLoc])]
toImportGraph = map processNode . M.assocs
 where
  processNode (imp, m) = (imp, imp, S.toList (moduleImports m))

-- | Check a 'SourceMap' to ensure that it contains no import cycles.
checkImportCycles ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  SourceMap Resolved ->
  m ()
checkImportCycles srcMap = do
  forM_ (findCycle (toImportGraph srcMap)) $ \importCycle ->
    throwError $ ImportCycle (map locToFilePath importCycle)

-- | Given a parent directory relative to which any local imports
--   should be interpreted, traverse some raw syntax, recursively
--   resolving and loading any imports it contains.  Returns the set
--   of canonicalized imports this term contains, along with a version
--   of the syntax where all imports have been resolved to their
--   canonicalized locations.
resolveImports ::
  ( Has (Throw SystemFailure) sig m
  , Has (State (SourceMap Resolved)) sig m
  , Has (Lift IO) sig m
  ) =>
  ImportDir Import.Resolved ->
  Syntax Raw ->
  m (Set (ImportLoc Import.Resolved), Syntax Resolved)
resolveImports parent = runAccum S.empty . traverseSyntax pure (resolveImport parent)

-- | Cache imported modules.
moduleCache :: (Has (Lift IO) sig m) => InternCache m (ImportLoc Import.Resolved) (Module Resolved)
moduleCache = unsafePerformIO $ hoist sendIO <$> IC.newInternCache @_ @IO
{-# NOINLINE moduleCache #-}

-- | Given a parent directory relative to which any local imports
--   should be interpreted, load an import and all its imports,
--   transitively.  Also return a canonicalized version of the import
--   location.
resolveImport ::
  forall sig m.
  ( Has (Throw SystemFailure) sig m
  , Has (State (SourceMap Resolved)) sig m
  , Has (Accum (Set (ImportLoc Import.Resolved))) sig m
  , Has (Lift IO) sig m
  ) =>
  ImportDir Import.Resolved ->
  ImportLoc Import.Raw ->
  m (ImportLoc Import.Resolved)
resolveImport parent loc = do
  -- Compute the canonicalized location for the import, and record it
  canonicalLoc <- resolveImportLoc (unresolveImportDir parent <//> loc)
  add $ S.singleton canonicalLoc

  -- Load + check the module, either from the cache if possible or
  -- from disk/network if necessary.
  m <- IC.updateCached moduleCache importModule isOutdated canonicalLoc

  -- Add the module to the SourceMap.
  modify @(SourceMap Resolved) (M.insert canonicalLoc m)

  pure canonicalLoc
 where
  -- Check whether a cached module is outdated and needs to be reloaded + rechecked.
  isOutdated :: ImportLoc Import.Resolved -> Module Resolved -> m Bool
  isOutdated cloc (moduleTimestamp -> mt) = case locToPath cloc of
    -- URLs are never considered outdated.  There is no consistent way
    -- to get a "modification time" for a remote file; in any case,
    -- typically, we expect modules loaded from URLs to change much
    -- less frequently than those loaded from local files.  If you
    -- want to pick up a change to a module imported from a URL, you
    -- can restart the entire app to clear the cache.  (Quitting the
    -- scenario and restarting may work as well, as long as the cache
    -- entry gets GC'd.)
    URL {} -> pure False
    -- For local files, get the modification time and compare to
    -- stored timestamp.
    LocalPath f ->
      maybe
        (pure True) -- Modules without a timestamp are always outdated
        (\t -> (t <) <$> sendIO (getModificationTime f))
        mt

  -- Actually load a module from disk or network and check it.
  importModule :: ImportLoc Import.Resolved -> m (Module Resolved)
  importModule cloc =
    -- Even though we know the module is not in the module cache, we
    -- still have to look it up in the SourceMap to see if it's
    -- already there---which can happen if a module transitively
    -- imports itself.  This prevents getting stuck in infinite
    -- recursion.
    M.lookup cloc <$> get @(SourceMap Resolved) >>= \case
      Just m -> pure m
      Nothing -> do
        -- Record this import loc in the source map using a temporary, empty module,
        -- to prevent it from attempting to load itself recursively
        modify @(SourceMap Resolved) (M.insert cloc $ Module Nothing () mempty Nothing)

        -- Read it from network/disk
        (mt, mtime) <- readLoc cloc

        -- Recursively resolve any imports it contains
        mres <- traverse (resolveImports (importDir cloc)) mt
        -- sequence :: Maybe (Set a, b) -> (Set a, Maybe b)
        let (imps, mt') = sequence mres

        -- Build the final checked module.
        let m = Module mt' () imps mtime

        -- Make sure imports are pure, i.e. contain ONLY defs + imports.
        validateImport cloc m

        -- Finally, return it.
        pure m

-- | Validate the source code of the import to ensure that it contains
--   *only* imports and definitions.  This is so we do not have to worry
--   about side-effects happening every time a module is imported.  In
--   other words, imports must be pure so we can get away with only
--   evaluating them once.
validateImport :: forall sig m. (Has (Throw SystemFailure) sig m) => ResLoc -> Module Resolved -> m ()
validateImport loc = maybe (pure ()) validate . moduleTerm
 where
  validate :: Syntax Resolved -> m ()
  validate = validateTerm . _sTerm

  validateTerm :: Term Resolved -> m ()
  validateTerm = \case
    SLet LSDef _ _ _ _ _ _ t -> validate t
    SImportIn _ t -> validate t
    STydef _ _ _ t -> validate t
    TConst Noop -> pure ()
    t -> throwError $ ImpureImport loc (prettyText t)

-- | Try to read and parse a term from a specific import location,
--   either over the network or on disk.
readLoc ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  ImportLoc Import.Resolved ->
  m (Maybe (Syntax Raw), Maybe UTCTime)
readLoc loc = do
  -- Try to read the file from network/disk, depending on the anchor
  (src, mtime) <- case importAnchor loc of
    Web_ {} -> readFromNet
    _ -> readFromDisk
  -- Finally, try to parse the contents
  syn <-
    readTerm' (defaultParserConfig & importLoc ?~ loc) src
      & withBadImport (SystemFailure . CanNotParseMegaparsec)
  pure (syn, mtime)
 where
  path = locToFilePath loc
  badImport :: Has (Throw SystemFailure) sig m => LoadingFailure -> m a
  badImport = throwError . AssetNotLoaded (Data Script) path
  withBadImport :: Has (Throw SystemFailure) sig m => (e -> LoadingFailure) -> Either e a -> m a
  withBadImport f = either (badImport . f) pure
  readFromDisk = do
    mcontent <- sendIO (readFileMayT SystemLocale path)
    content <- maybe (badImport (DoesNotExist File)) pure mcontent
    mt <- sendIO $ getModificationTimeMay path
    pure (content, mt)
  readFromNet = do
    -- Try to parse the URL
    req <- parseRequest (into @String path) & withBadImport (BadURL . showT)
    -- Send HTTP request
    resp <- sendIO $ httpBS req
    -- Try to decode the response
    content <- T.decodeUtf8' (getResponseBody resp) & withBadImport CanNotDecodeUTF8
    time <- sendIO getCurrentTime
    pure (content, Just time)
