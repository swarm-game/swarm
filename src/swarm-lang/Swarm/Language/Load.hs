{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import Swarm.Failure (Asset (..), AssetData (..), Entry (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig, importLoc)
import Swarm.Language.Syntax (ImportPhaseFor, Phase (..), SwarmType, Syntax)
import Swarm.Language.Syntax.Import hiding (ImportPhase (..))
import Swarm.Language.Syntax.Import qualified as Import
import Swarm.Language.Syntax.Util (Erasable(..), traverseSyntax)
import Swarm.Language.Types (TCtx, UCtx)
import Swarm.Util (readFileMayT, showT)
import Swarm.Util.Graph (findCycle)
import Witch (into)

-- | The context for a module, containing names and types of things
--   defined in the module (once typechecking has run).
type family ModuleCtx (phase :: Phase) where
  ModuleCtx Raw = ()
  ModuleCtx Resolved = ()
  ModuleCtx Inferred = UCtx
  ModuleCtx Typed = TCtx
  ModuleCtx Elaborated = TCtx
  ModuleCtx Instantiated = TCtx

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
  }
  deriving (Generic)

deriving instance (Show (Anchor (ImportPhaseFor phase)), Show (SwarmType phase), Show (ModuleCtx phase), Show (ModuleImports phase)) => Show (Module phase)
deriving instance (Eq (ModuleImports phase), Eq (ModuleCtx phase), Eq (SwarmType phase), Eq (Anchor (ImportPhaseFor phase))) => Eq (Module phase)
deriving instance (Eq (Anchor (ImportPhaseFor phase)), Data (Anchor (ImportPhaseFor phase)), Typeable phase, Typeable (ImportPhaseFor phase), Data (ModuleCtx phase), Data (ModuleImports phase), Data (SwarmType phase)) => Data (Module phase)
deriving instance (Hashable (ModuleImports phase), Hashable (ModuleCtx phase), Hashable (SwarmType phase), Hashable (Anchor (ImportPhaseFor phase)), Generic (Anchor (ImportPhaseFor phase))) => Hashable (Module phase)

instance Erasable Module where
  erase (Module t _ _) = Module (erase <$> t) () S.empty
  eraseRaw (Module t _ _) = Module (eraseRaw <$> t) () ()

-- | A SourceMap associates canonical 'ImportLocation's to modules.
type SourceMap phase = Map (ImportLoc (ImportPhaseFor phase)) (Module phase)

-- | An AST paired with information about its recursive imports.
data SyntaxWithImports phase = SyntaxWithImports
  { getSourceMap :: SourceMap phase
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
  Syntax Raw -> m (SyntaxWithImports Resolved)
resolve s = do
  cur <- sendIO $ resolveImportDir currentDir
  (resMap, (_, s')) <- runState mempty . resolveImports cur $ s
  checkImportCycles resMap
  pure $ SyntaxWithImports resMap s'

-- | Resolve a term without requiring any I/O, throwing an error if
--   any 'import' statements are encountered.
resolve' ::
  (Has (Throw SystemFailure) sig m) => Syntax Raw -> m (Syntax Resolved)
resolve' = traverseSyntax pure (throwError . DisallowedImport)

-- | Erase type annotations from a fully processed 'SourceMap'.
eraseSourceMap :: SourceMap Elaborated -> SourceMap Resolved
eraseSourceMap = M.map erase

type ResLoc = ImportLoc Import.Resolved

-- | Convert a 'SourceMap' into a suitable form for 'findCycle'.
toImportGraph :: SourceMap Resolved -> [(ResLoc, ResLoc, [ResLoc])]
toImportGraph = map processNode . M.assocs
 where
  processNode (imp, Module _ _ imps) = (imp, imp, S.toList imps)

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

-- | Given a parent directory relative to which any local imports
--   should be interpreted, load an import and all its imports,
--   transitively.  Also return a canonicalized version of the import
--   location.
resolveImport ::
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

  srcMap <- get @(SourceMap Resolved)
  case M.lookup canonicalLoc srcMap of
    Just _ -> pure () -- Already loaded - do nothing
    Nothing -> do
      -- Record this import loc in the source map using a temporary, empty module,
      -- to prevent it from attempting to load itself recursively
      modify @(SourceMap Resolved) (M.insert canonicalLoc $ Module Nothing () mempty)

      -- Read it from network/disk
      mt <- readLoc canonicalLoc

      -- Recursively resolve any imports it contains
      mres <- traverse (resolveImports (importDir canonicalLoc)) mt
      -- sequence :: Maybe (Set a, b) -> (Set a, Maybe b)
      let (imps, mt') = sequence mres

      -- Finally, record the loaded module in the SourceMap.
      modify @(SourceMap Resolved) (M.insert canonicalLoc $ Module mt' () imps)

  -- XXX make sure imports contain ONLY defs + more imports?
  -- Otherwise we can't cache their values---we would have to
  -- re-execute them every time they are imported.

  pure canonicalLoc

-- | Try to read and parse a term from a specific import location,
--   either over the network or on disk.
readLoc ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  ImportLoc Import.Resolved ->
  m (Maybe (Syntax Raw))
readLoc loc = do
  let path = locToFilePath loc
      badImport :: Has (Throw SystemFailure) sig m => LoadingFailure -> m a
      badImport = throwError . AssetNotLoaded (Data Script) path

  -- Try to read the file from network/disk, depending on the anchor
  src <- case importAnchor loc of
    Web_ {} ->
      case parseRequest (into @String path) of
        Left err -> badImport $ BadURL (showT err)
        Right req -> do
          resp <- sendIO $ httpBS req
          case T.decodeUtf8' (getResponseBody resp) of
            Left unicodeErr -> badImport $ CanNotDecodeUTF8 unicodeErr
            Right txt -> pure txt
    _ -> sendIO (readFileMayT path) >>= maybe (badImport (DoesNotExist File)) pure

  -- Try to parse the contents
  readTerm' (defaultParserConfig & importLoc ?~ loc) src
    & either (badImport . SystemFailure . CanNotParseMegaparsec) pure
