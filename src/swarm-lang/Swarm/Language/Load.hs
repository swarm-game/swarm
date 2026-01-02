{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Loading Swarm modules from disk or network, recursively loading
-- any imports.
module Swarm.Language.Load (
  resolve,
  resolve',
  ResolvedLoc,
  SourceMap,
)
where

import Control.Algebra (Has)
import Control.Carrier.Accum.Strict (runAccum)
import Control.Carrier.State.Strict (evalState, runState)
import Control.Effect.Accum (Accum, add)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.State (State, get, modify)
import Control.Effect.Throw (Throw, throwError)
import Control.Lens ((?~))
import Control.Monad (forM_, when)
import Data.Function ((&))
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OM
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Time.Clock
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import Swarm.Failure (Asset (..), AssetData (..), Entry (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Language.Cache
import Swarm.Language.Module
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig, importLoc, runParser)
import Swarm.Language.Parser.Import (parseImportLocationRaw)
import Swarm.Language.Syntax
import Swarm.Language.Syntax.Import hiding (ImportPhase (..))
import Swarm.Language.Syntax.Import qualified as Import
import Swarm.Pretty (prettyText)
import Swarm.Util (Encoding (SystemLocale), getModificationTimeMay, readFileMayT, showT)
import Swarm.Util.GlobalCache qualified as GC
import Swarm.Util.Graph (findCycleImplicit)
import Witch (into)

-- ~~~~ Note [Module loading]
--
-- Loading modules is a bit tricky, since we have to deal with:
--   - Resolving imports, especially relative imports, to a specific location
--   - Loading imported modules from disk/network
--   - Transitively loading imports, imports of imports, etc.
--   - Checking for import cycles (+ not getting stuck in infinite recursion if so)
--   - Making sure we don't reload the same module more than once
--
-- The goal is to read things in from disk and end up with a
-- consistent, topologically sorted set of modules, where every loaded
-- module's imports have also been loaded, all modules are keyed by a
-- fully resolved and canonicalized import location, and every import
-- directive contained in source has also been resolved and
-- canonicalized.  Note that typechecking and elaboration are done as
-- separate phases after loading + resolution completes.
--
-- The top-level function is 'resolve', which calls 'resolveImports'
-- and then 'checkImportCycles'.
--
-- 'resolveImports', 'resolveImport', and 'importModule' are mutually
-- recursive and do the bulk of the work.  'resolveImports' traverses
-- an AST and calls 'resolveImport' on each import it contains.
-- 'resolveImport' processes a single import; it first checks whether
-- the import has already been loaded, and if not, it calls
-- 'importModule', which actually loads the module (via 'readLoc'),
-- calls 'resolveImports' on it, and ensures (via 'validateImport')
-- that it contains only @def@s.

-- | A SourceMap associates canonical 'ImportLocation's to modules.
--   It is an /ordered/ map, and has as an invariant that the modules
--   are topologically ordered, i.e. if B imports A, then A comes
--   before B in order.  This ensures that if we later process modules
--   in order (say, during typechecking), by the time we process a
--   given module we will have already processed any modules it
--   imports.
type SourceMap phase = OMap (ImportLoc (ImportPhaseFor phase)) (Module phase)

-- | A fully resolved + canonicalized import location.
type ResolvedLoc = ImportLoc Import.Resolved

-- | A more descriptive name for the empty set that also fixes its type.
emptyImportSet :: Set ResolvedLoc
emptyImportSet = S.empty

-- | A more descriptive name for an empty SourceMap that also fixes
--   its type.
emptySourceMap :: SourceMap Resolved
emptySourceMap = OM.empty

-- | A type synonym to represent the set of all modules encountered so
--   far.
type VisitedModules = Set ResolvedLoc

-- | A type synonym to represent the set of modules imported by a
--   particular module.
type LocalModules = Set ResolvedLoc

-- | Recursively load and resolve all the imports contained in raw
--   syntax, returning the same syntax with resolved/canonicalized
--   imports, along with the set of imports found in the top-level
--   syntax, and a SourceMap containing any recursively loaded imports
--   which were not in the cache.
--
--   Requires IO in order to load imported modules from disk or
--   network (see also 'resolve'').  Can throw a 'SystemFailure' if
--   e.g. an import is not found; if an import cycle is detected; if
--   an import contains any commands other than @def@; etc.
--
--   See Note [Module loading] for an overview.
resolve ::
  (Has (Lift IO) sig m, Has (Throw SystemFailure) sig m) =>
  -- | Provenance of the source, and location relative to which
  --   imports should be interpreted.  If Nothing, use the current
  --   working directory.
  Maybe FilePath ->
  -- | Raw syntax to be resolved.
  Syntax Raw ->
  m (SourceMap Resolved, (LocalModules, Syntax Resolved))
resolve prov s = do
  cur <- sendIO . resolveImportDir $
    case prov of
      Nothing -> currentDir
      Just fp -> case runParser parseImportLocationRaw (into @Text fp) of
        Left _ -> currentDir
        Right (loc, _) -> importDir loc

  (resMap, (impSet, s')) <-
    evalState emptyImportSet . runState emptySourceMap . resolveImports cur $ s

  -- Check for cycles in the resulting import graph
  checkImportCycles resMap

  pure (resMap, (impSet, s'))

-- | Like 'resolve', but without requiring any I/O, throwing an error
--   if any 'import' statements are encountered.
resolve' ::
  (Has (Throw SystemFailure) sig m) => Syntax Raw -> m (Syntax Resolved)
resolve' = traverseSyntax pure (throwError . DisallowedImport)

-- | Given a 'SourceMap' containing newly loaded + resolved modules,
--   ensure that the resulting import graph contains no import cycles.
--   In particular, we look for cycles in the graph that results from
--   the union of the given 'SourceMap' and the global module
--   cache---biased to the 'SourceMap' when there is overlap, since in
--   that case we have just reloaded a module which is going to
--   replace the one in the module cache once we finish typechecking +
--   elaborating it.
checkImportCycles ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  SourceMap Resolved ->
  m ()
checkImportCycles srcMap = do
  -- Find cycles in the module graph, starting from any newly loaded
  -- modules.  If any cycles were created by the newly loaded modules
  -- then we will be able to find them by searching from there.
  mcyc <- findCycleImplicit (map fst $ OM.assocs srcMap) neighbors

  -- Finally, throw an error if a cycle was found
  forM_ mcyc $ \importCycle ->
    throwError $ ImportCycle (map locToFilePath importCycle)
 where
  neighbors loc = case OM.lookup loc srcMap of
    Just m -> pure . S.toList $ moduleImports m
    Nothing -> do
      mm <- sendIO $ GC.lookupCached moduleCache loc
      pure $ maybe [] (S.toList . moduleImports) mm

-- | Given a parent directory relative to which any local imports
--   should be interpreted, traverse some raw syntax, recursively
--   resolving and loading any module imports it contains, adding any
--   newly loaded modules to the 'SourceMap' in the 'State' effect.
--   Returns the set of canonicalized imports this term contains,
--   along with a version of the syntax where all imports have been
--   resolved to their canonicalized locations.
--
--   The State VisitedModules effect is just for keeping track of
--   modules we have already seen, to avoid infinite recursion in the
--   case of an import cycle.  We cannot reuse the SourceMap for this
--   purpose, because we have to wait to add modules to the SourceMap
--   /after/ they (and all their imports, recursively) are done being
--   processed, to end up with the modules properly topologically
--   sorted.
--
--   See Note [Module loading] for an overview.
resolveImports ::
  ( Has (Throw SystemFailure) sig m
  , Has (State (SourceMap Resolved)) sig m
  , Has (State VisitedModules) sig m
  , Has (Lift IO) sig m
  ) =>
  ImportDir Import.Resolved ->
  Syntax Raw ->
  m (LocalModules, Syntax Resolved)
resolveImports parent = runAccum emptyImportSet . traverseSyntax pure (resolveImport parent)

-- | Given a parent directory relative to which any local imports
--   should be interpreted, load an import and all its imports,
--   transitively.  Also return a canonicalized version of the import
--   location, and add it to the ambient @Accum@ effect which is
--   keeping track of all imports seen in a given module.
--
--   See Note [Module loading] for an overview.
resolveImport ::
  forall sig m.
  ( Has (Throw SystemFailure) sig m
  , Has (State (SourceMap Resolved)) sig m
  , Has (State VisitedModules) sig m
  , Has (Accum LocalModules) sig m
  , Has (Lift IO) sig m
  ) =>
  ImportDir Import.Resolved ->
  ImportLoc Import.Raw ->
  m ResolvedLoc
resolveImport parent loc = do
  -- Compute the canonicalized location for the import, and record it
  canonicalLoc <- resolveImportLoc (unresolveImportDir parent <//> loc)

  -- Accumulate the resolved import location; this is used in
  -- 'resolveImports' to collect the set of all imports of a given
  -- module.
  add @LocalModules $ S.singleton canonicalLoc

  -- Check whether the module needs to be loaded (either because it is
  -- not in the cache, or the version on disk is newer than the
  -- version in the cache).
  needsLoad <- moduleNeedsLoad canonicalLoc

  -- If it does, import it and stick it in the ambient SourceMap.
  when needsLoad $ importModule canonicalLoc
  pure canonicalLoc

-- | Load a module from a specific import location, i.e. from disk or
--   over the network, as well as recursively loading any modules it
--   transitively imports.
--
--   See Note [Module loading] for an overview.
importModule ::
  ( Has (Throw SystemFailure) sig m
  , Has (State (SourceMap Resolved)) sig m
  , Has (State VisitedModules) sig m
  , Has (Lift IO) sig m
  ) =>
  ResolvedLoc ->
  m ()
importModule canonicalLoc =
  -- First check whether we have already seen this module.
  S.member canonicalLoc <$> get @VisitedModules >>= \case
    True -> pure ()
    False -> do
      -- Record that we have seen this module.
      modify @VisitedModules $ S.insert canonicalLoc

      -- Read it from network/disk
      (mt, mtime) <- readLoc canonicalLoc

      -- Recursively resolve any imports it contains
      mres <- traverse (resolveImports (importDir canonicalLoc)) mt
      -- sequence :: Maybe (Set a, b) -> (Set a, Maybe b)
      let (imps, mt') = sequence mres

      -- Build the final resolved module.
      let m = Module mt' () imps mtime (FromImport canonicalLoc)

      -- Make sure imports are pure, i.e. contain ONLY defs + imports.
      validateImport canonicalLoc m

      -- Finally, add the resolved module to the SourceMap.
      modify @(SourceMap Resolved) $ (OM.|> (canonicalLoc, m))

-- | Validate the source code of the import to ensure that it contains
--   /only/ imports and definitions.  This is so we do not have to worry
--   about side-effects happening every time a module is imported.  In
--   other words, imports must be pure so we can get away with only
--   evaluating them once.
validateImport ::
  forall sig m.
  (Has (Throw SystemFailure) sig m) =>
  ResolvedLoc ->
  Module Resolved ->
  m ()
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
--   either over the network or on disk.  Return the term as well as
--   the time it was last modified, if there is one.
readLoc ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  ResolvedLoc ->
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
