{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Loading Swarm modules from disk or network, recursively loading
-- any imports.
module Swarm.Language.Load where

import Control.Algebra (Has)
import Control.Carrier.Accum.ActuallyStrict (runAccum)
import Control.Carrier.State.Strict (runState)
import Control.Effect.Accum (Accum, add)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.State (State, get, modify)
import Control.Effect.Throw (Throw, throwError)
import Control.Lens ((?~))
import Control.Monad (forM_, when)
import Data.Function ((&))
import Data.HashSet qualified as HS
import Data.Map (Map)
import Data.Map qualified as M
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
import Swarm.Util.Graph (findCycleImplicit)
import Swarm.Util.InternCache qualified as IC
import Witch (into)

-- | A SourceMap associates canonical 'ImportLocation's to modules.
type SourceMap phase = Map (ImportLoc (ImportPhaseFor phase)) (Module phase)

-- | Recursively load and resolve all the imports contained in raw
--   syntax, returning the same syntax with resolved/canonicalized
--   imports, along with a SourceMap containing any recursively loaded
--   imports which were not in the cache.
resolve ::
  (Has (Lift IO) sig m, Has (Throw SystemFailure) sig m) =>
  -- | Provenance of the source, and location relative to which
  --   imports should be interpreted.  If Nothing, use the current
  --   working directory.
  Maybe FilePath ->
  -- | Raw syntax to be resolved.
  Syntax Raw ->
  m (SourceMap Resolved, (Set (ImportLoc Import.Resolved), Syntax Resolved))
resolve prov s = do
  cur <- sendIO . resolveImportDir $
    case prov of
      Nothing -> currentDir
      Just fp -> case runParser parseImportLocationRaw (into @Text fp) of
        Left _ -> currentDir
        Right (loc, _) -> importDir loc

  (resMap, (impSet, s')) <- runState mempty . resolveImports cur $ s

  -- Check for cycles in the resulting import graph
  checkImportCycles resMap

  pure (resMap, (impSet, s'))

-- | Resolve a term without requiring any I/O, throwing an error if
--   any 'import' statements are encountered.
resolve' ::
  (Has (Throw SystemFailure) sig m) => Syntax Raw -> m (Syntax Resolved)
resolve' = traverseSyntax pure (throwError . DisallowedImport)

type ResLoc = ImportLoc Import.Resolved

-- -- | Convert a 'SourceMap' into a suitable form for 'findCycle'.
-- toImportGraph :: SourceMap Resolved -> [(ResLoc, [ResLoc])]
-- toImportGraph = map processNode . M.assocs
--  where
--   processNode (imp, m) = (imp, S.toList (moduleImports m))

-- | Given a 'SourceMap' containing newly loaded + resolved modules,
--   ensure that the resulting import graph contains no import cycles.
--   In particular, we look for cycles in the graph that results from
--   the union of the resMap and the global module cache---biased to
--   the resMap when there is overlap, since in that case we have just
--   reloaded a module which is going to replace the one in the module
--   cache once we finish typechecking + elaborating it.
checkImportCycles ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  SourceMap Resolved ->
  m ()
checkImportCycles srcMap = do
  -- Get all import locations in the module cache
  cachedLocs <- HS.toList <$> IC.cachedKeysSet moduleCache
  -- Combine with import locations that were just loaded
  let vs = S.toList $ M.keysSet srcMap `S.union` S.fromList cachedLocs
  -- Find cycles in the combined graph
  mcyc <- findCycleImplicit vs neighbors

  -- Finally, throw an error if a cycle was found
  forM_ mcyc $ \importCycle ->
    throwError $ ImportCycle (map locToFilePath importCycle)
 where
  neighbors loc = case M.lookup loc srcMap of
    Just m -> pure . S.toList $ moduleImports m
    Nothing -> do
      mm <- IC.lookupCached moduleCache loc
      pure $ maybe [] (S.toList . moduleImports) mm

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

  -- Note, the purpose of this set is not to track which imports have
  -- been loaded yet; rather, the purpose is simply to record the
  -- complete set of imports for a given module.
  --
  -- XXX do we actually use the resulting set?  It seems like
  -- currently we don't; however, perhaps we should change
  -- SyntaxWithImports to record it?
  add $ S.singleton canonicalLoc

  -- Check whether the module needs to be loaded (either because it is
  -- not in the cache, or the version on disk is newer than the
  -- version in the cache).
  needsLoad <- moduleNeedsLoad canonicalLoc
  -- If it does, import it and stick it in the ambient SourceMap
  when needsLoad $ importModule canonicalLoc
  pure canonicalLoc

-- | Load a module from a specific import location, i.e. from disk or
--   over the network, as well as recursively loading any modules it
--   transitively imports.
importModule ::
  ( Has (Throw SystemFailure) sig m
  , Has (State (SourceMap Resolved)) sig m
  , Has (Accum (Set (ImportLoc Import.Resolved))) sig m
  , Has (Lift IO) sig m
  ) =>
  ImportLoc Import.Resolved ->
  m ()
importModule canonicalLoc =
  -- Even though we know the module is not in the module cache, we
  -- still have to look it up in the SourceMap to see if it's
  -- already there---which can happen if a module transitively
  -- imports itself.  This prevents getting stuck in infinite
  -- recursion.
  M.lookup canonicalLoc <$> get @(SourceMap Resolved) >>= \case
    Just _ -> pure ()
    Nothing -> do
      -- Record this import loc in the source map using a temporary, empty module,
      -- to prevent it from attempting to load itself recursively
      modify @(SourceMap Resolved) (M.insert canonicalLoc $ Module Nothing () mempty Nothing NoProvenance)

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

      -- Add the module to the SourceMap.
      modify @(SourceMap Resolved) (M.insert canonicalLoc m)

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
--   either over the network or on disk.  Return the term as well as
--   the time it was last modified, if there is one.
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
