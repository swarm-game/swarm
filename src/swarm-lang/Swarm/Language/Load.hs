{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Loading Swarm modules from disk or network, recursively loading
-- any imports.
module Swarm.Language.Load where
-- XXX
-- (
--   dirToFilePath,
--   locToFilePath,
--   resolveImportLoc,
--   Module (..),
--   SourceMap,
--   buildSourceMap,
--   load,
--   loadWith,
-- ) where

import Control.Algebra (Has)
import Control.Carrier.Accum.Strict (runAccum)
import Control.Carrier.State.Strict (runState)
import Control.Effect.Accum (Accum)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.State (State, get, modify)
import Control.Effect.Throw (Throw, throwError)
import Control.Monad (forM_)
import Data.Data (Data, Typeable)
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import GHC.Generics (Generic)
import Swarm.Failure (Asset (..), AssetData (..), Entry (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Language.Context (Ctx)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig)
import Swarm.Language.Syntax (Phase (..))
import Swarm.Language.Syntax.AST (Syntax, SwarmType)
import Swarm.Language.Syntax.Import
import Swarm.Language.Syntax.Util (traverseSyntax)
import Swarm.Language.Types (Poly, ImplicitQuantification (Quantified))
import Swarm.Language.Var (Var)
import Swarm.Util (readFileMayT)
import Swarm.Util.Graph (findCycle)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath (joinPath, splitPath, (</>))
import Witch (into)

-- | A 'Module' is a (possibly empty) AST, along with a context for
--   any definitions contained in it, and a list of transitive,
--   canonicalized imports.
data Module phase = Module
  { -- | The contents of the module.
    moduleTerm :: Maybe (Syntax phase)

    -- | The context of names defined in this module and their types.
  , moduleCtx :: Ctx Var (Poly Quantified (SwarmType phase))
    -- XXX moduleCtx should depend on phase with type family?

    -- | The moduleImports are mostly for convenience, e.g. for checking modules for cycles.
  , moduleImports :: Set (ImportLoc phase)
  }
  deriving (Generic)

deriving instance (Typeable phase, Data (SwarmType phase), Data (ResolvedDir phase), Data (ResolvedFile phase), Ord (ResolvedDir phase), Ord (ResolvedFile phase)) => Data (Module phase)

-- | A SourceMap associates canonical 'ImportLocation's to modules.
type SourceMap phase = Map (ImportLoc phase) (Module phase)

-- | XXX
buildSourceMap ::
  (Has (Lift IO) sig m, Has (Throw SystemFailure) sig m) =>
  Syntax Raw -> m (Syntax Resolved, SourceMap Resolved)
buildSourceMap s = do
  (resMap, (_, s')) <- runState mempty . resolveImports currentDir $ s
  checkImportCycles resMap
  pure (s', resMap)

-- | Convert a 'SourceMap' into a suitable form for 'findCycle'.
toImportGraph :: SourceMap phase -> [(ImportLoc phase, ImportLoc phase, [ImportLoc phase])]
toImportGraph = map processNode . M.assocs
 where
  processNode (imp, Module _ _ imps) = (imp, imp, S.toList imps)

-- | Check a 'SourceMap' to ensure that it contains no import cycles.
checkImportCycles ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  SourceMap Resolved ->
  m ()
checkImportCycles srcMap = do
  forM_ (findCycle (toImportGraph srcMap)) $ \importCycle -> do
    importPaths <- mapM locToFilePath importCycle
    throwError $ ImportCycle importPaths

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
  ImportDir ->
  Syntax Raw ->
  m (Set (ImportLoc Resolved), Syntax Resolved)
resolveImports parent = runAccum S.empty . traverseSyntax pure (resolveImport parent)

-- | Given a parent directory relative to which any local imports
--   should be interpreted, load an import and all its imports,
--   transitively.  Also return a canonicalized version of the import
--   location.
resolveImport ::
  ( Has (Throw SystemFailure) sig m
  , Has (State (SourceMap Resolved)) sig m
  , Has (Accum (Set (ImportLoc Resolved))) sig m
  , Has (Lift IO) sig m
  ) =>
  ImportDir ->
  ImportLoc Raw ->
  m (ImportLoc Resolved)
resolveImport parent loc = do
   canonicalLoc <- resolveImportLoc parent loc
   srcMap <- get @(SourceMap Resolved)
   case M.lookup canonicalLoc srcMap of
     Just _ -> pure () -- Already loaded - do nothing
     Nothing -> do
       -- Record this import loc in the source map using a temporary, empty module,
       -- to prevent it from attempting to load itself recursively
       modify @(SourceMap Resolved) (M.insert canonicalLoc $ Module Nothing Ctx.empty mempty)

       -- Read it from network/disk
       mt <- readLoc canonicalLoc

       -- Recursively resolve any imports it contains
       mres <- traverse (resolveImports (importDirRes canonicalLoc)) mt
       -- sequence :: Maybe (Set a, b) -> (Set a, Maybe b)
       let (imps, mt') = sequence mres

       -- Finally, record the loaded module in the SourceMap.
       modify @(SourceMap Resolved) (M.insert canonicalLoc $ Module mt' Ctx.empty imps)

   pure canonicalLoc

-- | Try to read and parse a term from a specific import location,
--   either over the network or on disk.
readLoc ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  ImportLoc Resolved ->
  m (Maybe (Syntax Raw))
readLoc loc = do
  path <- locToFilePath loc
  let badImport = throwError . AssetNotLoaded (Data Script) path

  -- Try to read the file from network/disk
  src <- case importAnchorResolved loc of
    Web {} -> error "readLoc Web unimplemented" -- XXX load URL with some kind of HTTP library
    _ -> sendIO (readFileMayT path) >>= maybe (badImport (DoesNotExist File)) pure

  -- Try to parse the contents
  readTerm' defaultParserConfig src & either (badImport . SystemFailure . CanNotParseMegaparsec) pure
