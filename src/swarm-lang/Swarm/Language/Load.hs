{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Loading Swarm modules from disk or network, recursively loading
-- any imports.
module Swarm.Language.Load (
  dirToFilePath,
  locToFilePath,
  resolveImportLoc,
  Module' (..),
  Module,
  UModule,
  TModule,
  SourceMap,
  USourceMap,
  TSourceMap,
  buildSourceMap,
  load,
  loadWith,
) where

import Control.Algebra (Has)
import Control.Carrier.State.Strict (execState)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.State (State, get, modify)
import Control.Effect.Throw (Throw, throwError)
import Control.Lens (universe, view)
import Control.Monad (forM_)
import Data.Data (Data)
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Swarm.Failure (Asset (..), AssetData (..), Entry (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Language.Context (Ctx)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig)
import Swarm.Language.Syntax.AST (Syntax')
import Swarm.Language.Syntax.Import (Anchor (..), ImportDir, ImportLoc (..), currentDir, importAnchor, withImportDir)
import Swarm.Language.Syntax.Pattern (Syntax, sTerm, pattern TImportIn)
import Swarm.Language.Types (Polytype, UType, Poly, ImplicitQuantification (Quantified))
import Swarm.Util (readFileMayT)
import Swarm.Util.Graph (findCycle)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath (joinPath, splitPath, (</>))
import Witch (into)

------------------------------------------------------------
-- Import location utilities

-- | Turn an 'Anchor' into a concrete 'FilePath' (or URL).
anchorToFilePath :: (Has (Lift IO) sig m) => Anchor -> m FilePath
anchorToFilePath = \case
  Web w -> pure $ into @FilePath w
  Local n -> local n <$> sendIO getCurrentDirectory
  Home -> sendIO getHomeDirectory
  Absolute -> pure "/"
 where
  local :: Int -> FilePath -> FilePath
  local n = ("/" </>) . joinPath . reverse . drop n . reverse . splitPath

-- | Turn an 'ImportDir' into a concrete 'FilePath' (or URL).
dirToFilePath :: (Has (Lift IO) sig m) => ImportDir -> m FilePath
dirToFilePath = withImportDir $ \a p -> do
  af <- anchorToFilePath a
  pure $ af </> joinPath (map (into @FilePath) p)

-- | Turn an 'ImportLoc' into a concrete 'FilePath' (or URL).
locToFilePath :: (Has (Lift IO) sig m) => ImportLoc -> m FilePath
locToFilePath (ImportLoc d f) = do
  df <- dirToFilePath d
  pure $ df </> into @FilePath f

-- XXX simply assume web resources exist without checking?  + require them to be fully named...?

-- | Check whether a given 'ImportLoc' in fact exists.  Note that, for
--   the sake of efficiency, this simply assumes that any 'Web'
--   resource exists without checking; all other locations will
--   actually be checked.
doesLocationExist :: (Has (Lift IO) sig m) => ImportLoc -> m Bool
doesLocationExist loc = do
  fp <- locToFilePath loc
  case importAnchor loc of
    Web {} -> pure True
    _ -> sendIO $ doesFileExist fp

-- XXX need to be able to resolve "local" to something in a standard Swarm data location??

-- | Fully resolve an implicitly specified import location, relative
--   to a given base directory, possibly appending @.sw@.
--
--   Note that URLs will /not/ have @.sw@ appended automatically.
resolveImportLoc ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  ImportDir ->
  ImportLoc ->
  m ImportLoc
resolveImportLoc parent (ImportLoc d f) = do
  e1 <- doesLocationExist loc'
  e2 <- doesLocationExist loc'sw
  case (e1, e2) of
    -- Only automatically add .sw extension if the original location
    -- does not exist, but the location with .sw appended does
    (False, True) -> pure loc'sw
    _ -> pure loc'
 where
  d' = parent <> d
  loc' = ImportLoc d' f
  loc'sw = ImportLoc d' (f <> ".sw")

-- | A 'Module' is a (possibly empty) AST, along with a context for
--   any definitions contained in it, and a list of transitive,
--   canonicalized imports.
data Module' ty = Module
  { moduleTerm :: Maybe (Syntax' ty)
  , moduleCtx :: Ctx (Poly Quantified ty)
  , moduleImports :: [ImportLoc]
  }
  deriving (Data, Generic)

type Module = Module' ()
type UModule = Module' UType
type TModule = Module' Polytype

-- | A SourceMap associates canonical 'ImportLocation's to modules.
type SourceMap' ty = Map ImportLoc (Module' ty)

type SourceMap = SourceMap' ()
type USourceMap = SourceMap' UType
type TSourceMap = SourceMap' Polytype

-- | XXX
buildSourceMap ::
  (Has (Lift IO) sig m, Has (Throw SystemFailure) sig m) =>
  Syntax -> m SourceMap
buildSourceMap = load . enumerateImports

-- | Load and parse Swarm source code from a list of given import
--   locations, recursively loading and parsing any imports,
--   ultimately returning a 'SourceMap' from locations to parsed ASTs.
load ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  [ImportLoc] ->
  m SourceMap
load = loadWith M.empty

-- | Like 'load', but use an existing 'SourceMap' as a starting point.
--   Returns an updated 'SourceMap' which extends the existing one,
--   and is guaranteed to include the specified imports as well as
--   anything they import, recursively.
--
--   Any import locations which are already present in the 'SourceMap'
--   will /not/ be reloaded from the disk/network; only newly
--   encountered import locations will be loaded.  If you wish to
--   reload things from disk/network in case they have changed, use
--   'load' instead.
loadWith ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  SourceMap ->
  [ImportLoc] ->
  m SourceMap
loadWith srcMap locs = do
  resMap <- execState srcMap . mapM_ (loadRec currentDir) $ locs
  checkImportCycles resMap
  pure resMap

-- | Convert a 'SourceMap' into a suitable form for 'findCycle'.
toImportGraph :: SourceMap -> [(ImportLoc, ImportLoc, [ImportLoc])]
toImportGraph = map processNode . M.assocs
 where
  processNode (imp, Module _ _ imps) = (imp, imp, imps)

-- | Check a 'SourceMap' to ensure that it contains no import cycles.
checkImportCycles ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  SourceMap ->
  m ()
checkImportCycles srcMap = do
  forM_ (findCycle (toImportGraph srcMap)) $ \importCycle -> do
    importPaths <- mapM locToFilePath importCycle
    throwError $ ImportCycle importPaths

-- | Given a parent directory relative to which any local imports
--   should be interpreted, load an import and all its imports,
--   transitively.  Also return a canonicalized version of the import
--   location.
loadRec ::
  (Has (Throw SystemFailure) sig m, Has (State SourceMap) sig m, Has (Lift IO) sig m) =>
  ImportDir ->
  ImportLoc ->
  m ImportLoc
loadRec parent loc = do
  canonicalLoc <- resolveImportLoc parent loc
  srcMap <- get @SourceMap
  case M.lookup canonicalLoc srcMap of
    Just _ -> pure () -- Already loaded - do nothing
    Nothing -> do
      mt <- readLoc canonicalLoc -- read it from network/disk
      -- Recursively load anything it imports
      let recImports = maybe [] enumerateImports mt
      canonicalImports <- mapM (loadRec (importDir canonicalLoc)) recImports
      -- Finally, record it in the SourceMap
      modify @SourceMap (M.insert canonicalLoc $ Module mt Ctx.empty canonicalImports)

  pure canonicalLoc

-- | Enumerate all the @import@ expressions in an AST.
enumerateImports :: Syntax -> [ImportLoc]
enumerateImports = mapMaybe getImportLoc . universe . view sTerm
 where
  getImportLoc (TImportIn loc _) = Just loc
  getImportLoc _ = Nothing

-- | Try to read and parse a term from a specific import location,
--   either over the network or on disk.
readLoc ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  ImportLoc ->
  m (Maybe Syntax)
readLoc loc = do
  path <- locToFilePath loc
  let badImport = throwError . AssetNotLoaded (Data Script) path

  -- Try to read the file from network/disk
  src <- case importAnchor loc of
    Web {} -> undefined -- XXX load URL with some kind of HTTP library
    _ -> sendIO (readFileMayT path) >>= maybe (badImport (DoesNotExist File)) pure

  -- Try to parse the contents
  readTerm' defaultParserConfig src & either (badImport . SystemFailure . CanNotParseMegaparsec) pure
