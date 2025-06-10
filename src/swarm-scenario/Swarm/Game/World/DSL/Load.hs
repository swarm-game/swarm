{-# LANGUAGE DataKinds #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Loading world descriptions from `worlds/*.world`.
module Swarm.Game.World.DSL.Load where

import Control.Algebra (Has)
import Control.Arrow (left)
import Control.Carrier.Lift (Lift, sendIO)
import Control.Carrier.Reader (runReader)
import Control.Effect.Throw (Throw, liftEither)
import Data.Map qualified as M
import Data.Text (Text)
import Swarm.Failure (Asset (..), AssetData (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Game.Land
import Swarm.Game.World.DSL.Parse (parseWExp, runParser)
import Swarm.Game.World.DSL.Typecheck
import Swarm.Pretty (prettyText)
import Swarm.ResourceLoading (getDataDirSafe)
import Swarm.Util (acquireAllWithExt)
import Swarm.Util.Effect (withThrow)
import Swarm.Util.SrcLoc (SrcLoc (..))
import System.FilePath (dropExtension, joinPath, splitPath)
import Witch (into)

-- | Load and typecheck all world descriptions from `worlds/*.world`.
--   Throw an exception if any fail to parse or typecheck.
loadWorlds ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  TerrainEntityMaps ->
  m WorldMap
loadWorlds tem = do
  dir <- getDataDirSafe Worlds "worlds"
  worldFiles <- sendIO $ acquireAllWithExt dir "world"
  ws <- mapM (loadWorld dir tem) worldFiles
  return . M.fromList $ ws

-- | Load a file containing a world DSL term, throwing an exception if
--   it fails to parse or typecheck.
loadWorld ::
  (Has (Throw SystemFailure) sig m) =>
  FilePath ->
  TerrainEntityMaps ->
  (FilePath, String) ->
  m (Text, Some (TTerm '[]))
loadWorld dir tem (fp, src) = do
  let tsrc = into @Text src
  wexp <-
    liftEither . left (AssetNotLoaded (Data Worlds) fp . SystemFailure . CanNotParseMegaparsec) $
      runParser parseWExp tsrc
  t <-
    withThrow (AssetNotLoaded (Data Worlds) fp . SystemFailure . DoesNotTypecheck NoLoc . prettyText @CheckErr) $
      runReader tem . runReader @WorldMap M.empty $
        infer CNil wexp
  return (into @Text (dropExtension (stripDir dir fp)), t)

-- | Strip a leading directory from a 'FilePath'.
stripDir :: FilePath -> FilePath -> FilePath
stripDir dir fp = joinPath (drop (length (splitPath dir)) (splitPath fp))
