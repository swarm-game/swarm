{-# LANGUAGE DataKinds #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Evaluation for the Swarm world description DSL.
module Swarm.Game.World.Eval where

import Control.Arrow (left)
import Control.Carrier.Reader (runReader)
import Control.Carrier.Throw.Either (run, runThrow)
import Data.Bifunctor (second)
import Data.Either (partitionEithers)
import Data.Map qualified as M
import Data.Text (Text)
import Swarm.Game.Entity (Entity, EntityMap)
import Swarm.Game.Failure (Asset (..), AssetData (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Game.ResourceLoading (getDataDirSafe)
import Swarm.Game.Terrain (TerrainType (..))
import Swarm.Game.World (WorldFun (..))
import Swarm.Game.World.Abstract (bracket)
import Swarm.Game.World.Coords (Coords)
import Swarm.Game.World.Gen (Seed)
import Swarm.Game.World.Interpret (interpBTerm)
import Swarm.Game.World.Parse (parseWExp, runParser)
import Swarm.Game.World.Syntax
import Swarm.Game.World.Typecheck
import Swarm.Language.Pretty (prettyText)
import Swarm.Util (acquireAllWithExt)
import System.FilePath (dropExtension, joinPath, splitPath)
import Witch (into)

runWorld :: TTerm '[] (World CellVal) -> Seed -> WorldFun TerrainType Entity
runWorld t seed = convert . interpBTerm seed . bracket $ t

-- runCTerm . compile seed . bracket

convert :: (Coords -> CellVal) -> WorldFun TerrainType Entity
convert f = WF ((\(CellVal t e _) -> (t, e)) . f)

loadWorlds ::
  (Has (Accum (Seq SystemFailure)) sig m, Has (Lift IO) sig m)
  => EntityMap -> m WorldMap
loadWorlds em = do
  res <- throwToWarning @SystemFailure $ getDataDirSafe Worlds "worlds"
  case res of
    Nothing -> return M.empty
    Just dir -> do
      worldFiles <- sendIO $ acquireAllWithExt dir "world"
      ws <- mapM (throwToWarning @SystemFailure . processWorldFile dir em) worldFiles
      return . M.fromList . catMaybes $ ws

processWorldFile ::
  (Has (Throw SystemFailure) sig m) =>
  FilePath ->
  EntityMap ->
  (FilePath, String) ->
  m (Text, Some (TTerm '[]))
processWorldFile dir em (fp, src) = do
  wexp <-
    left (AssetNotLoaded (Data Worlds) fp . CanNotParseMegaparsec) $
      runParser parseWExp (into @Text src)
  t <-
    left (AssetNotLoaded (Data Worlds) fp . CustomMessage . prettyText) $
      run . runThrow @CheckErr . runReader em . runReader @WorldMap M.empty $
        infer CNil wexp
  return (into @Text (dropExtension (stripDir dir fp)), t)

stripDir :: FilePath -> FilePath -> FilePath
stripDir dir fp = joinPath (drop (length (splitPath dir)) (splitPath fp))
