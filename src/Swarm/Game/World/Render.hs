-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- TUI-independent world rendering.
module Swarm.Game.World.Render where

import Control.Effect.Lift (sendIO)
import Control.Lens (view)
import Data.Map (Map)
import Swarm.Doc.Gen (loadStandaloneScenario)
import Swarm.Game.Location
import Swarm.Game.ResourceLoading (initNameGenerator, readAppData)
import Swarm.Game.Scenario.Status (ParameterizableLaunchParams (LaunchParams))
import Swarm.Game.Scenario.Topography.EntityFacade (mkFacade)
import Swarm.Game.State
import Swarm.Game.Terrain
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.TUI.Editor.Util (getMapRectangle)
import Swarm.Util.Effect (simpleErrorHandle)

renderScenarioMap :: FilePath -> IO ()
renderScenarioMap fp = simpleErrorHandle $ do
  (myScenario, (worldDefs, entities, recipes)) <- loadStandaloneScenario fp
  appDataMap <- readAppData
  nameGen <- initNameGenerator appDataMap
  let gsc =
        GameStateConfig
          nameGen
          entities
          recipes
          worldDefs
  gs <- sendIO $ scenarioToGameState myScenario (LaunchParams (pure Nothing) (pure Nothing)) gsc
  let worlds = view (landscape . multiWorld) gs

      mkCosmic = Cosmic DefaultRootSubworld

      grid =
        getMapRectangle
          mkFacade
          (getContent worlds . mkCosmic)
          (W.locToCoords $ Location 0 0, W.locToCoords $ Location 5 5)

  sendIO $ print "Hi there."
 where
  getContent :: Map SubworldName (W.World Int e) -> Cosmic W.Coords -> (TerrainType, Maybe e)
  getContent w coords = (underlyingCellTerrain, underlyingCellEntity)
   where
    underlyingCellEntity = W.lookupCosmicEntity coords w
    underlyingCellTerrain = W.lookupCosmicTerrain coords w
