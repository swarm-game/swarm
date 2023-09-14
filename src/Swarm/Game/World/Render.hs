-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- TUI-independent world rendering.
module Swarm.Game.World.Render where

import Control.Effect.Lift (sendIO)
import Control.Lens (view)
import Control.Monad (forM_)
import Data.List.NonEmpty qualified as NE
import Swarm.Doc.Gen (loadStandaloneScenario)
import Swarm.Game.Display (defaultChar)
import Swarm.Game.ResourceLoading (initNameGenerator, readAppData)
import Swarm.Game.Scenario (Scenario, area, scenarioWorlds, ul, worldName)
import Swarm.Game.Scenario.Status (emptyLaunchParams)
import Swarm.Game.Scenario.Topography.Area (getAreaDimensions, upperLeftToBottomRight)
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.EntityFacade (EntityFacade (..), mkFacade)
import Swarm.Game.State
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.TUI.Editor.Util (getContentAt, getMapRectangle)
import Swarm.Util.Effect (simpleErrorHandle)
import Swarm.Util.Erasable (erasableToMaybe)

getDisplayChar :: PCell EntityFacade -> Char
getDisplayChar = maybe ' ' facadeChar . erasableToMaybe . cellEntity
 where
  facadeChar (EntityFacade _ d) = view defaultChar d

getDisplayGrid :: Scenario -> GameState -> [[PCell EntityFacade]]
getDisplayGrid myScenario gs =
  getMapRectangle
    mkFacade
    (getContentAt worlds . mkCosmic)
    boundingBox
 where
  worlds = view (landscape . multiWorld) gs

  firstScenarioWorld = NE.head $ view scenarioWorlds myScenario
  worldArea = area firstScenarioWorld
  upperLeftLocation = ul firstScenarioWorld
  areaDims = getAreaDimensions worldArea
  lowerRightLocation = upperLeftToBottomRight areaDims upperLeftLocation

  mkCosmic = Cosmic $ worldName firstScenarioWorld
  boundingBox = (W.locToCoords upperLeftLocation, W.locToCoords lowerRightLocation)

renderScenarioMap :: FilePath -> IO [String]
renderScenarioMap fp = simpleErrorHandle $ do
  (myScenario, (worldDefs, entities, recipes)) <- loadStandaloneScenario fp
  appDataMap <- readAppData
  nameGen <- initNameGenerator appDataMap
  let gsc = GameStateConfig nameGen entities recipes worldDefs
  gs <- sendIO $ scenarioToGameState myScenario emptyLaunchParams gsc
  let grid = getDisplayGrid myScenario gs

  return $ map (map getDisplayChar) grid

printScenarioMap :: [String] -> IO ()
printScenarioMap grid =
  sendIO $ forM_ grid putStrLn
