{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.View.CellDisplay where

import Brick
import Control.Lens
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Semigroup (sconcat)
import Swarm.Game.Display
import Swarm.Game.Entity as E
import Swarm.Game.Robot
import Swarm.Game.Scenario.EntityFacade
import Swarm.Game.State
import Swarm.Game.Terrain (terrainMap)
import Swarm.Game.World qualified as W
import Swarm.TUI.Editor.EditorModel
import Swarm.TUI.Editor.EditorView qualified as EV
import Swarm.TUI.Editor.Util qualified as EU
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI

-- | Render the 'Display' for a specific location.
drawLoc :: UIState -> GameState -> W.Coords -> Widget Name
drawLoc ui g coords =
  if EV.shouldHideWorldCell ui coords
    then str " "
    else drawCell
 where
  showRobots = ui ^. uiShowRobots
  we = ui ^. uiWorldEditor
  drawCell = renderDisplay $ displayLoc showRobots we g coords

displayTerrainCell :: WorldEditor Name -> GameState -> W.Coords -> Display
displayTerrainCell worldEditor g coords =
  terrainMap M.! EU.getTerrainAt worldEditor (g ^. world) coords

displayRobotCell :: GameState -> W.Coords -> [Display]
displayRobotCell g coords =
  map (view robotDisplay) $
    robotsAtLocation (W.coordsToLoc coords) g

displayEntityCell :: WorldEditor Name -> GameState -> W.Coords -> [Display]
displayEntityCell worldEditor g coords =
  maybeToList $ displayForEntity <$> maybeEntity
 where
  (_, maybeEntity) = EU.getContentAt worldEditor (g ^. world) coords

  displayForEntity :: EntityPaint -> Display
  displayForEntity e = (if known e then id else hidden) $ getDisplay e

  known (Facade (EntityFacade _ _)) = True
  known (Ref e) =
    e
      `hasProperty` Known
      || (e ^. entityName)
      `elem` (g ^. knownEntities)
      || case hidingMode g of
        HideAllEntities -> False
        HideNoEntity -> True
        HideEntityUnknownTo ro -> ro `robotKnows` e

-- | Get the 'Display' for a specific location, by combining the
--   'Display's for the terrain, entity, and robots at the location.
displayLoc :: Bool -> WorldEditor Name -> GameState -> W.Coords -> Display
displayLoc showRobots worldEditor g coords =
  sconcat $ terrain NE.:| entity <> robots
 where
  terrain = displayTerrainCell worldEditor g coords
  entity = displayEntityCell worldEditor g coords
  robots =
    if showRobots
      then displayRobotCell g coords
      else []

data HideEntity = HideAllEntities | HideNoEntity | HideEntityUnknownTo Robot

hidingMode :: GameState -> HideEntity
hidingMode g
  | g ^. creativeMode = HideNoEntity
  | otherwise = maybe HideAllEntities HideEntityUnknownTo $ focusedRobot g
