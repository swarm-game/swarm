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
import Swarm.Game.State
import Swarm.Game.Terrain
import Swarm.Game.World qualified as W
import Swarm.TUI.Model.Name
import Swarm.TUI.Attr

-- | Render a display as a UI widget.
renderDisplay :: Display -> Widget n
renderDisplay disp = withAttr (disp ^. displayAttr . to toAttrName) $ str [displayChar disp]

-- | Render the 'Display' for a specific location.
drawLoc :: Bool -> GameState -> W.Coords -> Widget Name
drawLoc showRobots g = renderDisplay . displayLoc showRobots g

displayTerrainCell :: GameState -> W.Coords -> Display
displayTerrainCell g coords = terrainMap M.! toEnum (W.lookupTerrain coords (g ^. world))

displayEntityCell, displayRobotCell :: GameState -> W.Coords -> [Display]
displayRobotCell g coords = map (view robotDisplay) (robotsAtLocation (W.coordsToLoc coords) g)
displayEntityCell g coords = maybeToList (displayForEntity <$> W.lookupEntity coords (g ^. world))
 where
  displayForEntity :: Entity -> Display
  displayForEntity e = (if known e then id else hidden) (e ^. entityDisplay)

  known e =
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
displayLoc :: Bool -> GameState -> W.Coords -> Display
displayLoc showRobots g coords =
  sconcat $ terrain NE.:| entity <> robots
 where
  terrain = displayTerrainCell g coords
  entity = displayEntityCell g coords
  robots =
    if showRobots
      then displayRobotCell g coords
      else []

data HideEntity = HideAllEntities | HideNoEntity | HideEntityUnknownTo Robot

hidingMode :: GameState -> HideEntity
hidingMode g
  | g ^. creativeMode = HideNoEntity
  | otherwise = maybe HideAllEntities HideEntityUnknownTo $ focusedRobot g
