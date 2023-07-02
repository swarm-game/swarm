{-# LANGUAGE OverloadedStrings #-}

-- |
-- Rendering of cells in the map view
--
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.CellDisplay where

import Brick
import Control.Lens (to, view, (&), (.~), (^.))
import Data.ByteString (ByteString)
import Data.Hash.Murmur
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Semigroup (sconcat)
import Data.Tagged (unTagged)
import Data.Word (Word32)
import Linear.Affine ((.-.))
import Swarm.Game.CESK (TickNumber (..))
import Swarm.Game.Display
import Swarm.Game.Entity
import Swarm.Game.Robot
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.State
import Swarm.Game.Terrain
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.TUI.Attr
import Swarm.TUI.Editor.Masking
import Swarm.TUI.Editor.Model
import Swarm.TUI.Editor.Util qualified as EU
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI
import Witch (from)
import Witch.Encoding qualified as Encoding

-- | Render a display as a UI widget.
renderDisplay :: Display -> Widget n
renderDisplay disp = withAttr (disp ^. displayAttr . to toAttrName) $ str [displayChar disp]

-- | Render the 'Display' for a specific location.
drawLoc :: UIState -> GameState -> Cosmo W.Coords -> Widget Name
drawLoc ui g cCoords@(Cosmo _ coords) =
  if shouldHideWorldCell ui coords
    then str " "
    else drawCell
 where
  showRobots = ui ^. uiShowRobots
  we = ui ^. uiWorldEditor
  drawCell = renderDisplay $ displayLoc showRobots we g cCoords

displayTerrainCell ::
  WorldEditor Name ->
  GameState ->
  Cosmo W.Coords ->
  Display
displayTerrainCell worldEditor g coords =
  terrainMap M.! EU.getTerrainAt worldEditor (g ^. multiWorld) coords

displayRobotCell ::
  GameState ->
  Cosmo W.Coords ->
  [Display]
displayRobotCell g coords =
  map (view robotDisplay) $
    robotsAtLocation (fmap W.coordsToLoc coords) g

displayEntityCell :: WorldEditor Name -> GameState -> Cosmo W.Coords -> [Display]
displayEntityCell worldEditor g coords =
  maybeToList $ displayForEntity <$> maybeEntity
 where
  (_, maybeEntity) = EU.getContentAt worldEditor (g ^. multiWorld) coords

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

data HideEntity = HideAllEntities | HideNoEntity | HideEntityUnknownTo Robot

hidingMode :: GameState -> HideEntity
hidingMode g
  | g ^. creativeMode = HideNoEntity
  | otherwise = maybe HideAllEntities HideEntityUnknownTo $ focusedRobot g

-- | Get the 'Display' for a specific location, by combining the
--   'Display's for the terrain, entity, and robots at the location, and
--   taking into account "static" based on the distance to the robot
--   being @view@ed.
displayLoc :: Bool -> WorldEditor Name -> GameState -> Cosmo W.Coords -> Display
displayLoc showRobots we g cCoords@(Cosmo _ coords) =
  staticDisplay g coords
    <> displayLocRaw showRobots we g cCoords

-- | Get the 'Display' for a specific location, by combining the
--   'Display's for the terrain, entity, and robots at the location.
displayLocRaw ::
  Bool ->
  WorldEditor Name ->
  GameState ->
  Cosmo W.Coords ->
  Display
displayLocRaw showRobots worldEditor g coords = sconcat $ terrain NE.:| entity <> robots
 where
  terrain = displayTerrainCell worldEditor g coords
  entity = displayEntityCell worldEditor g coords
  robots =
    if showRobots
      then displayRobotCell g coords
      else []

-- | Random "static" based on the distance to the robot being
--   @view@ed.
staticDisplay :: GameState -> W.Coords -> Display
staticDisplay g coords = maybe mempty displayStatic (getStatic g coords)

-- | Draw static given a number from 0-15 representing the state of
--   the four quarter-pixels in a cell
displayStatic :: Word32 -> Display
displayStatic s =
  defaultEntityDisplay (staticChar s)
    & displayPriority .~ maxBound -- Static has higher priority than anything else
    & displayAttr .~ AEntity

-- | Given a value from 0--15, considered as 4 bits, pick the
--   character with the corresponding quarter pixels turned on.
staticChar :: Word32 -> Char
staticChar = \case
  0 -> ' '
  1 -> '▖'
  2 -> '▗'
  3 -> '▄'
  4 -> '▘'
  5 -> '▌'
  6 -> '▚'
  7 -> '▙'
  8 -> '▝'
  9 -> '▞'
  10 -> '▐'
  11 -> '▟'
  12 -> '▀'
  13 -> '▛'
  14 -> '▜'
  15 -> '█'
  _ -> ' '

-- | Random "static" based on the distance to the robot being
--   @view@ed.  A cell can either be static-free (represented by
--   @Nothing@) or can have one of sixteen values (representing the
--   state of the four quarter-pixels in one cell).
getStatic :: GameState -> W.Coords -> Maybe Word32
getStatic g coords
  | isStatic = Just (h `mod` 16)
  | otherwise = Nothing
 where
  -- Offset from the location of the view center to the location under
  -- consideration for display.
  offset = W.coordsToLoc coords .-. (g ^. viewCenter . planar)

  -- Hash.
  h =
    murmur3 1 . unTagged . from @String @(Encoding.UTF_8 ByteString) . show $
      -- include the current tick count / 16 in the hash, so the pattern of static
      -- changes once every 16 ticks
      (offset, getTickNumber (g ^. ticks) `div` 16)

  -- Hashed probability, i.e. convert the hash into a floating-point number between 0 and 1
  hp :: Double
  hp = fromIntegral h / fromIntegral (maxBound :: Word32)

  isStatic = case focusedRange g of
    -- If we're not viewing a robot, display static.  This
    -- can happen if e.g. the robot we were viewing drowned.
    -- This is overridden by creative mode, e.g. when no robots
    -- have been defined for the scenario.
    Nothing -> not $ g ^. creativeMode
    -- Don't display static if the robot is close, or when we're in
    -- creative mode or the player is allowed to scroll the world.
    Just Close -> False
    -- At medium distances, replace cell with static with a
    -- probability that increases with distance.
    Just (MidRange s) -> hp < 1 - cos (s * (pi / 2))
    -- Far away, everything is static.
    Just Far -> True
