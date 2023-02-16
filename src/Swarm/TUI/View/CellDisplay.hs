module Swarm.TUI.View.CellDisplay where

import Brick
import Control.Lens (at, to, view, (^.))
import Data.ByteString (ByteString)
import Data.Hash.Murmur
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Semigroup (sconcat)
import Data.Tagged (unTagged)
import Data.Word (Word32)
import Linear.Affine ((.-.))
import Swarm.Game.Display
import Swarm.Game.Entity as E
import Swarm.Game.Location (euclidean)
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.Terrain
import Swarm.Game.World qualified as W
import Swarm.TUI.Attr
import Swarm.TUI.Model.Name
import Witch (from)
import Witch.Encoding qualified as Encoding

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
--   'Display's for the terrain, entity, and robots at the location,
--   taking into account the distance to another robot being @view@ed.
displayLoc :: Bool -> GameState -> W.Coords -> Display
displayLoc showRobots g coords
  -- "static" = random black + white cells
  | isStatic = if even h then mempty else terrainMap M.! IceT
  -- normal: show the terrain, entity, and robots.
  | otherwise = sconcat $ terrain NE.:| entity <> robots
 where
  terrain = displayTerrainCell g coords
  entity = displayEntityCell g coords
  robots =
    if showRobots
      then displayRobotCell g coords
      else []

  -- Offset from the location of the view center to the location under
  -- consideration for display.
  offset = W.coordsToLoc coords .-. (g ^. viewCenter)
  -- Euclidean distance from the base to the view center.
  viewDist = case g ^. robotMap . at 0 of
    Just br -> euclidean (g ^. viewCenter) (br ^. robotLocation)
    _ -> 1000000000

  -- XXX view thresholds
  viewDist1, viewDist2 :: Double
  viewDist1 = 32
  viewDist2 = 64

  -- Hash.
  h =
    murmur3 1 . unTagged . from @String @(Encoding.UTF_8 ByteString) . show $
      -- include the current tick count / 16 in the hash, so the pattern of static
      -- changes once every 16 ticks
      (offset, (g ^. ticks) `div` 16)

  -- Hashed probability, i.e. convert the hash into a floating-point number between 0 and 1
  hp :: Double
  hp = fromIntegral h / fromIntegral (maxBound :: Word32)

  isStatic
    -- Don't display static in creative mode or when the player is
    -- allowed to scroll the world
    | g ^. creativeMode || g ^. worldScrollable = False
    -- No static inside viewDist1
    | viewDist <= viewDist1 = False
    -- Completely static outside viewDist2
    | viewDist > viewDist2 = True
    -- In between, replace cell with static with a probability that
    -- linearly interpolates between 0 and 1 from viewDist1 to
    -- viewDist2
    | otherwise = hp < (viewDist - viewDist1) / (viewDist2 - viewDist1)

data HideEntity = HideAllEntities | HideNoEntity | HideEntityUnknownTo Robot

hidingMode :: GameState -> HideEntity
hidingMode g
  | g ^. creativeMode = HideNoEntity
  | otherwise = maybe HideAllEntities HideEntityUnknownTo $ focusedRobot g
