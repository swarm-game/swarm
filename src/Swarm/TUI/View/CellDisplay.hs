{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.View.CellDisplay where

import Brick
import Control.Lens (at, both, ix, over, to, view, (^.), (^?))
import Data.Bool (bool)
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

data HideEntity = HideAllEntities | HideNoEntity | HideEntityUnknownTo Robot

hidingMode :: GameState -> HideEntity
hidingMode g
  | g ^. creativeMode = HideNoEntity
  | otherwise = maybe HideAllEntities HideEntityUnknownTo $ focusedRobot g

-- | Get the 'Display' for a specific location, by combining the
--   'Display's for the terrain, entity, and robots at the location, and
--   taking into account "static" based on the distance to the robot
--   being @view@ed.
displayLoc :: Bool -> GameState -> W.Coords -> Display
displayLoc showRobots g coords = addStatic g coords $ displayLocRaw showRobots g coords

-- | Get the 'Display' for a specific location, by combining the
--   'Display's for the terrain, entity, and robots at the location.
displayLocRaw :: Bool -> GameState -> W.Coords -> Display
displayLocRaw showRobots g coords = sconcat $ terrain NE.:| entity <> robots
 where
  terrain = displayTerrainCell g coords
  entity = displayEntityCell g coords
  robots =
    if showRobots
      then displayRobotCell g coords
      else []

-- | Add random "static" based on the distance to the robot being
--   @view@ed.
addStatic :: GameState -> W.Coords -> Display -> Display
addStatic g coords d = maybe d displayStatic (getStatic g coords)

-- | Draw black or white static.
displayStatic :: Bool -> Display
displayStatic = bool mempty (terrainMap M.! IceT)

-- | Random "static" based on the distance to the robot being
--   @view@ed.  A cell can either be static-free (represented by
--   @Nothing@) or can have one of two types ("black" or "white").
getStatic :: GameState -> W.Coords -> Maybe Bool
getStatic g coords
  | isStatic = Just (even h)
  | otherwise = Nothing
 where
  -- Offset from the location of the view center to the location under
  -- consideration for display.
  offset = W.coordsToLoc coords .-. (g ^. viewCenter)
  -- Euclidean distance from the base to the view center.
  viewDist = case g ^. robotMap . at 0 of
    Just br -> euclidean (g ^. viewCenter) (br ^. robotLocation)
    _ -> 1000000000

  baseInv :: Maybe Inventory
  baseInv = g ^? robotMap . ix 0 . equippedDevices
  focInv = view equippedDevices <$> focusedRobot g

  gain :: Maybe Inventory -> (Double -> Double)
  gain (Just inv)
    | E.countByName "antenna" inv > 0 = (* 2)
  gain _ = id

  -- View thresholds.  Default thresholds are 16, 64; each antenna
  -- boosts the signal by 2x.
  viewDist1, viewDist2 :: Double
  (viewDist1, viewDist2) = over both (gain baseInv . gain focInv) (16, 64)

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
    -- interpolates between 0 and 1 from viewDist1 to
    -- viewDist2.
    | otherwise = hp < 1 - cos (s * (pi / 2))
   where
    s = (viewDist - viewDist1) / (viewDist2 - viewDist1)
