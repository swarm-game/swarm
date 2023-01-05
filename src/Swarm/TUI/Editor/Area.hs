{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.Editor.Area where

import Linear (V2 (..))
import Swarm.Util.Location
import Data.Maybe (listToMaybe)
import Data.Int (Int32)
import Data.List qualified as L

data AreaDimensions = AreaDimensions
  { rectWidth :: Int32
  , rectHeight :: Int32
  }

renderRectDimensions :: AreaDimensions -> String
renderRectDimensions (AreaDimensions w h) =
  L.intercalate "x" $ map show [w, h]

invertVecY :: V2 Int32 -> V2 Int32
invertVecY (V2 x y) = V2 x (-y)

-- | Incorporates an offset by -1, since the area is
-- "inclusive" of the lower-right coordinate.
upperLeftToBottomRight :: AreaDimensions -> Location -> Location
upperLeftToBottomRight (AreaDimensions w h) upperLeft =
  upperLeft .+^ displacement
  where
    displacement = invertVecY $ subtract 1 <$> V2 w h

-- | Converts the displacement vector between the two
-- diagonal corners of the rectangle into an "AreaDimensions" record.
-- Adds one to both dimensions since the corner coordinates are "inclusive".
cornersToArea :: Location -> Location -> AreaDimensions
cornersToArea upperLeft lowerRight =
  AreaDimensions x y
 where
  V2 x y = (+1) <$> invertVecY (lowerRight .-. upperLeft)

isEmpty :: AreaDimensions -> Bool
isEmpty (AreaDimensions w h) = w == 0 || h == 0

getAreaDimensions :: [[a]] -> AreaDimensions
getAreaDimensions cellGrid =
  AreaDimensions w h
 where
  w = fromIntegral $ maybe 0 length $ listToMaybe cellGrid -- column count
  h = fromIntegral $ length cellGrid -- row count