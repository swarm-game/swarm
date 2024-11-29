{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Area where

import Data.Aeson (ToJSON)
import Data.Function (on)
import Data.Int (Int32)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Maybe (listToMaybe)
import Data.Semigroup
import GHC.Generics (Generic)
import Linear (V2 (..))
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Grid
import Prelude hiding (zipWith)

-- | Height and width of a 2D map region
data AreaDimensions = AreaDimensions
  { rectWidth :: Int32
  , rectHeight :: Int32
  }
  deriving (Show, Eq, Generic, ToJSON)

getGridDimensions :: Grid a -> AreaDimensions
getGridDimensions g = getAreaDimensions $ getRows g

getNEGridDimensions :: NonEmptyGrid a -> AreaDimensions
getNEGridDimensions (NonEmptyGrid xs) =
  (AreaDimensions `on` fromIntegral)
    (NE.length firstRow)
    (NE.length xs)
 where
  firstRow = NE.head xs

asTuple :: AreaDimensions -> (Int32, Int32)
asTuple (AreaDimensions x y) = (x, y)

renderRectDimensions :: AreaDimensions -> String
renderRectDimensions (AreaDimensions w h) =
  L.intercalate "x" $ map show [w, h]

invertY :: V2 Int32 -> V2 Int32
invertY (V2 x y) = V2 x (-y)

-- | Incorporates an offset by @-1@, since the area is
-- "inclusive" of the lower-right coordinate.
-- Inverse of 'cornersToArea'.
computeBottomRightFromUpperLeft :: AreaDimensions -> Location -> Location
computeBottomRightFromUpperLeft a upperLeft =
  upperLeft .+^ displacement
 where
  displacement = invertY $ computeAbsoluteCornerDisplacement a

computeAbsoluteCornerDisplacement :: AreaDimensions -> V2 Int32
computeAbsoluteCornerDisplacement (AreaDimensions w h) =
  subtract 1 <$> V2 w h

-- | Converts the displacement vector between the two
-- diagonal corners of the rectangle into an 'AreaDimensions' record.
-- Adds one to both dimensions since the corner coordinates are "inclusive".
-- Inverse of 'computeBottomRightFromUpperLeft'.
cornersToArea :: Location -> Location -> AreaDimensions
cornersToArea upperLeft bottomRight =
  AreaDimensions x y
 where
  V2 x y = (+ 1) <$> invertY (bottomRight .-. upperLeft)

-- | Has zero width or height.
isEmpty :: AreaDimensions -> Bool
isEmpty (AreaDimensions w h) = w == 0 || h == 0

-- | Extracts the dimensions of a map grid.
getAreaDimensions :: [[a]] -> AreaDimensions
getAreaDimensions cellGrid =
  AreaDimensions w h
 where
  w = fromIntegral $ maybe 0 length $ listToMaybe cellGrid -- column count
  h = fromIntegral $ length cellGrid -- row count

computeArea :: AreaDimensions -> Int32
computeArea (AreaDimensions w h) = w * h

fillGrid :: AreaDimensions -> a -> Grid a
fillGrid (AreaDimensions 0 _) _ = EmptyGrid
fillGrid (AreaDimensions _ 0) _ = EmptyGrid
fillGrid (AreaDimensions w h) x =
  Grid
    . NonEmptyGrid
    . stimes h
    . pure
    . stimes w
    . pure
    $ x
