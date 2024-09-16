{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Generic overlay operations on grids
module Swarm.Game.Scenario.Topography.Structure.Overlay (
  PositionedGrid (..),
) where

import Control.Applicative
import Debug.Trace
import Data.Function (on)
import Data.Int (Int32)
import Data.Tuple (swap)
import Linear hiding (trace)
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Util (applyWhen)

data PositionedGrid a = PositionedGrid
  { gridPosition :: Location
  -- ^ location of the upper-left cell
  , gridContent :: Grid a
  }
  deriving (Eq)

instance Show (PositionedGrid a) where
  show (PositionedGrid p g) =
    unwords
      [ "Grid with dimension"
      , renderRectDimensions $ getGridDimensions g
      , "located at"
      , show p
      ]

data OverlayPair a = OverlayPair
  { _base :: a
  , _overlay :: a
  }

-- | Has a 'Semigroup' instance to determine the smallest
-- bounds that enclose two rectangles
data SubsumingRect = SubsumingRect
  { _northwestCorner :: Location
  , _southeastCorner :: Location
  }

-- | @r1 <> r2@ is the smallest rectangle that contains both @r1@ and @r2@.
instance Semigroup SubsumingRect where
  SubsumingRect (Location ulx1 uly1) (Location brx1 bry1)
    <> SubsumingRect (Location ulx2 uly2) (Location brx2 bry2) =
      SubsumingRect (Location westernMostX northernMostY) (Location easternMostX southernMostY)
     where
      westernMostX = min ulx1 ulx2
      northernMostY = max uly1 uly2
      easternMostX = max brx1 brx2
      southernMostY = min bry1 bry2

getSubsumingRect :: PositionedGrid a -> SubsumingRect
getSubsumingRect (PositionedGrid loc g) =
  SubsumingRect loc $ computeBottomRightFromUpperLeft (getGridDimensions g) loc

computeMergedArea :: OverlayPair (PositionedGrid a) -> AreaDimensions
computeMergedArea (OverlayPair pg1 pg2) =
  cornersToArea ul br
 where
  SubsumingRect ul br = ((<>) `on` getSubsumingRect) pg1 pg2

zipGridRows ::
  Alternative f =>
  AreaDimensions ->
  OverlayPair [[f a]] ->
  Grid (f a)
zipGridRows dims (OverlayPair paddedBaseRows paddedOverlayRows) =
  mkGrid $ (pad2D paddedBaseRows . pad2D paddedOverlayRows) blankGrid
 where
  -- Right-bias; that is, take the last non-empty value
  pad2D = zipPadded $ zipPadded $ flip (<|>)
  blankGrid = getRows $ fillGrid dims empty

-- |
-- First arg: base layer
-- Second arg: overlay layer
--
-- The upper-left corner of the base layer is the original "origin".
--
-- If the overlay is to the west or north of the base layer,
-- then we must pad the base layer on the left or top.
-- And since the area expands relative to the "origin" of the
-- base layer, we must shift the combined grid's "origin" location
-- to the new position of the base layer's upper-left corner.
--
-- If the overlay is to the east/south, we do not have to
-- modify the origin, since no padding is added to the left/top
-- of the base layer.
instance (Alternative f) => Semigroup (PositionedGrid (f a)) where
  a1@(PositionedGrid baseLoc baseGrid) <> a2@(PositionedGrid overlayLoc overlayGrid) =
    PositionedGrid newOrigin combinedGrid
   where
    mergedSize = computeMergedArea $ OverlayPair a1 a2
    combinedGrid = zipGridRows mergedSize paddedOverlayPair

    -- We subtract the base origin from the
    -- overlay position, such that the displacement vector
    -- will have:
    -- \* negative X component if the origin must be shifted east
    -- \* positive Y component if the origin must be shifted south
    originDelta@(V2 deltaX deltaY) = overlayLoc .-. baseLoc
    -- Note that the adjustment vector will only ever have
    -- a non-negative X component (i.e. loc of upper-left corner must be shifted east) and
    -- a non-positive Y component (i.e. loc of upper-left corner must be shifted south).
    -- We don't have to adjust the origin if the base layer lies
    -- to the northwest of the overlay layer.
    clampedDelta = V2 (min 0 deltaX) (max 0 deltaY)
    newOrigin2 = baseLoc .-^ clampedDelta

    newOrigin = trace (unwords [
        "COMBINING baseLoc"
      , show baseLoc
      , "with overlayLoc"
      , show overlayLoc
      , "to get"
      , show newOrigin2
      ]) newOrigin2

    paddedOverlayPair =
      padSouthwest (overlayLoc .-. origin) $
        OverlayPair baseGrid overlayGrid

-- | NOTE: We only make explicit grid adjustments for
-- left/top padding.  Any padding that is needed on the right/bottom
-- of either grid will be taken care of by the 'zipPadded' function.
--
-- TODO(#2004): The return type should be 'Grid'.
padSouthwest ::
  Alternative f =>
  V2 Int32 ->
  OverlayPair (Grid (f a)) ->
  OverlayPair [[f a]]
padSouthwest (V2 deltaX deltaY) (OverlayPair baseGrid overlayGrid) =
  OverlayPair paddedBaseGrid paddedOverlayGrid
 where
  prefixPadDimension delta f = f (padding <>)
   where
    padding = replicate (abs $ fromIntegral delta) empty

  prefixPadRows = prefixPadDimension deltaY id
  prefixPadColumns = prefixPadDimension deltaX map

  -- Assume only the *overlay* requires vertical (top-)padding.
  -- However, if the conditional is true, then
  -- the *base* needs vertical padding instead.
  (baseVerticalPadFunc, overlayVerticalPadFunc) =
    applyWhen (deltaY > 0) swap (id, prefixPadRows)

  -- Assume only the *overlay* requires horizontal (left-)padding.
  -- However, if the conditional is true, then
  -- the *base* needs horizontal padding instead.
  (baseHorizontalPadFunc, overlayHorizontalPadFunc) =
    applyWhen (deltaX < 0) swap (id, prefixPadColumns)

  paddedBaseGrid = baseVerticalPadFunc $ baseHorizontalPadFunc $ getRows baseGrid
  paddedOverlayGrid = overlayVerticalPadFunc $ overlayHorizontalPadFunc $ getRows overlayGrid

-- * Utils

-- | Apply a function to combine elements from two lists
-- of potentially different lengths.
-- Produces a result with length equal to the longer list.
-- Elements from the longer list are placed directly in the
-- resulting list when the shorter list runs out of elements.
zipPadded :: (a -> a -> a) -> [a] -> [a] -> [a]
zipPadded _ [] ys = ys
zipPadded _ xs [] = xs
zipPadded f (x : xs) (y : ys) = f x y : zipPadded f xs ys
