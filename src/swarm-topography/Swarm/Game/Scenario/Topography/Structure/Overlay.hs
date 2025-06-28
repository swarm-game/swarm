{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Generic overlay operations on grids
module Swarm.Game.Scenario.Topography.Structure.Overlay (
  PositionedGrid (..),

  -- * Exported for unit tests
  computeMergedArea,
  OverlayPair (..),
) where

import Control.Applicative
import Control.Lens (view)
import Data.Function (on)
import Data.Int (Int32)
import Data.Tuple (swap)
import Linear.V2 (R1 (_x), R2 (_y), V2 (..))
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Util (applyWhen)

data PositionedGrid a = PositionedGrid
  { gridPosition :: Location
  -- ^ location of the upper-left cell
  , gridContent :: Grid a
  }
  deriving (Eq, Functor, Foldable, Traversable)

instance HasLocation (PositionedGrid a) where
  modifyLoc f (PositionedGrid originalLoc g) =
    PositionedGrid (f originalLoc) g

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

getNorthwesternExtent :: Location -> Location -> Location
getNorthwesternExtent ul1 ul2 =
  Location westernMostX northernMostY
 where
  westernMostX = (min `on` view _x) ul1 ul2
  northernMostY = (max `on` view _y) ul1 ul2

getSoutheasternExtent :: Location -> Location -> Location
getSoutheasternExtent br1 br2 =
  Location easternMostX southernMostY
 where
  easternMostX = (max `on` view _x) br1 br2
  southernMostY = (min `on` view _y) br1 br2

-- | @r1 <> r2@ is the smallest rectangle that contains both @r1@ and @r2@.
instance Semigroup SubsumingRect where
  SubsumingRect ul1 br1 <> SubsumingRect ul2 br2 =
    SubsumingRect northwesternExtent southeasternExtent
   where
    northwesternExtent = getNorthwesternExtent ul1 ul2
    southeasternExtent = getSoutheasternExtent br1 br2

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
  -- Right-biased; that is, takes the last non-empty value
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
    PositionedGrid newUpperLeftCornerPosition combinedGrid
   where
    mergedSize = computeMergedArea $ OverlayPair a1 a2
    combinedGrid = zipGridRows mergedSize paddedOverlayPair

    -- We create a vector from the overlay position,
    -- such that the displacement vector will have:
    -- \* negative X component if the origin must be shifted east
    -- \* positive Y component if the origin must be shifted south
    upperLeftCornersDelta = overlayLoc .-. baseLoc

    newUpperLeftCornerPosition = getNorthwesternExtent baseLoc overlayLoc

    paddedOverlayPair =
      padNorthwest upperLeftCornersDelta $
        OverlayPair baseGrid overlayGrid

-- |
-- 'deltaX' and 'deltaY' refer to the positioning of the *overlay grid*
-- relative to the *base grid*.
-- A negative 'deltaY' means that the top edge of the overlay
-- lies to the south of the top edge of the base grid.
-- A positive 'deltaX' means that the left edge of the overlay
-- lies to the east of the left edge of base grid.
--
-- We add padding to either the overlay grid or the base grid
-- so as to align their upper-left corners.
--
-- NOTE: We only make explicit grid adjustments for
-- left/top padding.  Any padding that is needed on the right/bottom
-- of either grid will be taken care of by the 'zipPadded' function.
--
-- TODO(#2004): The return type should be 'Grid'.
padNorthwest ::
  Alternative f =>
  V2 Int32 ->
  OverlayPair (Grid (f a)) ->
  OverlayPair [[f a]]
padNorthwest (V2 deltaX deltaY) (OverlayPair baseGrid overlayGrid) =
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
