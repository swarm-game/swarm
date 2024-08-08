-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Convenience functions for rendering topographical structures
module Swarm.Game.Scenario.Topography.Rasterize where

import Codec.Picture
import Data.Vector qualified as V
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Grid

class ToPixel a where
  toPixel :: a -> PixelRGBA8

instance ToPixel a => ToPixel (Maybe a) where
  toPixel = maybe transparent toPixel
   where
    transparent = PixelRGBA8 0 0 0 0

makeImage :: ToPixel a => Grid a -> Image PixelRGBA8
makeImage g =
  generateImage (pixelRenderer vecGrid) (fromIntegral w) (fromIntegral h)
 where
  vecGrid = gridToVec g
  AreaDimensions w h = getGridDimensions g
  pixelRenderer vg x y = toPixel $ (vg V.! y) V.! x
