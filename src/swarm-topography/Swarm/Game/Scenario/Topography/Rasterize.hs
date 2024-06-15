-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Convenience functions for rendering topographical structures
module Swarm.Game.Scenario.Topography.Rasterize where

import Codec.Picture
import Data.Vector qualified as V
import Swarm.Game.Scenario.Topography.Area
import Data.List.NonEmpty qualified as NE

-- | Converts linked lists to vectors to facilitate
-- random access when assembling the image
gridToVec :: Grid a -> V.Vector (V.Vector a)
gridToVec (Grid g) = V.fromList . map V.fromList $ NE.toList g

makeImage :: Pixel px => (a -> px) -> Grid a -> Image px
makeImage computeColor g =
  generateImage (pixelRenderer vecGrid) (fromIntegral w) (fromIntegral h)
 where
  vecGrid = gridToVec g
  AreaDimensions w h = getGridDimensions g
  pixelRenderer vg x y = computeColor $ (vg V.! y) V.! x
