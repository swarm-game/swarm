-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Render.Image (
  TransparencyHandling (..),
  ImgRendering (..),
  mkStructureImage,
  mkStructurePng,
  defaultImageRendering,
) where

import Codec.Picture
import Data.ByteString.Lazy qualified as LBS
import Data.Either.Utils (forceEither)
import Data.Function (on)
import Data.Map (Map)
import Swarm.Game.Scenario.Topography.Navigation.Waypoint (
  Parentage (Root),
 )
import Swarm.Game.Scenario.Topography.Rasterize
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Scenario.Topography.Structure.Assembly
import Swarm.Game.Scenario.Topography.Structure.Named (NamedArea, StructureName)
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Swarm.Util (applyWhen)

data TransparencyHandling
  = Transparent
  | DiagonalIndicators

data ImgRendering = ImgRendering
  { scaleNum :: Int
  , transparencyHandling :: TransparencyHandling
  }

defaultImageRendering :: ImgRendering
defaultImageRendering = ImgRendering 1 Transparent

mkStructureImage ::
  ToPixel a =>
  ImgRendering ->
  Map StructureName (NamedArea (PStructure (Maybe a))) ->
  PStructure (Maybe a) ->
  Image PixelRGBA8
mkStructureImage (ImgRendering scaleFactor transparencyMode) sMap parentStruct =
  imgPipeline . makeImage $ gridContent overlayArea
 where
  imgPipeline = illustrateTransparency transparencyMode . scalingFunc
  scalingFunc = scaleWithPixelBorders (PixelRGBA8 minBound minBound minBound maxBound) scaleFactor
  overlayArea = forceMerge sMap parentStruct

mkStructurePng ::
  ToPixel a =>
  ImgRendering ->
  Map StructureName (NamedArea (PStructure (Maybe a))) ->
  PStructure (Maybe a) ->
  LBS.ByteString
mkStructurePng r sMap parentStruct =
  encodePng $ mkStructureImage r sMap parentStruct

illustrateTransparency ::
  TransparencyHandling ->
  Image PixelRGBA8 ->
  Image PixelRGBA8
illustrateTransparency mode img@(Image w h _) = case mode of
  Transparent -> img
  DiagonalIndicators -> mkNewImage img
 where
  mkNewImage s = generateImage (f s) w h
  f s x y =
    if pixelOpacity px == 0
      then checkerColor
      else px
   where
    px = pixelAt s x y
    checkerOpacity =
      if even $ (x `div` 2) + (y `div` 2)
        then maxBound `div` 4
        else maxBound `div` 2
    checkerColor = PixelRGBA8 (gradientPixel x w) 128 128 checkerOpacity
    gradientPixel i d = fromIntegral $ (i * 255) `div` d

-- | Integral-factor scaling by nearest neighbor.
-- Preserves sharp definition for pixel art.

{-
scaleImage :: Pixel a => Int -> Image a -> Image a
scaleImage scaleFactor =
  applyWhen (scaleFactor > 1) mkNewImage
 where
  mkNewImage s@(Image w h _) = (generateImage (f s) `on` (* scaleFactor)) w h
  f s = pixelAt s `on` (`div` scaleFactor)
-}

-- | Integral-factor scaling by nearest neighbor.
-- Preserves sharp definition for pixel art.
--
-- Inserts a black border between pixels.
scaleWithPixelBorders :: Pixel a => a -> Int -> Image a -> Image a
scaleWithPixelBorders substitutionColor rawScaleFactor =
  applyWhen (rawScaleFactor > 1) mkNewImage
 where
  scaleFactor = rawScaleFactor + 1
  -- We add a final +1 to the image dimensions after scaling so that
  -- the border is drawn on all four sides of the image, rather than just the top and left.
  mkNewImage s@(Image w h _) = (generateImage (f s) `on` augmentDimension) w h
  augmentDimension d = d * scaleFactor + 1
  f s x y =
    if x `mod` scaleFactor == 0 || y `mod` scaleFactor == 0
      then substitutionColor
      else (pixelAt s `on` (`div` scaleFactor)) x y

forceMerge ::
  Map StructureName (NamedArea (PStructure (Maybe a))) ->
  PStructure (Maybe a) ->
  PositionedGrid (Maybe a)
forceMerge sMap parentStruct =
  overlayArea
 where
  MergedStructure overlayArea _ _ = forceEither $ mergeStructures sMap Root parentStruct
