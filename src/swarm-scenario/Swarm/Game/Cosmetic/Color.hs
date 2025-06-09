-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Preserve color fidelity for non-TUI rendering.
module Swarm.Game.Cosmetic.Color where

import Codec.Picture (PixelRGBA8 (..))
import Data.Colour.SRGB (RGB (..))
import Data.Map (Map)
import Data.Word (Word8)
import Swarm.Game.Cosmetic.Attribute (Attribute)
import Swarm.Game.Scenario.Topography.Rasterize

data NamedColor
  = White
  | BrightRed
  | Red
  | Green
  | Blue
  | BrightYellow
  | Yellow
  deriving (Show)

-- | 8-bit color
type RGBColor = RGB Word8

fromHiFi :: PreservableColor -> ColorLayers RGBColor
fromHiFi = fmap $ \case
  Triple x -> x
  -- The triples we've manually assigned for named
  -- ANSI colors do not need to be round-tripped, since
  -- those triples are not inputs to the VTY attribute creation.
  AnsiColor x -> namedToTriple x

-- | Since terminals can customize these named
-- colors using themes or explicit user overrides,
-- these color assignments are somewhat arbitrary.
namedToTriple :: NamedColor -> RGBColor
namedToTriple = \case
  White -> RGB 208 207 204
  BrightRed -> RGB 246 97 81
  Red -> RGB 192 28 40
  Green -> RGB 38 162 105
  Blue -> RGB 18 72 139
  BrightYellow -> RGB 233 173 12
  Yellow -> RGB 162 115 76

-- | High-fidelity color representation for rendering
-- outside of the TUI.
data TrueColor
  = AnsiColor NamedColor
  | Triple RGBColor
  deriving (Show)

-- |
-- A value of type @ColorLayers a@ represents the assignment of
-- foreground and\/or background color to an 'Entity' or terrain,
-- where @a@ may be a medium-independent (i.e. "authoritative") color
-- representation, or medium-specific (e.g. a @vty@ color).
-- The 'Functor' instance facilitates easy conversion from the
-- authoritative color to the specialized representation.
--
-- Ignores @vty@ "styles", such as bold\/italic\/underline.
--
-- This is intended to facilitate multiple rendering mediums:
--
-- * Single pixel per world cell (one color must be chosen
--   between foreground and background, if both are specified)
-- * Pixel block per world cell (can show two colors in some stylized manner)
-- * Glyph per world cell (can render a colored display character on a colored background)
data ColorLayers a
  = FgOnly a
  | BgOnly a
  | FgAndBg
      -- | foreground
      a
      -- | background
      a
  deriving (Show, Functor)

type PreservableColor = ColorLayers TrueColor

type AttributeMap = Map Attribute PreservableColor

instance ToPixel PreservableColor where
  toPixel h = PixelRGBA8 r g b 255
   where
    RGB r g b = flattenBg $ fromHiFi h

mkColorLayers :: Maybe a -> Maybe a -> Maybe (ColorLayers a)
mkColorLayers Nothing Nothing = Nothing
mkColorLayers (Just fg) Nothing = Just $ FgOnly fg
mkColorLayers Nothing (Just bg) = Just $ BgOnly bg
mkColorLayers (Just fg) (Just bg) = Just $ FgAndBg fg bg

flattenBg :: ColorLayers a -> a
flattenBg = \case
  FgOnly x -> x
  BgOnly x -> x
  FgAndBg _ x -> x
