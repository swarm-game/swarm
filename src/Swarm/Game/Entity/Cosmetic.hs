-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Preserve color fidelity for non-TUI rendering
module Swarm.Game.Entity.Cosmetic where

import Data.Colour.SRGB (RGB)
import Data.Word (Word8)

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

getBackground :: ColorLayers a -> Maybe a
getBackground = \case
  FgOnly _ -> Nothing
  BgOnly x -> Just x
  FgAndBg _ x -> Just x

newtype WorldAttr = WorldAttr String
  deriving (Eq, Ord, Show)

newtype TerrainAttr = TerrainAttr String
  deriving (Eq, Ord, Show)
