-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Preserve color fidelity for non-TUI rendering
module Swarm.Game.Entity.Cosmetic where

import Data.Colour.SRGB (RGB)
import Data.Word (Word8)

type RGBColor = RGB Word8

-- | High-fidelity color representation, for rendering
-- outside of the TUI.
-- Ignores vty "styles", such as bold/italic/underline.
--
-- This is intended to facilitate multiple rendering mediums:
--
-- * Single pixel per world cell (one color must be chosen between foreground and background, if both are specified)
-- * Pixel block per world cell (can show two colors in some stylized manner)
-- * Glyph per world cell (can render a colored display character on a colored background)
data HiFiColor
  = FgOnly RGBColor
  | BgOnly RGBColor
  | FgAndBg
      -- | foreground
      RGBColor
      -- | background
      RGBColor
  deriving (Show)

newtype WorldAttr = WorldAttr
  { attrSuffix :: String
  }
  deriving (Eq, Ord, Show)

newtype TerrainAttr = TerrainAttr
  { terrainAttrSuffix :: String
  }
  deriving (Eq, Ord, Show)
