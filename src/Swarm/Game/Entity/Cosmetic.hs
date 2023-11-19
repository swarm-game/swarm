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

type RGBColor = RGB Word8

data TrueColor
  = AnsiColor NamedColor
  | Triple RGBColor
  deriving (Show)

-- | High-fidelity color representation, for rendering
-- outside of the TUI.
-- Ignores vty "styles", such as bold/italic/underline.
--
-- This is intended to facilitate multiple rendering mediums:
--
-- * Single pixel per world cell (one color must be chosen between foreground and background, if both are specified)
-- * Pixel block per world cell (can show two colors in some stylized manner)
-- * Glyph per world cell (can render a colored display character on a colored background)
data HiFiColor a
  = FgOnly a
  | BgOnly a
  | FgAndBg
      -- | foreground
      a
      -- | background
      a
  deriving (Show, Functor)

type PreservableColor = HiFiColor TrueColor

newtype WorldAttr = WorldAttr String
  deriving (Eq, Ord, Show)

newtype TerrainAttr = TerrainAttr String
  deriving (Eq, Ord, Show)
