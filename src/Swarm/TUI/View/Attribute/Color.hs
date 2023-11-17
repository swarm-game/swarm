-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Preserve color fidelity for non-TUI rendering
module Swarm.TUI.View.Attribute.Color where

import Data.Colour.SRGB (RGB)
import Data.Word (Word8)
import Graphics.Vty qualified as V

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
  | FgAndBg RGBColor RGBColor

-- | Includes both a VTY-specific style specification and a
-- high-fidelity color specification for rendering to other mediums.
--
-- Preferably, in most cases a single source of truth serves to specify
-- a color upstream, and the other of the two specifications is derived,
-- using the utility functions in this module.
data EntityStyle = EntityStyle
  { vtyColor :: V.Attr
  -- ^ Includes foreground, background, and styling
  , niceColor :: HiFiColor
  }
