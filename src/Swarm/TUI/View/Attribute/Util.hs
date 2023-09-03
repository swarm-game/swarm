-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.Attribute.Util where

import Brick.Util (on)
import Data.Colour.CIE (luminance)
import Data.Colour.Palette.BrewerSet (Kolor)
import Data.Colour.SRGB (RGB (..), toSRGB24)
import Graphics.Vty qualified as V
import Graphics.Vty.Attributes

kolorToAttrColor :: Kolor -> Color
kolorToAttrColor c =
  RGBColor r g b
 where
  RGB r g b = toSRGB24 c

-- | Automatically selects black or white for the foreground
-- based on the darkness of the supplied background.
bgWithAutoForeground :: Kolor -> Attr
bgWithAutoForeground c = fgColor `on` kolorToAttrColor c
 where
  fgColor =
    if luminance c > 0.5
      then V.black
      else V.white
