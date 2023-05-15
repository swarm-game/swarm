-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.CustomStyling where

import Brick (AttrName, attrName)
import Data.Colour.SRGB (Colour, RGB (..), sRGB24read, toSRGB24)
import Data.Set (toList)
import Data.Text qualified as T
import Graphics.Vty.Attributes
import Swarm.Game.Scenario.Style
import Swarm.TUI.Attr (worldPrefix)

toStyle :: StyleFlag -> Style
toStyle = \case
  Standout -> standout
  Italic -> italic
  Strikethrough -> strikethrough
  Underline -> underline
  ReverseVideo -> reverseVideo
  Blink -> blink
  Dim -> dim
  Bold -> bold

toAttrColor :: HexColor -> Color
toAttrColor (HexColor colorText) =
  RGBColor r g b
 where
  RGB r g b = toSRGB24 c
  c :: Colour Double
  c = sRGB24read $ T.unpack colorText

toAttrPair :: CustomAttr -> (AttrName, Attr)
toAttrPair ca =
  (worldPrefix <> attrName (name ca), addStyle $ addFg $ addBg defAttr)
 where
  addFg = maybe id (flip withForeColor . toAttrColor) $ fg ca
  addBg = maybe id (flip withBackColor . toAttrColor) $ bg ca
  addStyle = maybe id (flip withStyle . sum . map toStyle . toList) $ style ca
