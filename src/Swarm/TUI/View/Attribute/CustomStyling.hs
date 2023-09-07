-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.Attribute.CustomStyling where

import Brick (AttrName, attrName)
import Data.Colour.SRGB (sRGB24read)
import Data.Set (toList)
import Data.Text qualified as T
import Graphics.Vty.Attributes
import Swarm.Game.Scenario.Style
import Swarm.TUI.View.Attribute.Attr (worldPrefix)
import Swarm.TUI.View.Attribute.Util

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

hexToAttrColor :: HexColor -> Color
hexToAttrColor (HexColor colorText) =
  kolorToAttrColor c
 where
  c = sRGB24read $ T.unpack colorText

toAttrPair :: CustomAttr -> (AttrName, Attr)
toAttrPair ca =
  (worldPrefix <> attrName (name ca), addStyle $ addFg $ addBg defAttr)
 where
  addFg = maybe id (flip withForeColor . hexToAttrColor) $ fg ca
  addBg = maybe id (flip withBackColor . hexToAttrColor) $ bg ca
  addStyle = maybe id (flip withStyle . sum . map toStyle . toList) $ style ca
