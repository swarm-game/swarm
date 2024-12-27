-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.Attribute.CustomStyling where

import Data.Set (toList)
import Graphics.Vty.Attributes
import Swarm.Game.Entity.Cosmetic (WorldAttr (..))
import Swarm.Game.Scenario.Style
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
hexToAttrColor (HexColor kolor) = kolorToAttrColor kolor

toAttrPair :: CustomAttr -> (WorldAttr, Attr)
toAttrPair ca =
  (WorldAttr (name ca), addStyle $ addFg $ addBg defAttr)
 where
  addFg = maybe id (flip withForeColor . hexToAttrColor) $ fg ca
  addBg = maybe id (flip withBackColor . hexToAttrColor) $ bg ca
  addStyle = maybe id (flip withStyle . sum . map toStyle . toList) $ style ca
