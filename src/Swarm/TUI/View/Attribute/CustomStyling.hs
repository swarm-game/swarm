-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.Attribute.CustomStyling where

import Data.Colour.SRGB (sRGB24read)
import Data.Set (toList)
import Data.Text qualified as T
import Graphics.Vty.Attributes
import Swarm.Game.Scenario.Style
import Swarm.TUI.View.Attribute.Util
import Swarm.Game.Entity.Cosmetic (WorldAttr (..))

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

toAttrPair :: CustomAttr -> (WorldAttr, Attr)
toAttrPair ca =
  (WorldAttr (name ca), addStyle $ addFg $ addBg defAttr)
 where
  addFg = maybe id (flip withForeColor . hexToAttrColor) $ fg ca
  addBg = maybe id (flip withBackColor . hexToAttrColor) $ bg ca
  addStyle = maybe id (flip withStyle . sum . map toStyle . toList) $ style ca
