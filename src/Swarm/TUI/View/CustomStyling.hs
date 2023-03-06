-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.CustomStyling where

import Brick (AttrName, attrName)
import Data.Set (toList)
import Data.Text qualified as T
import Graphics.Vty.Attributes
import Numeric (readHex)
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
  case nums of
    [r, g, b] -> RGBColor r g b
    _ -> RGBColor 255 255 255
 where
  chunks = T.chunksOf 2 $ T.dropWhile (== '#') colorText
  nums = map (fst . head . readHex . T.unpack) chunks

toAttrPair :: CustomAttr -> (AttrName, Attr)
toAttrPair ca =
  (worldPrefix <> attrName (name ca), addStyle $ addFg $ addBg defAttr)
 where
  addFg = maybe id (flip withForeColor . toAttrColor) $ fg ca
  addBg = maybe id (flip withBackColor . toAttrColor) $ bg ca
  addStyle = maybe id (flip withStyle . sum . map toStyle . toList) $ style ca
