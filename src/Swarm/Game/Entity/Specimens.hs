-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Preserve color fidelity for non-TUI rendering
module Swarm.Game.Entity.Specimens where

import Data.Bifunctor (bimap)
import Data.Colour.SRGB (RGB (..))
import Data.Map (Map)
import Data.Map qualified as M
import Swarm.Game.Entity.Cosmetic

entity :: (WorldAttr, HiFiColor)
entity = (WorldAttr "entity", FgOnly whiteRGB)

water :: (WorldAttr, HiFiColor)
water = (WorldAttr "water", FgAndBg whiteRGB blueRGB)

rock :: (WorldAttr, HiFiColor)
rock = (WorldAttr "rock", FgOnly $ RGB 80 80 80)

plant :: (WorldAttr, HiFiColor)
plant = (WorldAttr "plant", FgOnly greenRGB)

-- | Colors of entities in the world.
worldAttributes :: Map WorldAttr HiFiColor
worldAttributes =
  M.fromList $
    [entity, water, rock, plant]
      <> map
        (bimap WorldAttr FgOnly)
        [ ("device", brightYellowRGB)
        , ("wood", RGB 139 69 19)
        , ("flower", RGB 200 0 200)
        , ("rubber", RGB 245 224 179)
        , ("copper", yellowRGB)
        , ("copper'", RGB 78 117 102)
        , ("iron", RGB 97 102 106)
        , ("iron'", RGB 183 65 14)
        , ("quartz", whiteRGB)
        , ("silver", RGB 192 192 192)
        , ("gold", RGB 255 215 0)
        , ("snow", whiteRGB)
        , ("sand", RGB 194 178 128)
        , ("fire", RGB 246 97 81)
        , ("red", RGB 192 28 40)
        , ("green", greenRGB)
        , ("blue", blueRGB)
        ]

-- * Named colors

whiteRGB :: RGBColor
whiteRGB = RGB 208 207 204

blueRGB :: RGBColor
blueRGB = RGB 42 123 222

greenRGB :: RGBColor
greenRGB = RGB 38 162 105

brightYellowRGB :: RGBColor
brightYellowRGB = RGB 233 173 12

yellowRGB :: RGBColor
yellowRGB = RGB 162 115 76