-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Entity and terrain coloring.
module Swarm.Game.Entity.Cosmetic.Specimen where

import Data.Bifunctor (bimap)
import Data.Colour.SRGB (RGB (..))
import Data.Map (Map)
import Data.Map qualified as M
import Swarm.Game.Entity.Cosmetic

-- * Named colors

whiteRGB :: RGBColor
whiteRGB = RGB 208 207 204

brightRedRGB :: RGBColor
brightRedRGB = RGB 246 97 81

redRGB :: RGBColor
redRGB = RGB 192 28 40

greenRGB :: RGBColor
greenRGB = RGB 38 162 105

blueRGB :: RGBColor
blueRGB = RGB 18 72 139

brightYellowRGB :: RGBColor
brightYellowRGB = RGB 233 173 12

yellowRGB :: RGBColor
yellowRGB = RGB 162 115 76

-- * Entities

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
    -- these four are referenced elsewhere,
    -- so they have their own toplevel definition
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
        , ("fire", brightRedRGB)
        , ("red", redRGB)
        , ("green", greenRGB)
        , ("blue", blueRGB)
        ]

-- * Terrain

dirt :: (TerrainAttr, HiFiColor)
dirt = (TerrainAttr "dirt", FgOnly $ RGB 165 42 42)

grass :: (TerrainAttr, HiFiColor)
grass = (TerrainAttr "grass", FgOnly $ RGB 0 32 0) -- dark green

stone :: (TerrainAttr, HiFiColor)
stone = (TerrainAttr "stone", FgOnly $ RGB 32 32 32)

ice :: (TerrainAttr, HiFiColor)
ice = (TerrainAttr "ice", BgOnly whiteRGB)

terrainAttributes :: M.Map TerrainAttr HiFiColor
terrainAttributes =
  M.fromList
    [ dirt
    , grass
    , stone
    , ice
    ]
