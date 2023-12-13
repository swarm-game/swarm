-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Entity and terrain coloring.
--
-- This module is the sole "ground truth" for color
-- assignment of entities and terrain.
-- More specifically, it sets colors for "attributes",
-- and the attributes are referenced by entities\/terrain.
module Swarm.Game.Entity.Cosmetic.Assignment where

import Data.Bifunctor (bimap)
import Data.Colour.SRGB (RGB (..))
import Data.Map (Map)
import Data.Map qualified as M
import Swarm.Game.Entity.Cosmetic

-- * Entities

entity :: (WorldAttr, PreservableColor)
entity = (WorldAttr "entity", FgOnly $ AnsiColor White)

water :: (WorldAttr, PreservableColor)
water = (WorldAttr "water", FgAndBg (AnsiColor White) (AnsiColor Blue))

rock :: (WorldAttr, PreservableColor)
rock = (WorldAttr "rock", FgOnly $ Triple $ RGB 80 80 80)

plant :: (WorldAttr, PreservableColor)
plant = (WorldAttr "plant", FgOnly $ AnsiColor Green)

-- | Colors of entities in the world.
worldAttributes :: Map WorldAttr PreservableColor
worldAttributes =
  M.fromList $
    -- these four are referenced elsewhere,
    -- so they have their own toplevel definition
    [entity, water, rock, plant]
      <> map
        (bimap WorldAttr FgOnly)
        [ ("device", AnsiColor BrightYellow)
        , ("wood", Triple $ RGB 139 69 19)
        , ("flower", Triple $ RGB 200 0 200)
        , ("rubber", Triple $ RGB 245 224 179)
        , ("copper", AnsiColor Yellow)
        , ("copper'", Triple $ RGB 78 117 102)
        , ("iron", Triple $ RGB 97 102 106)
        , ("iron'", Triple $ RGB 183 65 14)
        , ("quartz", AnsiColor White)
        , ("silver", Triple $ RGB 192 192 192)
        , ("gold", Triple $ RGB 255 215 0)
        , ("snow", AnsiColor White)
        , ("sand", Triple $ RGB 194 178 128)
        , ("fire", AnsiColor BrightRed)
        , ("red", AnsiColor Red)
        , ("green", AnsiColor Green)
        , ("blue", AnsiColor Blue)
        ]

-- * Terrain

dirt :: (TerrainAttr, PreservableColor)
dirt = (TerrainAttr "dirt", BgOnly $ Triple $ RGB 87 47 47)

grass :: (TerrainAttr, PreservableColor)
grass = (TerrainAttr "grass", BgOnly $ Triple $ RGB 0 47 0) -- dark green

stone :: (TerrainAttr, PreservableColor)
stone = (TerrainAttr "stone", BgOnly $ Triple $ RGB 47 47 47)

ice :: (TerrainAttr, PreservableColor)
ice = (TerrainAttr "ice", BgOnly $ AnsiColor White)

terrainAttributes :: M.Map TerrainAttr PreservableColor
terrainAttributes =
  M.fromList
    [ dirt
    , grass
    , stone
    , ice
    ]
