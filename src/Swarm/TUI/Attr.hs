-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Swarm.TUI.Attr
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Rendering attributes (/i.e./ foreground and background colors,
-- styles, /etc./) used by the Swarm TUI.
module Swarm.TUI.Attr where

import Brick
import Brick.Forms
import Brick.Widgets.Dialog
import Brick.Widgets.List
import qualified Graphics.Vty as V

import Data.Yaml
import Witch (from)

-- | A mapping from the defined attribute names to TUI attributes.
swarmAttrMap :: AttrMap
swarmAttrMap =
  attrMap
    V.defAttr
    -- World rendering attributes
    [ (robotAttr, fg V.white `V.withStyle` V.bold)
    , (entityAttr, fg V.white)
    , (plantAttr, fg V.green)
    , (rockAttr, fg (V.rgbColor @Int 80 80 80))
    , (woodAttr, fg (V.rgbColor @Int 139 69 19))
    , (flowerAttr, fg (V.rgbColor @Int 200 0 200))
    , (copperAttr, fg V.yellow)
    , (copperAttr', fg (V.rgbColor @Int 78 117 102))
    , (ironAttr, fg (V.rgbColor @Int 97 102 106))
    , (ironAttr', fg (V.rgbColor @Int 183 65 14))
    , (quartzAttr, fg V.white)
    , (snowAttr, fg V.white)
    , (sandAttr, fg (V.rgbColor @Int 194 178 128))
    , (fireAttr, fg V.red `V.withStyle` V.bold)
    , (deviceAttr, fg V.yellow `V.withStyle` V.bold)
    , -- Terrain attributes
      (dirtAttr, fg (V.rgbColor @Int 165 42 42))
    , (grassAttr, fg (V.rgbColor @Int 0 32 0)) -- dark green
    , (stoneAttr, fg (V.rgbColor @Int 32 32 32))
    , (waterAttr, V.white `on` V.blue)
    , (iceAttr, bg V.white)
    , -- UI rendering attributes
      (highlightAttr, fg V.cyan)
    , (invalidFormInputAttr, fg V.red)
    , (focusedFormInputAttr, V.defAttr)
    , (listSelectedFocusedAttr, bg V.blue)
    , (infoAttr, fg (V.rgbColor @Int 50 50 50))
    , (buttonSelectedAttr, bg V.blue)
    , -- Default attribute
      (defAttr, V.defAttr)
    ]

-- | Some defined attribute names used in the Swarm TUI.
robotAttr
  , entityAttr
  , plantAttr
  , flowerAttr
  , copperAttr
  , copperAttr'
  , ironAttr
  , ironAttr'
  , quartzAttr
  , snowAttr
  , sandAttr
  , rockAttr
  , baseAttr
  , fireAttr
  , woodAttr
  , deviceAttr
  , dirtAttr
  , grassAttr
  , stoneAttr
  , waterAttr
  , iceAttr
  , highlightAttr
  , sepAttr
  , infoAttr
  , defAttr ::
    AttrName
dirtAttr = "dirt"
grassAttr = "grass"
stoneAttr = "stone"
waterAttr = "water"
iceAttr = "ice"
robotAttr = "robot"
entityAttr = "entity"
plantAttr = "plant"
flowerAttr = "flower"
copperAttr = "copper"
copperAttr' = "copper'"
ironAttr = "iron"
ironAttr' = "iron'"
quartzAttr = "quartz"
snowAttr = "snow"
sandAttr = "sand"
fireAttr = "fire"
rockAttr = "rock"
woodAttr = "wood"
baseAttr = "base"
deviceAttr = "device"
highlightAttr = "highlight"
sepAttr = "sep"
infoAttr = "info"
defAttr = "def"

instance ToJSON AttrName where
  toJSON = toJSON . head . attrNameComponents

instance FromJSON AttrName where
  parseJSON = withText "AttrName" (pure . attrName . from)
