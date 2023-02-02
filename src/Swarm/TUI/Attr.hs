{-# LANGUAGE OverloadedStrings #-}
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
--
-- We export constants only for those we use in the Haskell code
-- and not those used in the world map, to avoid abusing attributes.
-- For example using the robot attribute to highlight some text.
--
-- The few attributes that we use for drawing the logo are an exception.
module Swarm.TUI.Attr (
  swarmAttrMap,
  worldAttributes,
  worldPrefix,

  -- ** Terrain attributes
  dirtAttr,
  grassAttr,
  stoneAttr,
  waterAttr,
  iceAttr,

  -- ** Common attributes
  entityAttr,
  robotAttr,
  rockAttr,
  plantAttr,

  -- ** Swarm TUI Attributes
  highlightAttr,
  notifAttr,
  infoAttr,
  boldAttr,
  dimAttr,
  magentaAttr,
  cyanAttr,
  lightCyanAttr,
  yellowAttr,
  blueAttr,
  greenAttr,
  redAttr,
  defAttr,
  customEditFocusedAttr,
) where

import Brick
import Brick.Forms
import Brick.Widgets.Dialog
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List
import Data.Bifunctor (bimap)
import Data.Yaml
import Graphics.Vty qualified as V
import Witch (from)

-- | A mapping from the defined attribute names to TUI attributes.
swarmAttrMap :: AttrMap
swarmAttrMap =
  attrMap
    V.defAttr
    $ worldAttributes
      <> [(waterAttr, V.white `on` V.blue)]
      <> terrainAttr
      <> [ -- Robot attribute
           (robotAttr, fg V.white `V.withStyle` V.bold)
         , -- UI rendering attributes
           (highlightAttr, fg V.cyan)
         , (invalidFormInputAttr, fg V.red)
         , (focusedFormInputAttr, V.defAttr)
         , (customEditFocusedAttr, V.black `on` V.yellow)
         , (listSelectedFocusedAttr, bg V.blue)
         , (infoAttr, fg (V.rgbColor @Int 50 50 50))
         , (buttonSelectedAttr, bg V.blue)
         , (notifAttr, fg V.yellow `V.withStyle` V.bold)
         , (dimAttr, V.defAttr `V.withStyle` V.dim)
         , (boldAttr, V.defAttr `V.withStyle` V.bold)
         , -- Basic colors
           (redAttr, fg V.red)
         , (greenAttr, fg V.green)
         , (blueAttr, fg V.blue)
         , (yellowAttr, fg V.yellow)
         , (cyanAttr, fg V.cyan)
         , (lightCyanAttr, fg (V.rgbColor @Int 200 255 255))
         , (magentaAttr, fg V.magenta)
         , -- Default attribute
           (defAttr, V.defAttr)
         ]

entityAttr :: AttrName
entityAttr = fst $ head worldAttributes

worldPrefix :: AttrName
worldPrefix = attrName "world"

-- | Colors of entities in the world.
--
-- Also used to color messages, so water is special and excluded.
worldAttributes :: [(AttrName, V.Attr)]
worldAttributes =
  bimap ((worldPrefix <>) . attrName) fg
    <$> [ ("entity", V.white)
        , ("device", V.brightYellow)
        , ("plant", V.green)
        , ("rock", V.rgbColor @Int 80 80 80)
        , ("wood", V.rgbColor @Int 139 69 19)
        , ("flower", V.rgbColor @Int 200 0 200)
        , ("rubber", V.rgbColor @Int 245 224 179)
        , ("copper", V.yellow)
        , ("copper'", V.rgbColor @Int 78 117 102)
        , ("iron", V.rgbColor @Int 97 102 106)
        , ("iron'", V.rgbColor @Int 183 65 14)
        , ("quartz", V.white)
        , ("silver", V.rgbColor @Int 192 192 192)
        , ("gold", V.rgbColor @Int 255 215 0)
        , ("snow", V.white)
        , ("sand", V.rgbColor @Int 194 178 128)
        , ("fire", V.brightRed)
        , ("red", V.red)
        , ("green", V.green)
        , ("blue", V.blue)
        ]

terrainPrefix :: AttrName
terrainPrefix = attrName "terrain"

terrainAttr :: [(AttrName, V.Attr)]
terrainAttr =
  [ (dirtAttr, fg (V.rgbColor @Int 165 42 42))
  , (grassAttr, fg (V.rgbColor @Int 0 32 0)) -- dark green
  , (stoneAttr, fg (V.rgbColor @Int 32 32 32))
  , (iceAttr, bg V.white)
  ]

-- | The default robot attribute.
robotAttr :: AttrName
robotAttr = attrName "robot"

dirtAttr, grassAttr, stoneAttr, iceAttr, waterAttr, rockAttr, plantAttr :: AttrName
dirtAttr = terrainPrefix <> attrName "dirt"
grassAttr = terrainPrefix <> attrName "grass"
stoneAttr = terrainPrefix <> attrName "stone"
iceAttr = terrainPrefix <> attrName "ice"
waterAttr = worldPrefix <> attrName "water"
rockAttr = worldPrefix <> attrName "rock"
plantAttr = worldPrefix <> attrName "plant"

-- | Some defined attribute names used in the Swarm TUI.
highlightAttr
  , notifAttr
  , infoAttr
  , boldAttr
  , dimAttr
  , defAttr ::
    AttrName
highlightAttr = attrName "highlight"
notifAttr = attrName "notif"
infoAttr = attrName "info"
boldAttr = attrName "bold"
dimAttr = attrName "dim"
defAttr = attrName "def"

-- | Some basic colors used in TUI.
redAttr, greenAttr, blueAttr, yellowAttr, cyanAttr, lightCyanAttr, magentaAttr :: AttrName
redAttr = attrName "red"
greenAttr = attrName "green"
blueAttr = attrName "blue"
yellowAttr = attrName "yellow"
cyanAttr = attrName "cyan"
lightCyanAttr = attrName "lightCyan"
magentaAttr = attrName "magenta"

customEditFocusedAttr = attrName "custom" <> E.editFocusedAttr

instance ToJSON AttrName where
  toJSON = toJSON . head . attrNameComponents

instance FromJSON AttrName where
  parseJSON = withText "AttrName" (pure . attrName . from)
