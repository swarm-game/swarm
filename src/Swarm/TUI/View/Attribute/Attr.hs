{-# LANGUAGE OverloadedStrings #-}

-- |
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
module Swarm.TUI.View.Attribute.Attr (
  swarmAttrMap,
  worldAttributeNames,
  worldPrefix,
  meterAttributeNames,
  toAttrName,

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
  italicAttr,
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
import Brick.Widgets.List hiding (reverse)
import Data.Bifunctor (bimap, first)
import Data.Colour.Palette.BrewerSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Graphics.Vty qualified as V
import Swarm.Game.Display (Attribute (..))
import Swarm.TUI.View.Attribute.Util

toAttrName :: Attribute -> AttrName
toAttrName = \case
  ARobot -> robotAttr
  AEntity -> entityAttr
  AWorld n -> worldPrefix <> attrName (unpack n)
  ATerrain n -> terrainPrefix <> attrName (unpack n)
  ADefault -> defAttr

-- | A mapping from the defined attribute names to TUI attributes.
swarmAttrMap :: AttrMap
swarmAttrMap =
  attrMap
    V.defAttr
    $ NE.toList activityMeterAttributes
      <> NE.toList (NE.map (first getWorldAttrName) worldAttributes)
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
         , (italicAttr, V.defAttr `V.withStyle` V.italic)
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

worldPrefix :: AttrName
worldPrefix = attrName "world"

-- | We introduce this (module-private) newtype
-- so that we can define the 'entity' attribute
-- separate from the list of other 'worldAttributes',
-- while enforcing the convention that both its attribute
-- name and the rest of 'worldAttributes' be consistently
-- prefixed by 'worldPrefix'.
newtype WorldAttr = WorldAttr
  { getWorldAttrName :: AttrName
  }

mkWorldAttr :: String -> WorldAttr
mkWorldAttr = WorldAttr . (worldPrefix <>) . attrName

entity :: (WorldAttr, V.Attr)
entity = (mkWorldAttr "entity", fg V.white)

entityAttr :: AttrName
entityAttr = getWorldAttrName $ fst entity

-- | Colors of entities in the world.
--
-- Also used to color messages, so water is special and excluded.
worldAttributes :: NonEmpty (WorldAttr, V.Attr)
worldAttributes =
  entity
    :| map
      (bimap mkWorldAttr fg)
      [ ("device", V.brightYellow)
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

worldAttributeNames :: NonEmpty AttrName
worldAttributeNames = NE.map (getWorldAttrName . fst) worldAttributes

activityMeterPrefix :: AttrName
activityMeterPrefix = attrName "activityMeter"

activityMeterAttributes :: NonEmpty (AttrName, V.Attr)
activityMeterAttributes =
  NE.zip indices $ fromMaybe (pure $ bg V.black) $ NE.nonEmpty brewers
 where
  indices = NE.map ((activityMeterPrefix <>) . attrName . show) $ (0 :: Int) :| [1 ..]
  brewers = map bgWithAutoForeground $ reverse $ brewerSet RdYlGn 7

meterAttributeNames :: NonEmpty AttrName
meterAttributeNames = NE.map fst activityMeterAttributes

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
  , italicAttr
  , dimAttr
  , defAttr ::
    AttrName
highlightAttr = attrName "highlight"
notifAttr = attrName "notif"
infoAttr = attrName "info"
boldAttr = attrName "bold"
italicAttr = attrName "italics"
dimAttr = attrName "dim"
defAttr = attrName "def"

customEditFocusedAttr :: AttrName
customEditFocusedAttr = attrName "custom" <> E.editFocusedAttr

-- | Some basic colors used in TUI.
redAttr, greenAttr, blueAttr, yellowAttr, cyanAttr, lightCyanAttr, magentaAttr :: AttrName
redAttr = attrName "red"
greenAttr = attrName "green"
blueAttr = attrName "blue"
yellowAttr = attrName "yellow"
cyanAttr = attrName "cyan"
lightCyanAttr = attrName "lightCyan"
magentaAttr = attrName "magenta"
