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
  worldAttributes,
  worldPrefix,
  meterAttributeNames,
  messageAttributeNames,
  toAttrName,
  getWorldAttrName,

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
  grayAttr,
  defAttr,
  customEditFocusedAttr,
) where

import Brick
import Brick.Forms (focusedFormInputAttr, invalidFormInputAttr)
import Brick.Widgets.Dialog
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List (listSelectedFocusedAttr)
import Control.Arrow ((***))
import Data.Colour.Palette.BrewerSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Graphics.Vty qualified as V
import Swarm.Game.Display (Attribute (..))
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Entity.Cosmetic.Specimen
import Swarm.TUI.View.Attribute.Color
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
      <> NE.toList robotMessageAttributes
      <> map (getWorldAttrName *** vtyColor . fromHiFi) (M.toList worldAttributes)
      <> map (getTerrainAttrName *** vtyColor . fromHiFi) (M.toList terrainAttributes)
      <> [ -- Robot attribute
           (robotAttr, fg V.white `V.withStyle` V.bold)
         , -- UI rendering attributes
           (highlightAttr, fg V.cyan)
         , (invalidFormInputAttr, fg V.red)
         , (focusedFormInputAttr, V.defAttr)
         , (customEditFocusedAttr, V.black `on` V.yellow)
         , (listSelectedFocusedAttr, bg V.blue)
         , (infoAttr, fg (V.rgbColor @Int 100 100 100))
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
         , (grayAttr, fg (V.rgbColor @Int 128 128 128))
         , -- Default attribute
           (defAttr, V.defAttr)
         ]

terrainPrefix :: AttrName
terrainPrefix = attrName "terrain"

getTerrainAttrName :: TerrainAttr -> AttrName
getTerrainAttrName (TerrainAttr n) = terrainPrefix <> attrName n

worldPrefix :: AttrName
worldPrefix = attrName "world"

getWorldAttrName :: WorldAttr -> AttrName
getWorldAttrName (WorldAttr n) = worldPrefix <> attrName n

entityAttr :: AttrName
entityAttr = getWorldAttrName $ fst entity

waterAttr :: AttrName
waterAttr = getWorldAttrName $ fst water

rockAttr :: AttrName
rockAttr = getWorldAttrName $ fst rock

plantAttr :: AttrName
plantAttr = getWorldAttrName $ fst rock

robotMessagePrefix :: AttrName
robotMessagePrefix = attrName "robotMessage"

robotMessageAttributes :: NonEmpty (AttrName, V.Attr)
robotMessageAttributes =
  NE.zip indices $ fromMaybe (pure $ fg V.white) $ NE.nonEmpty brewers
 where
  indices = NE.map ((robotMessagePrefix <>) . attrName . show) $ (0 :: Int) :| [1 ..]
  brewers = map (fg . kolorToAttrColor) $ brewerSet Set3 12

messageAttributeNames :: NonEmpty AttrName
messageAttributeNames = NE.map fst robotMessageAttributes

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

-- | The default robot attribute.
robotAttr :: AttrName
robotAttr = attrName "robot"

dirtAttr, grassAttr, stoneAttr, iceAttr :: AttrName
dirtAttr = getTerrainAttrName $ fst dirt
grassAttr = getTerrainAttrName $ fst grass
stoneAttr = getTerrainAttrName $ fst stone
iceAttr = getTerrainAttrName $ fst ice

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
redAttr, greenAttr, blueAttr, yellowAttr, cyanAttr, lightCyanAttr, magentaAttr, grayAttr :: AttrName
redAttr = attrName "red"
greenAttr = attrName "green"
blueAttr = attrName "blue"
yellowAttr = attrName "yellow"
cyanAttr = attrName "cyan"
lightCyanAttr = attrName "lightCyan"
magentaAttr = attrName "magenta"
grayAttr = attrName "gray"
