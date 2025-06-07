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
  mkBrickColor,

  -- ** Common attributes
  entityAttr,
  robotAttr,

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
  beigeAttr,
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
import Data.Colour.SRGB (RGB (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Graphics.Vty qualified as V
import Swarm.Game.Cosmetic.Assignment
import Swarm.Game.Cosmetic.Attribute
import Swarm.Game.Cosmetic.Color
import Swarm.TUI.View.Attribute.Util

toAttrName :: Attribute -> AttrName
toAttrName = \case
  ARobot -> robotAttr
  AEntity -> entityAttr
  AWorld n -> worldPrefix <> attrName (unpack n)

toVtyAttr :: PreservableColor -> V.Attr
toVtyAttr hifi = case fmap mkBrickColor hifi of
  FgOnly c -> fg c
  BgOnly c -> bg c
  FgAndBg foreground background -> foreground `on` background

mkBrickColor :: TrueColor -> V.Color
mkBrickColor = \case
  Triple (RGB r g b) -> V.linearColor r g b
  AnsiColor x -> case x of
    White -> V.white
    BrightRed -> V.brightRed
    Red -> V.red
    Green -> V.green
    Blue -> V.blue
    BrightYellow -> V.brightYellow
    Yellow -> V.yellow

-- | A mapping from the defined attribute names to TUI attributes.
swarmAttrMap :: AttrMap
swarmAttrMap =
  attrMap
    V.defAttr
    $ NE.toList activityMeterAttributes
      <> NE.toList robotMessageAttributes
      <> map (toAttrName *** toVtyAttr) (M.toList worldAttributes)
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
         , (beigeAttr, fg (V.rgbColor @Int 238 217 196))
         , (cyanAttr, fg V.cyan)
         , (lightCyanAttr, fg (V.rgbColor @Int 200 255 255))
         , (magentaAttr, fg V.magenta)
         , (grayAttr, fg (V.rgbColor @Int 128 128 128))
         , -- Default attribute
           (defAttr, V.defAttr)
         ]

worldPrefix :: AttrName
worldPrefix = attrName "world"

entityAttr :: AttrName
entityAttr = toAttrName $ fst entity

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
redAttr, greenAttr, blueAttr, yellowAttr, beigeAttr, cyanAttr, lightCyanAttr, magentaAttr, grayAttr :: AttrName
redAttr = attrName "red"
greenAttr = attrName "green"
blueAttr = attrName "blue"
yellowAttr = attrName "yellow"
beigeAttr = attrName "beige"
cyanAttr = attrName "cyan"
lightCyanAttr = attrName "lightCyan"
magentaAttr = attrName "magenta"
grayAttr = attrName "gray"
