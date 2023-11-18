{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Code for drawing the Swarm logo.
module Swarm.TUI.View.Logo where

import Brick
import Brick.Widgets.Center (centerLayer)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Game.Entity.Cosmetic.Specimen
import Swarm.TUI.Model.Name
import Swarm.TUI.View.Attribute.Attr

drawLogo :: Text -> Widget Name
drawLogo = centerLayer . vBox . map (hBox . T.foldr (\c ws -> drawThing c : ws) []) . T.lines
 where
  drawThing :: Char -> Widget Name
  drawThing c = withAttr (attrFor c) $ str [c]

  attrFor :: Char -> AttrName
  attrFor c
    | c `elem` ("<>v^" :: String) = robotAttr
  attrFor 'T' = plantAttr
  attrFor '@' = rockAttr
  attrFor '~' = waterAttr
  attrFor '▒' = dirtAttr
  attrFor _ = defAttr

  waterAttr :: AttrName
  waterAttr = getWorldAttrName $ fst water

  rockAttr :: AttrName
  rockAttr = getWorldAttrName $ fst rock

  plantAttr :: AttrName
  plantAttr = getWorldAttrName $ fst rock

  dirtAttr :: AttrName
  dirtAttr = getTerrainAttrName $ fst dirt
