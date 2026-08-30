{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Code for describing and displaying clickable keybinding hints.
module Swarm.TUI.View.KeyCmd (
  KeyHighlight (..),
  KeyCmd (..),
  drawKeyCmd,
  drawKeyCmds,
) where

import Brick (Widget, clickable, hBox, padLeftRight, txt, withAttr)
import Data.List (intersperse)
import Data.Text (Text)
import Swarm.TUI.Model (Name (..))
import Swarm.TUI.View.Attribute.Attr (defAttr, highlightAttr, notifAttr)
import Swarm.Util (brackets)

-- | How should a keybinding be highlighted?
data KeyHighlight
  = -- | No special highlighting
    NoHighlight
  | -- | Alert the user that something new/exciting is behind this keybinding
    Alert
  | -- | Highlight the fact that this keybinding is specific to a particular panel
    PanelSpecific

-- | A description of keybinding(s) for a single event.
data KeyCmd
  = SingleButton KeyHighlight Text Text
  | MultiButton KeyHighlight [(Text, Text)] Text

-- | Draw a single clickable keybinding.
drawKeyCmd :: KeyCmd -> Widget Name
drawKeyCmd keycmd =
  case keycmd of
    SingleButton h key cmd ->
      clickable (UIShortcut cmd) $
        hBox
          [ withAttr (attr h) (txt $ brackets key)
          , txt cmd
          ]
    MultiButton h keyArr cmd ->
      hBox $ intersperse (txt "/") (map (createCmd h) keyArr) ++ [txt cmd]
 where
  createCmd h (key, cmd) = clickable (UIShortcut cmd) $ withAttr (attr h) (txt $ brackets key)
  attr h = case h of
    NoHighlight -> defAttr
    Alert -> notifAttr
    PanelSpecific -> highlightAttr

-- | Lay out a sequence of keybindings with 1 space of padding on either
--   end and 2 spaces of padding between each.
drawKeyCmds :: [KeyCmd] -> Widget Name
drawKeyCmds = hBox . map (padLeftRight 1 . drawKeyCmd)
