{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Rendering (& animating) notification popups.
module Swarm.TUI.View.Popup where

import Brick (Widget (..), cropTopTo, padLeftRight, txt)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenterLayer)
import Brick.Widgets.Core (emptyWidget, hBox, withAttr)
import Control.Lens ((^.))
import Swarm.Game.Achievement.Definitions (title)
import Swarm.Game.Achievement.Description (describe)
import Swarm.TUI.Model (AppState, Name, uiState)
import Swarm.TUI.Model.Popup (Popup (..), currentPopup, popupFrames)
import Swarm.TUI.Model.UI (uiPopups)
import Swarm.TUI.View.Attribute.Attr (notifAttr)

-- | The number of frames taken by each step of the notification popup
--   animation.
animFrames :: Int
animFrames = 3

-- | Draw the current notification popup (if any).
drawPopups :: AppState -> Widget Name
drawPopups s = hCenterLayer $
  case s ^. uiState . uiPopups . currentPopup of
    Just (notif, f) ->
      cropTopTo (popupRows f) . border . padLeftRight 2 $ drawPopup notif
    _ -> emptyWidget

drawPopup :: Popup -> Widget Name
drawPopup = \case
  AchievementPopup ach ->
    hBox
      [ withAttr notifAttr (txt "Achievement unlocked: ")
      , txt (title (describe ach))
      ]
  RecipesPopup _ ->
    hBox
      [ withAttr notifAttr (txt "New recipes unlocked! ")
      , txt "[F3] to view."
      ]
  CommandsPopup _ ->
    hBox
      [ withAttr notifAttr (txt "New commands unlocked! ")
      , txt "[F4] to view."
      ]

-- | Compute the number of rows of the notification popup we should be
--   showing, based on the number of frames the popup has existed.
--   This is what causes the popup to animate in and out of existence.
popupRows :: Int -> Int
popupRows f
  -- If we're less than halfway through the lifetime of the popup,
  -- divide the number of frames by the number of frames for each step
  -- of the animation (rounded up).  This will become much larger than
  -- the actual number of rows in the popup, but the 'cropTopTo' function
  -- simply has no effect when given any value equal to or larger than the
  -- number of rows of a widget.  This way the animation will continue to
  -- work for popups with any (reasonable) number of rows.
  | f <= popupFrames `div` 2 = (f + animFrames - 1) `div` animFrames
  -- Otherwise, divide the number of frames remaining by the number of
  -- frames for each step of the animation (rounded up).
  | otherwise = (popupFrames - f + animFrames - 1) `div` animFrames
