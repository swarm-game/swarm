-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Rendering (& animating) notification popup.
module Swarm.TUI.View.Notification where

import Brick (Widget (..), cropTopTo, padLeftRight, txt)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenterLayer)
import Brick.Widgets.Core (emptyWidget)
import Control.Lens ((^.))
import Swarm.TUI.Model (AppState, Name, uiState)
import Swarm.TUI.Model.Notification (currentNotification, notificationFrames)
import Swarm.TUI.Model.UI (uiNotifications)

-- | The number of frames taken by each step of the notification popup
--   animation.
animFrames :: Int
animFrames = 3

-- | Draw the current notification popup (if any).
drawNotifications :: AppState -> Widget Name
drawNotifications s = hCenterLayer $
  case s ^. uiState . uiNotifications . currentNotification of
    Just (notif, f) ->
      cropTopTo (notificationRows f) . border . padLeftRight 2 $ txt notif
    _ -> emptyWidget

-- | Compute he number of rows of the notification popup we should be
--   showing, based on the number of frames the popup has existed.
--   This is what causes the popup to animate in and out of existence.
notificationRows :: Int -> Int
notificationRows f
  -- If we're less than halfway through the lifetime of the popup,
  -- divide the number of frames by the number of frames for each step
  -- of the animation (rounded up).  This will become much larger than
  -- the actual number of rows in the popup, but the 'cropTopTo' function
  -- simply has no effect when given any value equal to or larger than the
  -- number of rows of a widget.  This way the animation will continue to
  -- work for popups with any (reasonable) number of rows.
  | f <= notificationFrames `div` 2 = (f + animFrames - 1) `div` animFrames
  -- Otherwise, divide the number of frames remaining by the number of
  -- frames for each step of the animation (rounded up).
  | otherwise = (notificationFrames - f + animFrames - 1) `div` animFrames
