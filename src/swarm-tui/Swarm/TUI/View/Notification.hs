-- XXX comment me
module Swarm.TUI.View.Notification where

import Brick (Widget (..), cropTopTo, padLeftRight, txt)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenterLayer)
import Brick.Widgets.Core (emptyWidget)
import Control.Lens ((^.))
import Swarm.TUI.Model (AppState, Name, uiState)
import Swarm.TUI.Model.Notification (currentNotification, notificationFrames)
import Swarm.TUI.Model.UI (uiNotifications)

animFrames :: Int
animFrames = 3

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
  | f <= notificationFrames `div` 2 = (f + animFrames - 1) `div` animFrames
  | otherwise = (notificationFrames - f + animFrames - 1) `div` animFrames
