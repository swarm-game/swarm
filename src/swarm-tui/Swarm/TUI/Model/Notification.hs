{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.TUI.Model.Notification where

import Control.Lens (makeLenses, use, (%~), (+=), (.=), _2, _Just)
import Control.Monad.State (MonadState)
import Data.Functor (($>))
import Data.Sequence (Seq, (|>), pattern (:<|))
import Data.Sequence qualified as Seq
import Data.Text (Text)

-- XXX this should become something more interesting
type Notification = Text

data NotificationState = NS
  { _currentNotification :: Maybe (Notification, Int)
  , _notificationQueue :: Seq Notification
  }

makeLenses ''NotificationState

initNotificationState :: NotificationState
initNotificationState =
  NS
    { _currentNotification = Nothing
    , _notificationQueue = Seq.empty
    }

-- | Add a notification to the end of the queue.
addNotification :: Notification -> NotificationState -> NotificationState
addNotification notif = notificationQueue %~ (|> notif)

-- | The number of frames for which to display a notification.
notificationFrames :: Int
notificationFrames = 100

-- | Progress the notification state by one frame: pull the next
--   notification from the queue if there is no current notification
--   or the current notification has reached the max frame count;
--   otherwise just increment the frame count of the current
--   notification.
--
--   Return True if something was updated that might require redrawing
--   the UI.
progressNotifications :: MonadState NotificationState m => m Bool
progressNotifications = do
  cur <- use currentNotification
  case cur of
    Nothing -> nextNotification
    Just (_, frameCount)
      | frameCount == notificationFrames -> nextNotification $> True
      | otherwise -> do
          currentNotification . _Just . _2 += 1
          pure True

-- | Move the next notification (if any) from the queue to the
--   currently displayed notification.  Return True if there was any
--   notification to move.
nextNotification :: MonadState NotificationState m => m Bool
nextNotification = do
  q <- use notificationQueue
  case q of
    Seq.Empty -> do
      currentNotification .= Nothing
      pure False
    n :<| ns -> do
      currentNotification .= Just (n, 0)
      notificationQueue .= ns
      pure True
