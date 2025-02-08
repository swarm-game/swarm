{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.TUI.Model.Dialog.Popup (
  -- * Popup types
  Popup (..),

  -- * Popup state
  PopupState,
  currentPopup,
  popupQueue,
  initPopupState,
  addPopup,

  -- * Popup animation
  popupFrames,
  progressPopups,
) where

import Control.Lens (makeLenses, use, (%~), (+=), (.=), _2, _Just)
import Control.Monad.State (MonadState)
import Data.Functor (($>))
import Data.Maybe (isJust)
import Data.Sequence (Seq, (|>), pattern (:<|))
import Data.Sequence qualified as Seq
import Swarm.Game.Achievement.Definitions (CategorizedAchievement)
import Swarm.Language.Syntax (Const)

-- | Different types of popups that can be displayed to the
--   player.
data Popup
  = AchievementPopup CategorizedAchievement
  | RecipesPopup
  | CommandsPopup [Const]
  | DebugWarningPopup

-- | State to track pending popup queue as well as any
--   popup which is currently being displayed.
data PopupState = PopupState
  { _currentPopup :: Maybe (Popup, Int)
  , _popupQueue :: Seq Popup
  }

makeLenses ''PopupState

-- | Initial, empty popup state.
initPopupState :: PopupState
initPopupState =
  PopupState
    { _currentPopup = Nothing
    , _popupQueue = Seq.empty
    }

-- | Add a popup to the end of the queue.
addPopup :: Popup -> PopupState -> PopupState
addPopup notif = popupQueue %~ (|> notif)

-- | The number of frames for which to display a popup.
popupFrames :: Int
popupFrames = 100

-- | Progress the popup state by one frame: pull the next
--   popup from the queue if there is no current popup
--   or the current popup has reached the max frame count;
--   otherwise just increment the frame count of the current
--   popup.
--
--   Return True if something was updated that might require redrawing
--   the UI.
progressPopups :: MonadState PopupState m => m Bool
progressPopups = do
  cur <- use currentPopup
  case cur of
    Nothing -> nextPopup
    Just (_, frameCount)
      | frameCount == popupFrames -> nextPopup $> True
      | otherwise -> do
          currentPopup . _Just . _2 += 1
          pure True

-- | Move the next popup (if any) from the queue to the
--   currently displayed popup.  Return True if there was any
--   popup to move.
nextPopup :: MonadState PopupState m => m Bool
nextPopup = do
  q <- use popupQueue
  cur <- use currentPopup
  case q of
    Seq.Empty
      | isJust cur -> do
          currentPopup .= Nothing
          pure True
      | otherwise -> pure False
    n :<| ns -> do
      currentPopup .= Just (n, 0)
      popupQueue .= ns
      pure True
