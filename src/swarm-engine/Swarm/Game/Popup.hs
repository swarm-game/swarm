{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.Game.Popup (
  -- * Popup types
  Popup (..),

  -- * Popup state
  PopupState,
  currentPopup,
  popupQueue,
  initPopupState,
  addPopup,
  nextPopup,
) where

import Control.Lens (makeLenses, use, (%~), (.=))
import Control.Monad.State (MonadState)
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
  { _currentPopup :: Maybe Popup
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

-- | Move the next popup (if any) from the queue to the
--   currently displayed popup.
nextPopup :: MonadState PopupState m => m ()
nextPopup = do
  q <- use popupQueue
  case q of
    Seq.Empty -> currentPopup .= Nothing
    n :<| ns -> do
      currentPopup .= Just n
      popupQueue .= ns
