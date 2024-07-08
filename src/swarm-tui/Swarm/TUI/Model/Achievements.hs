{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Collection of helper functions for managing achievements in other controllers.
module Swarm.TUI.Model.Achievements (
  attainAchievement,
  attainAchievement',
  notifyAchievement,
) where

import Control.Lens hiding (from, (<.>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState)
import Data.Map qualified as M
import Data.Time (ZonedTime, getZonedTime)
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Achievement.Persistence
import Swarm.TUI.Model
import Swarm.TUI.Model.Notification (addNotification)
import Swarm.TUI.Model.UI

attainAchievement :: (MonadIO m, MonadState AppState m) => CategorizedAchievement -> m ()
attainAchievement a = do
  currentTime <- liftIO getZonedTime
  attainAchievement' currentTime Nothing a

attainAchievement' ::
  (MonadIO m, MonadState AppState m) =>
  ZonedTime ->
  Maybe FilePath ->
  CategorizedAchievement ->
  m ()
attainAchievement' t p a = do
  notifyAchievement a
  (uiState . uiAchievements)
    %= M.insertWith
      (<>)
      a
      (Attainment a p t)
  newAchievements <- use $ uiState . uiAchievements
  liftIO $ saveAchievementsInfo $ M.elems newAchievements

-- | Generate a pop-up notification for an achievement.
notifyAchievement :: MonadState AppState m => CategorizedAchievement -> m ()
notifyAchievement _ = uiState . uiNotifications %= addNotification "New achievement!"
