{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Collection of helper functions for managing achievements in other controllers.
module Swarm.TUI.Model.Achievements (
  attainAchievement,
  attainAchievement',
  popupAchievement,
) where

import Control.Lens hiding (from, (<.>))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState)
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Data.Time (ZonedTime, getZonedTime)
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Achievement.Persistence
import Swarm.TUI.Model
import Swarm.TUI.Model.Popup (Popup (AchievementPopup), addPopup)
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
  mAttainment <- use $ uiState . uiAchievements . at a
  when (isNothing mAttainment) $ popupAchievement a

  (uiState . uiAchievements)
    %= M.insertWith
      (<>)
      a
      (Attainment a p t)
  newAchievements <- use $ uiState . uiAchievements
  liftIO $ saveAchievementsInfo $ M.elems newAchievements

-- | Generate a popup for an achievement.
popupAchievement :: MonadState AppState m => CategorizedAchievement -> m ()
popupAchievement ach = uiState . uiPopups %= addPopup (AchievementPopup ach)
