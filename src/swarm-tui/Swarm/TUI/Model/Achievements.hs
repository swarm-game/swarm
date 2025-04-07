-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Collection of helper functions for managing achievements in other controllers.
module Swarm.TUI.Model.Achievements (
  attainAchievement,
  attainAchievement',
  popupAchievement,
) where

import Brick (EventM)
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
import Swarm.Game.Popup (Popup (AchievementPopup), addPopup)
import Swarm.Game.Scenario.Status (ScenarioPath (..))
import Swarm.Game.State.Runtime

attainAchievement ::
  CategorizedAchievement ->
  EventM n ProgressionState ()
attainAchievement a = do
  currentTime <- liftIO getZonedTime
  attainAchievement' currentTime Nothing a

attainAchievement' ::
  ZonedTime ->
  Maybe ScenarioPath ->
  CategorizedAchievement ->
  EventM n ProgressionState ()
attainAchievement' t p a = do
  mAttainment <- use $ attainedAchievements . at a
  when (isNothing mAttainment) $ popupAchievement a

  attainedAchievements
    %= M.insertWith
      (<>)
      a
      (Attainment a (getScenarioPath <$> p) t)
  newAchievements <- use attainedAchievements
  liftIO $ saveAchievementsInfo $ M.elems newAchievements

-- | Generate a popup for an achievement.
popupAchievement :: MonadState ProgressionState m => CategorizedAchievement -> m ()
popupAchievement ach = uiPopups %= addPopup (AchievementPopup ach)
