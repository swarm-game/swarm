-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Collection of functions used to save the scenario metadata.
module Swarm.TUI.Controller.SaveScenario (
  saveScenarioInfoOnFinish,
  saveScenarioInfoOnFinishNocheat,
  saveScenarioInfoOnQuit,
) where

-- See Note [liftA2 re-export from Prelude]

import Control.Lens as Lens
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState)
import Data.Maybe (listToMaybe)
import Data.Time (getZonedTime)
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Scenario.Status (scenarioIsCompleted, updateScenarioInfoOnFinish)
import Swarm.Game.ScenarioInfo
import Swarm.Game.State
import Swarm.Game.State.Runtime
import Swarm.Game.State.Substate
import Swarm.TUI.Model
import Swarm.TUI.Model.Achievements (attainAchievement, attainAchievement')
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.UI (uiDebugOptions)
import Swarm.TUI.Model.UI.Gameplay
import System.FilePath (splitDirectories)

getNormalizedCurrentScenarioPath ::
  MonadIO m =>
  GameState ->
  ScenarioCollection a ->
  m (Maybe FilePath)
getNormalizedCurrentScenarioPath gs sc = do
  -- the path should be normalized and good to search in scenario collection
  traverse (liftIO . normalizeScenarioPath sc) $ gs ^. currentScenarioPath

saveScenarioInfoOnFinish ::
  (MonadIO m, MonadState AppState m) =>
  FilePath ->
  m ()
saveScenarioInfoOnFinish p = do
  initialRunCode <- use $ playState . gameState . gameControls . initiallyRunCode
  t <- liftIO getZonedTime
  wc <- use $ playState . gameState . winCondition
  let won = case wc of
        WinConditions (Won _ _) _ -> True
        _ -> False
  ts <- use $ playState . gameState . temporal . ticks
  saved <- use $ playState . gameState . completionStatsSaved

  let currentScenarioInfo :: Traversal' AppState ScenarioInfo
      currentScenarioInfo = runtimeState . scenarios . scenarioItemByPath p . _SISingle . _2

  replHist <- use $ playState . uiGameplay . uiREPL . replHistory
  let determinator = CodeSizeDeterminators initialRunCode $ replHist ^. replHasExecutedManualInput

  -- Don't update scenario statistics if we have previously saved
  -- statistics for the current scenario upon scenario completion.
  unless saved $
    currentScenarioInfo
      %= updateScenarioInfoOnFinish determinator t ts won

  status <- preuse currentScenarioInfo
  forM_ status $ \si -> do
    forM_ (listToMaybe $ splitDirectories p) $ \firstDir -> do
      when (won && firstDir == tutorialsDirname) $
        attainAchievement' t (Just p) $
          GlobalAchievement CompletedSingleTutorial
    liftIO $ saveScenarioInfo p si

  -- Check if all tutorials have been completed
  tutorialMap <- use $ runtimeState . scenarios . to getTutorials . to scMap
  let isComplete (SISingle (_, s)) = scenarioIsCompleted s
      -- There are not currently any subcollections within the
      -- tutorials, but checking subcollections recursively just seems
      -- like the right thing to do
      isComplete (SICollection _ (SC m)) = all isComplete m

  when (all isComplete tutorialMap) $
    attainAchievement $
      GlobalAchievement CompletedAllTutorials

  playState . gameState . completionStatsSaved .= won

-- | Don't save progress for developers or cheaters.
unlessCheating :: MonadState AppState m => m () -> m ()
unlessCheating a = do
  debugging <- use $ uiState . uiDebugOptions
  isAuto <- use $ playState . uiGameplay . uiIsAutoPlay
  when (null debugging && not isAuto) a

-- | Write the @ScenarioInfo@ out to disk when finishing a game (i.e. on winning or exit).
saveScenarioInfoOnFinishNocheat :: (MonadIO m, MonadState AppState m) => m ()
saveScenarioInfoOnFinishNocheat = do
  sc <- use $ runtimeState . scenarios
  gs <- use $ playState . gameState
  unlessCheating $
    -- the path should be normalized and good to search in scenario collection
    getNormalizedCurrentScenarioPath gs sc >>= mapM_ saveScenarioInfoOnFinish

-- | Write the @ScenarioInfo@ out to disk when exiting a game.
saveScenarioInfoOnQuit :: (MonadIO m, MonadState AppState m) => m ()
saveScenarioInfoOnQuit = do
  sc <- use $ runtimeState . scenarios
  gs <- use $ playState . gameState
  unlessCheating $
    getNormalizedCurrentScenarioPath gs sc >>= mapM_ saveScenarioInfoOnFinish
