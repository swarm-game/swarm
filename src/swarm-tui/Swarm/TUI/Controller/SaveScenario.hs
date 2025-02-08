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
import Brick.Widgets.List qualified as BL
import Control.Lens as Lens
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time (getZonedTime)
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Scenario.Status (updateScenarioInfoOnFinish)
import Swarm.Game.ScenarioInfo
import Swarm.Game.State
import Swarm.Game.State.Runtime
import Swarm.Game.State.Substate
import Swarm.TUI.Model
import Swarm.TUI.Model.Achievements (attainAchievement')
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.UI
import Swarm.TUI.Model.UI.Gameplay
import System.FilePath (splitDirectories)

getNormalizedCurrentScenarioPath :: (MonadIO m, MonadState AppState m) => m (Maybe FilePath)
getNormalizedCurrentScenarioPath =
  -- the path should be normalized and good to search in scenario collection
  use (gameState . currentScenarioPath) >>= \case
    Nothing -> return Nothing
    Just p' -> do
      gs <- use $ runtimeState . scenarios
      Just <$> liftIO (normalizeScenarioPath gs p')

saveScenarioInfoOnFinish :: (MonadIO m, MonadState AppState m) => FilePath -> m (Maybe ScenarioInfo)
saveScenarioInfoOnFinish p = do
  initialRunCode <- use $ gameState . gameControls . initiallyRunCode
  t <- liftIO getZonedTime
  wc <- use $ gameState . winCondition
  let won = case wc of
        WinConditions (Won _ _) _ -> True
        _ -> False
  ts <- use $ gameState . temporal . ticks
  saved <- use $ gameState . completionStatsSaved

  -- NOTE: This traversal is apparently not the same one as used by
  -- the scenario selection menu, so the menu needs to be updated separately.
  -- See Note [scenario menu update]
  let currentScenarioInfo :: Traversal' AppState ScenarioInfo
      currentScenarioInfo = runtimeState . scenarios . scenarioItemByPath p . _SISingle . _2

  replHist <- use $ uiState . uiGameplay . uiREPL . replHistory
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

  gameState . completionStatsSaved .= won

  return status

-- | Don't save progress for developers and cheaters.
unlessCheating :: MonadState AppState m => m () -> m ()
unlessCheating a = do
  debugging <- use $ uiState . uiDebugOptions
  isAuto <- use $ uiState . uiGameplay . uiIsAutoPlay
  when (null debugging && not isAuto) a

-- | Write the @ScenarioInfo@ out to disk when finishing a game (i.e. on winning or exit).
saveScenarioInfoOnFinishNocheat :: (MonadIO m, MonadState AppState m) => m ()
saveScenarioInfoOnFinishNocheat =
  unlessCheating $
    -- the path should be normalized and good to search in scenario collection
    getNormalizedCurrentScenarioPath >>= mapM_ saveScenarioInfoOnFinish

-- | Write the @ScenarioInfo@ out to disk when exiting a game.
saveScenarioInfoOnQuit :: (MonadIO m, MonadState AppState m) => m ()
saveScenarioInfoOnQuit =
  unlessCheating $
    getNormalizedCurrentScenarioPath >>= mapM_ go
 where
  go p = do
    maybeSi <- saveScenarioInfoOnFinish p
    -- Note [scenario menu update]
    -- Ensures that the scenario selection menu gets updated
    -- with the high score/completion status
    forM_
      maybeSi
      ( uiState
          . uiMenu
          . _NewGameMenu
          . ix 0
          . BL.listSelectedElementL
          . _SISingle
          . _2
          .=
      )

    -- See what scenario is currently focused in the menu.  Depending on how the
    -- previous scenario ended (via quit vs. via win), it might be the same as
    -- currentScenarioPath or it might be different.
    curPath <- preuse $ uiState . uiMenu . _NewGameMenu . ix 0 . BL.listSelectedElementL . _SISingle . _2 . scenarioPath

    menu <- use $ uiState . uiMenu
    case menu of
      -- If the menu is NoMenu, it means we skipped showing the
      -- menu at startup, so leave it alone; we simply want to
      -- exit the entire app.
      NoMenu -> pure ()
      -- Otherwise, rebuild the NewGameMenu so it gets the updated
      -- ScenarioInfo, being sure to preserve the same focused
      -- scenario.
      _ -> do
        sc <- use $ runtimeState . scenarios
        forM_ (mkNewGameMenu sc (fromMaybe p curPath)) (uiState . uiMenu .=)
