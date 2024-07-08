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
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState)
import Data.Maybe (fromMaybe)
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
import System.FilePath (splitDirectories)
import Prelude hiding (Applicative (..))

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

  -- NOTE: This traversal is apparently not the same one as used by
  -- the scenario selection menu, so the menu needs to be updated separately.
  -- See Note [scenario menu update]
  let currentScenarioInfo :: Traversal' AppState ScenarioInfo
      currentScenarioInfo = runtimeState . scenarios . scenarioItemByPath p . _SISingle . _2

  replHist <- use $ uiState . uiGameplay . uiREPL . replHistory
  let determinator = CodeSizeDeterminators initialRunCode $ replHist ^. replHasExecutedManualInput
  currentScenarioInfo
    %= updateScenarioInfoOnFinish determinator t ts won
  status <- preuse currentScenarioInfo
  case status of
    Nothing -> return ()
    Just si -> do
      let segments = splitDirectories p
      case segments of
        firstDir : _ -> do
          when (won && firstDir == tutorialsDirname) $
            attainAchievement' t (Just p) (GlobalAchievement CompletedSingleTutorial)
        _ -> return ()
      liftIO $ saveScenarioInfo p si
  return status

-- | Write the @ScenarioInfo@ out to disk when finishing a game (i.e. on winning or exit).
saveScenarioInfoOnFinishNocheat :: (MonadIO m, MonadState AppState m) => m ()
saveScenarioInfoOnFinishNocheat = do
  -- Don't save progress if we are in cheat mode
  cheat <- use $ uiState . uiCheatMode
  unless cheat $ do
    -- the path should be normalized and good to search in scenario collection
    getNormalizedCurrentScenarioPath >>= \case
      Nothing -> return ()
      Just p -> void $ saveScenarioInfoOnFinish p

-- | Write the @ScenarioInfo@ out to disk when exiting a game.
saveScenarioInfoOnQuit :: (MonadIO m, MonadState AppState m) => m ()
saveScenarioInfoOnQuit = do
  -- Don't save progress if we are in cheat mode
  -- NOTE This check is duplicated in "saveScenarioInfoOnFinishNocheat"
  cheat <- use $ uiState . uiCheatMode
  unless cheat $ do
    getNormalizedCurrentScenarioPath >>= \case
      Nothing -> return ()
      Just p -> do
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
        -- Now rebuild the NewGameMenu so it gets the updated ScenarioInfo,
        -- being sure to preserve the same focused scenario.
        sc <- use $ runtimeState . scenarios
        forM_ (mkNewGameMenu cheat sc (fromMaybe p curPath)) (uiState . uiMenu .=)
