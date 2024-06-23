-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- TODO: describe
{-# LANGUAGE OverloadedStrings #-}
module Swarm.TUI.Controller.MainEventHandler (
  mainEventHandlers
) where
import Brick.Keybindings qualified as B
import Swarm.TUI.Model.Event (SwarmEvent (..), MainEvent (..))
import Brick
import Swarm.TUI.Model
import Control.Lens as Lens
import Control.Monad (unless)
import Swarm.Game.Scenario.Topography.Structure.Recognition (automatons)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type (originalStructureDefinitions)
import Swarm.Game.State
import Swarm.Game.State.Substate
import Swarm.TUI.Controller.Util
import Swarm.TUI.Model.Goal
import Swarm.TUI.Model.UI


mainEventHandlers :: [B.KeyEventHandler SwarmEvent (EventM Name AppState)]
mainEventHandlers =
    [ B.onEvent (Main QuitEvent) "Open quit game dialog" $ do
        s <- get
        case s ^. gameState . winCondition of
          WinConditions (Won _ _) _ -> toggleModal $ ScenarioEndModal WinModal
          WinConditions (Unwinnable _) _ -> toggleModal $ ScenarioEndModal LoseModal
          _ -> toggleModal QuitModal
    , B.onEvent (Main ViewHelpEvent) "View Help screen" $ toggleModal HelpModal
    , B.onEvent (Main ViewRobotsEvent) "View Robots screen" $ toggleModal RobotsModal
    , B.onEvent (Main ViewRecipesEvent) "View Recipes screen" $ do
        s <- get
        unless (null (s ^. gameState . discovery . availableRecipes . notificationsContent)) $ do
          toggleModal RecipesModal
          gameState . discovery . availableRecipes . notificationsCount .= 0
    , B.onEvent (Main ViewCommandsEvent) "View Commands screen" $ do
        s <- get
        unless (null (s ^. gameState . discovery . availableCommands . notificationsContent)) $ do
          toggleModal CommandsModal
          gameState . discovery . availableCommands . notificationsCount .= 0
    , B.onEvent (Main ViewStructuresEvent) "View Structures screen" $ do
        s <- get
        unless (null $ s ^. gameState . discovery . structureRecognition . automatons . originalStructureDefinitions) $ do
          toggleModal StructuresModal
    , B.onEvent (Main ViewGoalEvent) "View scenario goal description" $ do
      s <- get
      if hasAnythingToShow $ s ^. uiState . uiGameplay . uiGoal . goalsContent
        then toggleModal GoalModal
        else continueWithoutRedraw
    , B.onEvent (Main HideRobotsEvent) "Hide robots for a few ticks" $ do
      t <- liftIO $ getTime Monotonic
      h <- use $ uiState . uiGameplay . uiHideRobotsUntil
      case h >= t of
        -- ignore repeated keypresses
        True -> continueWithoutRedraw
        -- hide for two seconds
        False -> do
          uiState . uiGameplay . uiHideRobotsUntil .= t + TimeSpec 2 0
          invalidateCacheEntry WorldCache
    , B.onEvent (Main ShowCESKDebugEvent) "Show active robot CESK machine debugging line" $ do
      s <- get
      let isPaused = s ^. gameState . temporal . paused
      let hasDebug = hasDebugCapability isCreative s
      when (isPaused && hasDebug) $ do
        debug <- uiState . uiGameplay . uiShowDebug Lens.<%= not
        if debug
          then gameState . temporal . gameStep .= RobotStep SBefore
          else zoomGameState finishGameTick >> void updateUI
    ]