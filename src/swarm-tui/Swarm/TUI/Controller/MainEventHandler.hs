{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- TODO: describe
module Swarm.TUI.Controller.MainEventHandler (
  mainEventHandlers,
) where

import Brick
import Brick.Keybindings
import Control.Lens as Lens
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty.Input.Events qualified as V
import Swarm.Game.Scenario.Topography.Structure.Recognition (automatons)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type (originalStructureDefinitions)
import Swarm.Game.State
import Swarm.Game.State.Substate
import Swarm.Game.Step (finishGameTick)
import Swarm.TUI.Controller.FrameEventHandling (runGameTickUI)
import Swarm.TUI.Controller.UpdateUI (updateUI)
import Swarm.TUI.Controller.Util
import Swarm.TUI.Model
import Swarm.TUI.Model.Event (MainEvent (..), SwarmEvent (..))
import Swarm.TUI.Model.Goal
import Swarm.TUI.Model.UI
import System.Clock (Clock (..), TimeSpec (..), getTime)
import Swarm.TUI.Editor.Model (worldOverdraw, isWorldEditorEnabled)

-- | Main keybindings event handler while running the game itself.
--
-- See 'Swarm.TUI.Controller.handleMainEvent'.
mainEventHandlers :: [KeyEventHandler SwarmEvent (EventM Name AppState)]
mainEventHandlers = nonCustomizableHandlers <> customizableHandlers
 where
  nonCustomizableHandlers =
    [ onKey V.KEsc "Close open modal" closeModal
    ]
  customizableHandlers = allHandlers Main $ \case
    QuitEvent -> ("Open quit game dialog", toggleQuitGameDialog)
    ViewHelpEvent -> ("View Help screen", toggleModal HelpModal)
    ViewRobotsEvent -> ("View Robots screen", toggleModal RobotsModal)
    ViewRecipesEvent -> ("View Recipes screen", toggleDiscoveryNotificationModal RecipesModal availableRecipes)
    ViewCommandsEvent -> ("View Commands screen", toggleDiscoveryNotificationModal CommandsModal availableCommands)
    ViewMessagesEvent -> ("View Messages screen", toggleMessagesModal)
    ViewStructuresEvent -> ("View Structures screen", toggleDiscoveryModal StructuresModal (structureRecognition . automatons . originalStructureDefinitions))
    ViewGoalEvent -> ("View scenario goal description", viewGoal)
    HideRobotsEvent -> ("Hide robots for a few ticks", hideRobots)
    ShowCESKDebugEvent -> ("Show active robot CESK machine debugging line", showCESKDebug)
    PauseEvent -> ("Pause or unpause the game", whenRunning safeTogglePause)
    RunSingleTickEvent -> ("Run game for a single tick", whenRunning runSingleTick)
    IncreaseTpsEvent -> ("Increase game speed by one tick per second", whenRunning . modify $ adjustTPS (+))
    DecreaseTpsEvent -> ("Descrease game speed by one tick per second", whenRunning . modify $ adjustTPS (-))
    FocusWorldEvent -> ("Set focus on the World panel", setFocus WorldPanel)
    FocusRobotEvent -> ("Set focus on the Robot panel", setFocus RobotPanel)
    FocusREPLEvent -> ("Set focus on the REPL panel", setFocus REPLPanel)
    FocusInfoEvent -> ("Set focus on the Info panel", setFocus InfoPanel)
    ToggleCreativeModeEvent -> ("Toggle creative mode", whenCheating toggleCreativeMode)
    ToggleWorldEditorEvent -> ("Toggle world editor mode", whenCheating toggleWorldEditor)

closeModal :: EventM Name AppState ()
closeModal = do
  s <- get
  case s ^. uiState . uiGameplay . uiModal of
    Nothing -> return ()
    Just m -> do
      safeAutoUnpause
      uiState . uiGameplay . uiModal .= Nothing
      -- message modal is not autopaused, so update notifications when leaving it
      case m ^. modalType of
        MessagesModal -> do
          gameState . messageInfo . lastSeenMessageTime .= s ^. gameState . temporal . ticks
        _ -> return ()

toggleQuitGameDialog :: EventM Name AppState ()
toggleQuitGameDialog = do
  s <- get
  case s ^. gameState . winCondition of
    WinConditions (Won _ _) _ -> toggleModal $ ScenarioEndModal WinModal
    WinConditions (Unwinnable _) _ -> toggleModal $ ScenarioEndModal LoseModal
    _ -> toggleModal QuitModal

toggleGameModal :: Foldable t => ModalType -> Getter GameState (t a) -> EventM Name AppState Bool
toggleGameModal m l = do
  s <- get
  let nothingToShow = null $ s ^. gameState . l
  unless nothingToShow $ toggleModal m
  return nothingToShow

toggleDiscoveryModal :: Foldable t => ModalType -> Lens' Discovery (t a) -> EventM Name AppState ()
toggleDiscoveryModal m l = void $ toggleGameModal m (discovery . l)

toggleDiscoveryNotificationModal :: ModalType -> Lens' Discovery (Notifications a) -> EventM Name AppState ()
toggleDiscoveryNotificationModal m l = do
  nothingToShow <- toggleGameModal m (discovery . l . notificationsContent)
  unless nothingToShow $ gameState . discovery . l . notificationsCount .= 0

toggleMessagesModal :: EventM Name AppState ()
toggleMessagesModal = do
  s <- get
  nothingToShow <- toggleGameModal MessagesModal (messageNotifications . notificationsContent)
  unless nothingToShow $ gameState . messageInfo . lastSeenMessageTime .= s ^. gameState . temporal . ticks

viewGoal :: EventM Name AppState ()
viewGoal = do
  s <- get
  if hasAnythingToShow $ s ^. uiState . uiGameplay . uiGoal . goalsContent
    then toggleModal GoalModal
    else continueWithoutRedraw

hideRobots :: EventM Name AppState ()
hideRobots = do
  t <- liftIO $ getTime Monotonic
  h <- use $ uiState . uiGameplay . uiHideRobotsUntil
  case h >= t of
    -- ignore repeated keypresses
    True -> continueWithoutRedraw
    -- hide for two seconds
    False -> do
      uiState . uiGameplay . uiHideRobotsUntil .= t + TimeSpec 2 0
      invalidateCacheEntry WorldCache

showCESKDebug :: EventM Name AppState ()
showCESKDebug = do
  s <- get
  let isPaused = s ^. gameState . temporal . paused
  let isCreative = s ^. gameState . creativeMode
  let hasDebug = hasDebugCapability isCreative s
  when (isPaused && hasDebug) $ do
    debug <- uiState . uiGameplay . uiShowDebug Lens.<%= not
    if debug
      then gameState . temporal . gameStep .= RobotStep SBefore
      else zoomGameState finishGameTick >> void updateUI

runSingleTick :: EventM Name AppState ()
runSingleTick = do
  gameState . temporal . runStatus .= ManualPause
  runGameTickUI

-- | Adjust the ticks per second speed.
adjustTPS :: (Int -> Int -> Int) -> AppState -> AppState
adjustTPS (+/-) = uiState . uiGameplay . uiTiming . lgTicksPerSecond %~ (+/- 1)

toggleCreativeMode :: EventM Name AppState ()
toggleCreativeMode = gameState . creativeMode %= not

toggleWorldEditor :: EventM Name AppState ()
toggleWorldEditor = do
  uiState . uiGameplay . uiWorldEditor . worldOverdraw . isWorldEditorEnabled %= not
  setFocus WorldEditorPanel

-- ----------------------------------------------
--                 HELPER UTILS
-- ----------------------------------------------

isRunning :: EventM Name AppState Bool
isRunning = do
  mt <- preuse $ uiState . uiGameplay . uiModal . _Just . modalType
  return $ maybe True isRunningModal mt

whenRunning :: EventM Name AppState () -> EventM Name AppState ()
whenRunning a = isRunning >>= \r -> when r a

whenCheating :: EventM Name AppState () -> EventM Name AppState ()
whenCheating a = do
  s <- get
  when (s ^. uiState . uiCheatMode) a
