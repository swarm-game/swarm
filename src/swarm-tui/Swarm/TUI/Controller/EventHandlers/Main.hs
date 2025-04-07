{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Here is the main player configurable key event handler while playing the game.
module Swarm.TUI.Controller.EventHandlers.Main (
  mainEventHandlers,
  isRunning,
  whenRunningAppState,
  whenRunningPlayState,
  runSingleTick,
  adjustTPS,
  toggleREPLVisibility,
  showCESKDebug,
  hideRobots,
  toggleDiscoveryNotificationModal,
  viewGoal,
  toggleMessagesModal,
) where

import Brick
import Brick.Keybindings
import Control.Lens as Lens
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type (originalStructureDefinitions)
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Robot
import Swarm.Game.State.Substate
import Swarm.Game.Step (finishGameTick)
import Swarm.TUI.Controller.EventHandlers.Frame (runGameTickUI)
import Swarm.TUI.Controller.UpdateUI (updateUI)
import Swarm.TUI.Controller.Util
import Swarm.TUI.Editor.Model (isWorldEditorEnabled, worldOverdraw)
import Swarm.TUI.Model
import Swarm.TUI.Model.DebugOption (DebugOption (ToggleCreative, ToggleWorldEditor))
import Swarm.TUI.Model.Dialog.Goal
import Swarm.TUI.Model.Event (MainEvent (..), SwarmEvent (..))
import Swarm.TUI.Model.UI
import Swarm.TUI.Model.UI.Gameplay
import System.Clock (Clock (..), TimeSpec (..), getTime)

-- | Main keybindings event handler while running the game itself.
--
-- See 'Swarm.TUI.Controller.handleMainEvent'.
mainEventHandlers :: [KeyEventHandler SwarmEvent (EventM Name AppState)]
mainEventHandlers = allHandlers Main $ \case
  QuitEvent -> ("Open quit game dialog", toggleQuitGameDialog)
  ViewHelpEvent ->
    ( "View Help screen"
    , playStateWithMenu $ toggleModal HelpModal
    )
  ViewRobotsEvent ->
    ( "View Robots screen"
    , playStateWithMenu $ toggleModal RobotsModal
    )
  ViewRecipesEvent ->
    ( "View Recipes screen"
    , playStateWithMenu $ toggleDiscoveryNotificationModal RecipesModal availableRecipes
    )
  ViewCommandsEvent ->
    ( "View Commands screen"
    , playStateWithMenu $ toggleDiscoveryNotificationModal CommandsModal availableCommands
    )
  ViewMessagesEvent ->
    ( "View Messages screen"
    , playStateWithMenu toggleMessagesModal
    )
  ViewStructuresEvent ->
    ( "View Structures screen"
    , playStateWithMenu $
        toggleStructuresModal StructuresModal (recognizerAutomatons . originalStructureDefinitions)
    )
  ViewGoalEvent ->
    ( "View scenario goal description"
    , playStateWithMenu viewGoal
    )
  HideRobotsEvent -> ("Hide robots for a few ticks", Brick.zoom (playState . scenarioState . uiGameplay) hideRobots)
  ShowCESKDebugEvent -> ("Show active robot CESK machine debugging line", showCESKDebug)
  PauseEvent -> ("Pause or unpause the game", Brick.zoom (playState . scenarioState) $ whenRunningPlayState safeTogglePause)
  RunSingleTickEvent -> ("Run game for a single tick", whenRunningAppState runSingleTick)
  IncreaseTpsEvent -> ("Double game speed", Brick.zoom (playState . scenarioState) $ whenRunningPlayState . modify $ adjustTPS (+))
  DecreaseTpsEvent -> ("Halve game speed", Brick.zoom (playState . scenarioState) $ whenRunningPlayState . modify $ adjustTPS (-))
  FocusWorldEvent -> ("Set focus on the World panel", Brick.zoom (playState . scenarioState) $ setFocus WorldPanel)
  FocusRobotEvent -> ("Set focus on the Robot panel", Brick.zoom (playState . scenarioState) $ setFocus RobotPanel)
  FocusREPLEvent -> ("Set focus on the REPL panel", Brick.zoom (playState . scenarioState) $ setFocus REPLPanel)
  FocusInfoEvent -> ("Set focus on the Info panel", Brick.zoom (playState . scenarioState) $ setFocus InfoPanel)
  ToggleCreativeModeEvent -> ("Toggle creative mode", whenDebug ToggleCreative $ Brick.zoom (playState . scenarioState) toggleCreativeMode)
  ToggleWorldEditorEvent -> ("Toggle world editor mode", whenDebug ToggleWorldEditor $ Brick.zoom (playState . scenarioState) toggleWorldEditor)
  ToggleREPLVisibilityEvent -> ("Collapse/Expand REPL panel", Brick.zoom (playState . scenarioState) toggleREPLVisibility)
  ViewBaseEvent -> ("View the base robot", Brick.zoom (playState . scenarioState) viewBase)
  ToggleFPSEvent -> ("Toggle the FPS display", Brick.zoom (playState . scenarioState) toggleFPS)

toggleQuitGameDialog :: EventM Name AppState ()
toggleQuitGameDialog = do
  s <- get
  let whichModal = case s ^. playState . scenarioState . gameState . winCondition of
        WinConditions (Won _ _) _ -> ScenarioEndModal WinModal
        WinConditions (Unwinnable _) _ -> ScenarioEndModal LoseModal
        _ -> QuitModal
  playStateWithMenu $ toggleModal whichModal

toggleGameModal ::
  Foldable t =>
  Menu ->
  ModalType ->
  Getter GameState (t a) ->
  EventM Name ScenarioState Bool
toggleGameModal menu m l = do
  s <- get
  let nothingToShow = null $ s ^. gameState . l

  unless nothingToShow $
    toggleModal m menu
  return nothingToShow

toggleStructuresModal ::
  Foldable t =>
  ModalType ->
  Lens' Landscape (t a) ->
  Menu ->
  EventM Name ScenarioState ()
toggleStructuresModal m l menu = void $ toggleGameModal menu m (landscape . l)

toggleDiscoveryNotificationModal ::
  ModalType ->
  Lens' Discovery (Notifications a) ->
  Menu ->
  EventM Name ScenarioState ()
toggleDiscoveryNotificationModal m l menu = do
  nothingToShow <- toggleGameModal menu m (discovery . l . notificationsContent)
  unless nothingToShow $ gameState . discovery . l . notificationsCount .= 0

toggleMessagesModal :: Menu -> EventM Name ScenarioState ()
toggleMessagesModal menu = do
  s <- get
  nothingToShow <- toggleGameModal menu MessagesModal (messageNotifications . notificationsContent)
  unless nothingToShow $ gameState . messageInfo . lastSeenMessageTime .= s ^. gameState . temporal . ticks

viewGoal :: Menu -> EventM Name ScenarioState ()
viewGoal m = do
  s <- get
  if hasAnythingToShow $ s ^. uiGameplay . uiDialogs . uiGoal . goalsContent
    then toggleModal GoalModal m
    else continueWithoutRedraw

hideRobots :: EventM Name UIGameplay ()
hideRobots = do
  t <- liftIO $ getTime Monotonic
  h <- use uiHideRobotsUntil
  case h >= t of
    -- ignore repeated keypresses
    True -> continueWithoutRedraw
    -- hide for two seconds
    False -> do
      uiHideRobotsUntil .= t + TimeSpec 2 0
      invalidateCacheEntry WorldCache

showCESKDebug :: EventM Name AppState ()
showCESKDebug = do
  s <- get
  let isPaused = s ^. playState . scenarioState . gameState . temporal . paused
  let isCreative = s ^. playState . scenarioState . gameState . creativeMode
  let hasDebug = hasDebugCapability isCreative $ s ^. playState . scenarioState . gameState
  when (isPaused && hasDebug) $ do
    debug <- playState . scenarioState . uiGameplay . uiShowDebug Lens.<%= not
    if debug
      then playState . scenarioState . gameState . temporal . gameStep .= RobotStep SBefore
      else zoomGameStateFromAppState finishGameTick >> void updateUI

runSingleTick :: EventM Name AppState ()
runSingleTick = do
  playState . scenarioState . gameState . temporal . runStatus .= ManualPause
  runGameTickUI

-- | Adjust the ticks per second speed.
adjustTPS :: (Int -> Int -> Int) -> ScenarioState -> ScenarioState
adjustTPS (+/-) = uiGameplay . uiTiming . lgTicksPerSecond %~ (+/- 1)

toggleCreativeMode :: EventM Name ScenarioState ()
toggleCreativeMode = gameState . creativeMode %= not

toggleWorldEditor :: EventM Name ScenarioState ()
toggleWorldEditor = do
  uiGameplay . uiWorldEditor . worldOverdraw . isWorldEditorEnabled %= not
  setFocus WorldEditorPanel

toggleREPLVisibility :: EventM Name ScenarioState ()
toggleREPLVisibility = do
  invalidateCacheEntry WorldCache
  uiGameplay . uiShowREPL %= not

viewBase :: EventM Name ScenarioState ()
viewBase = do
  invalidateCacheEntry WorldCache
  gameState . robotInfo . viewCenterRule .= VCRobot 0

toggleFPS :: EventM Name ScenarioState ()
toggleFPS = uiGameplay . uiTiming . uiShowFPS %= not

-- ----------------------------------------------
--                 HELPER UTILS
-- ----------------------------------------------

isRunning :: EventM Name ScenarioState Bool
isRunning = do
  mt <- preuse $ uiGameplay . uiDialogs . uiModal . _Just . modalType
  return $ maybe True isRunningModal mt

whenRunningAppState :: EventM Name AppState () -> EventM Name AppState ()
whenRunningAppState a = Brick.zoom (playState . scenarioState) isRunning >>= \r -> when r a

whenRunningPlayState :: EventM Name ScenarioState () -> EventM Name ScenarioState ()
whenRunningPlayState a = isRunning >>= \r -> when r a

whenDebug :: DebugOption -> EventM Name AppState () -> EventM Name AppState ()
whenDebug d a = do
  debug <- use $ uiState . uiDebugOptions . contains d
  when debug a
