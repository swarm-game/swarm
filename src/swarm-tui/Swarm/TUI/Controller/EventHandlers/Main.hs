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
import Swarm.Game.State.Substate
import Swarm.Game.Step (finishGameTick)
import Swarm.Language.Syntax (Phase (..))
import Swarm.TUI.Controller.EventHandlers.Frame (runGameTickUI)
import Swarm.TUI.Controller.UpdateUI (updateUI)
import Swarm.TUI.Controller.Util
import Swarm.TUI.Editor.Model (isWorldEditorEnabled, worldOverdraw)
import Swarm.TUI.Model
import Swarm.TUI.Model.DebugOption (DebugOption (ToggleCreative, ToggleWorldEditor))
import Swarm.TUI.Model.Dialog.Goal
import Swarm.TUI.Model.Event (MainEvent (..), SwarmEvent (..))
import Swarm.TUI.Model.Menu
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
    , Brick.zoom (playState . scenarioState) $ toggleMidScenarioModal HelpModal
    )
  ViewRobotsEvent ->
    ( "View Robots screen"
    , Brick.zoom (playState . scenarioState) $ toggleMidScenarioModal RobotsModal
    )
  ViewRecipesEvent ->
    ( "View Recipes screen"
    , Brick.zoom (playState . scenarioState) $ toggleDiscoveryNotificationModal RecipesModal availableRecipes
    )
  ViewCommandsEvent ->
    ( "View Commands screen"
    , Brick.zoom (playState . scenarioState) $ toggleDiscoveryNotificationModal CommandsModal availableCommands
    )
  ViewMessagesEvent ->
    ( "View Messages screen"
    , Brick.zoom (playState . scenarioState) toggleMessagesModal
    )
  ViewStructuresEvent ->
    ( "View Structures screen"
    , Brick.zoom (playState . scenarioState) $
        toggleStructuresModal (recognizerAutomatons . originalStructureDefinitions)
    )
  ViewGoalEvent ->
    ( "View scenario goal description"
    , Brick.zoom (playState . scenarioState) viewGoal
    )
  HideRobotsEvent -> ("Hide robots for a few ticks", Brick.zoom (playState . scenarioState) hideRobots)
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
        WinConditions (Won _ _) _ -> ScenarioFinishModal WinModal
        WinConditions (Unwinnable _) _ -> ScenarioFinishModal LoseModal
        _ -> QuitModal
  Brick.zoom playState $ toggleEndScenarioModal whichModal $ s ^. uiState . uiMenu

toggleGameModal ::
  Foldable t =>
  MidScenarioModalType ->
  Getter GameState (t a) ->
  EventM Name ScenarioState Bool
toggleGameModal m l = do
  s <- get
  let nothingToShow = null $ s ^. gameState . l

  unless nothingToShow $
    toggleMidScenarioModal m
  return nothingToShow

toggleStructuresModal ::
  Foldable t =>
  Lens' (Landscape Typed) (t a) ->
  EventM Name ScenarioState ()
toggleStructuresModal l = void $ toggleGameModal StructuresModal (landscape . l)

toggleDiscoveryNotificationModal ::
  MidScenarioModalType ->
  Lens' Discovery (Notifications a) ->
  EventM Name ScenarioState ()
toggleDiscoveryNotificationModal mt l = do
  nothingToShow <- toggleGameModal mt $ discovery . l . notificationsContent
  unless nothingToShow $ gameState . discovery . l . notificationsCount .= 0

toggleMessagesModal :: EventM Name ScenarioState ()
toggleMessagesModal = do
  s <- get
  nothingToShow <- toggleGameModal MessagesModal $ messageNotifications . notificationsContent
  unless nothingToShow $ gameState . messageInfo . lastSeenMessageTime .= s ^. gameState . temporal . ticks

viewGoal :: EventM Name ScenarioState ()
viewGoal = do
  s <- get
  if hasAnythingToShow $ s ^. uiGameplay . uiDialogs . uiGoal . goalsContent
    then toggleMidScenarioModal GoalModal
    else continueWithoutRedraw

hideRobots :: EventM Name ScenarioState ()
hideRobots = do
  t <- liftIO $ getTime Monotonic
  uiGameplay . uiHideRobotsUntil .= Just (t + TimeSpec 2 0)
  gameState . redraw %= redrawWorld

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
toggleCreativeMode = do
  gameState . creativeMode %= not
  gameState . redraw %= redrawWorld

toggleWorldEditor :: EventM Name ScenarioState ()
toggleWorldEditor = do
  uiGameplay . uiWorldEditor . worldOverdraw . isWorldEditorEnabled %= not
  setFocus WorldEditorPanel

toggleREPLVisibility :: EventM Name ScenarioState ()
toggleREPLVisibility = uiGameplay . uiShowREPL %= not

viewBase :: EventM Name ScenarioState ()
viewBase = gameState . robotInfo . viewCenterRule .= VCRobot 0

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
