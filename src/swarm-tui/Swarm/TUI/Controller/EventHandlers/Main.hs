{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Here is the main player configurable key event handler while playing the game.
module Swarm.TUI.Controller.EventHandlers.Main (
  mainEventHandlers,
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
    , do
        m <- use $ uiState . uiMenu
        Brick.zoom playState $ toggleModal m HelpModal
    )
  ViewRobotsEvent ->
    ( "View Robots screen"
    , do
        m <- use $ uiState . uiMenu
        Brick.zoom playState $ toggleModal m RobotsModal
    )
  ViewRecipesEvent ->
    ( "View Recipes screen"
    , do
        m <- use $ uiState . uiMenu
        Brick.zoom playState $ toggleDiscoveryNotificationModal m RecipesModal availableRecipes
    )
  ViewCommandsEvent ->
    ( "View Commands screen"
    , do
        m <- use $ uiState . uiMenu
        Brick.zoom playState $ toggleDiscoveryNotificationModal m CommandsModal availableCommands
    )
  ViewMessagesEvent ->
    ( "View Messages screen"
    , do
        m <- use $ uiState . uiMenu
        Brick.zoom playState $ toggleMessagesModal m
    )
  ViewStructuresEvent ->
    ( "View Structures screen"
    , do
        m <- use $ uiState . uiMenu
        Brick.zoom playState $ toggleStructuresModal m StructuresModal (recognizerAutomatons . originalStructureDefinitions)
    )
  ViewGoalEvent ->
    ( "View scenario goal description"
    , do
        m <- use $ uiState . uiMenu
        Brick.zoom playState $ viewGoal m
    )
  HideRobotsEvent -> ("Hide robots for a few ticks", hideRobots)
  ShowCESKDebugEvent -> ("Show active robot CESK machine debugging line", showCESKDebug)
  PauseEvent -> ("Pause or unpause the game", whenRunning $ Brick.zoom playState safeTogglePause)
  RunSingleTickEvent -> ("Run game for a single tick", whenRunning runSingleTick)
  IncreaseTpsEvent -> ("Double game speed", whenRunning . modify $ adjustTPS (+))
  DecreaseTpsEvent -> ("Halve game speed", whenRunning . modify $ adjustTPS (-))
  FocusWorldEvent -> ("Set focus on the World panel", Brick.zoom playState $ setFocus WorldPanel)
  FocusRobotEvent -> ("Set focus on the Robot panel", Brick.zoom playState $ setFocus RobotPanel)
  FocusREPLEvent -> ("Set focus on the REPL panel", Brick.zoom playState $ setFocus REPLPanel)
  FocusInfoEvent -> ("Set focus on the Info panel", Brick.zoom playState $ setFocus InfoPanel)
  ToggleCreativeModeEvent -> ("Toggle creative mode", whenDebug ToggleCreative $ Brick.zoom playState toggleCreativeMode)
  ToggleWorldEditorEvent -> ("Toggle world editor mode", whenDebug ToggleWorldEditor $ Brick.zoom playState toggleWorldEditor)
  ToggleREPLVisibilityEvent -> ("Collapse/Expand REPL panel", toggleREPLVisibility)
  ViewBaseEvent -> ("View the base robot", Brick.zoom playState viewBase)
  ToggleFPSEvent -> ("Toggle the FPS display", Brick.zoom playState toggleFPS)

toggleQuitGameDialog :: EventM Name AppState ()
toggleQuitGameDialog = do
  s <- get
  m <- use $ uiState . uiMenu
  Brick.zoom playState $ case s ^. playState . gameState . winCondition of
    WinConditions (Won _ _) _ -> toggleModal m $ ScenarioEndModal WinModal
    WinConditions (Unwinnable _) _ -> toggleModal m $ ScenarioEndModal LoseModal
    _ -> toggleModal m QuitModal

toggleGameModal ::
  Foldable t =>
  Menu ->
  ModalType ->
  Getter GameState (t a) ->
  EventM Name PlayState Bool
toggleGameModal menu m l = do
  s <- get
  let nothingToShow = null $ s ^. gameState . l

  unless nothingToShow $
    toggleModal menu m
  return nothingToShow

toggleStructuresModal ::
  Foldable t =>
  Menu ->
  ModalType ->
  Lens' Landscape (t a) ->
  EventM Name PlayState ()
toggleStructuresModal menu m l = void $ toggleGameModal menu m (landscape . l)

toggleDiscoveryNotificationModal ::
  Menu ->
  ModalType ->
  Lens' Discovery (Notifications a) ->
  EventM Name PlayState ()
toggleDiscoveryNotificationModal menu m l = do
  nothingToShow <- toggleGameModal menu m (discovery . l . notificationsContent)
  unless nothingToShow $ gameState . discovery . l . notificationsCount .= 0

toggleMessagesModal :: Menu -> EventM Name PlayState ()
toggleMessagesModal menu = do
  s <- get
  nothingToShow <- toggleGameModal menu MessagesModal (messageNotifications . notificationsContent)
  unless nothingToShow $ gameState . messageInfo . lastSeenMessageTime .= s ^. gameState . temporal . ticks

viewGoal :: Menu -> EventM Name PlayState ()
viewGoal m = do
  s <- get
  if hasAnythingToShow $ s ^. uiGameplay . uiDialogs . uiGoal . goalsContent
    then toggleModal m GoalModal
    else continueWithoutRedraw

hideRobots :: EventM Name AppState ()
hideRobots = Brick.zoom (playState . uiGameplay) $ do
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
  let isPaused = s ^. playState . gameState . temporal . paused
  let isCreative = s ^. playState . gameState . creativeMode
  let hasDebug = hasDebugCapability isCreative s
  when (isPaused && hasDebug) $ do
    debug <- playState . uiGameplay . uiShowDebug Lens.<%= not
    if debug
      then playState . gameState . temporal . gameStep .= RobotStep SBefore
      else zoomGameState finishGameTick >> void updateUI

runSingleTick :: EventM Name AppState ()
runSingleTick = do
  playState . gameState . temporal . runStatus .= ManualPause
  runGameTickUI

-- | Adjust the ticks per second speed.
adjustTPS :: (Int -> Int -> Int) -> AppState -> AppState
adjustTPS (+/-) = playState . uiGameplay . uiTiming . lgTicksPerSecond %~ (+/- 1)

toggleCreativeMode :: EventM Name PlayState ()
toggleCreativeMode = gameState . creativeMode %= not

toggleWorldEditor :: EventM Name PlayState ()
toggleWorldEditor = do
  uiGameplay . uiWorldEditor . worldOverdraw . isWorldEditorEnabled %= not
  setFocus WorldEditorPanel

toggleREPLVisibility :: EventM Name AppState ()
toggleREPLVisibility = do
  invalidateCacheEntry WorldCache
  playState . uiGameplay . uiShowREPL %= not

viewBase :: EventM Name PlayState ()
viewBase = do
  invalidateCacheEntry WorldCache
  gameState . robotInfo . viewCenterRule .= VCRobot 0

toggleFPS :: EventM Name PlayState ()
toggleFPS = uiGameplay . uiTiming . uiShowFPS %= not

-- ----------------------------------------------
--                 HELPER UTILS
-- ----------------------------------------------

isRunning :: EventM Name AppState Bool
isRunning = do
  mt <- preuse $ playState . uiGameplay . uiDialogs . uiModal . _Just . modalType
  return $ maybe True isRunningModal mt

whenRunning :: EventM Name AppState () -> EventM Name AppState ()
whenRunning a = isRunning >>= \r -> when r a

whenDebug :: DebugOption -> EventM Name AppState () -> EventM Name AppState ()
whenDebug d a = do
  debug <- use $ uiState . uiDebugOptions . contains d
  when debug a
