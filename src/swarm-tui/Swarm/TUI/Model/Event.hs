{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Sum types representing the Swarm events
-- abstracted away from keybindings.
module Swarm.TUI.Model.Event (
  SwarmEvent (..),
  MainEvent (..),
  REPLEvent (..),
  WorldEvent (..),
  RobotEvent (..),
  swarmEvents,
  defaultSwarmBindings,
) where

import Brick.Keybindings
import Control.Arrow ((&&&))
import Data.Bifunctor (first)
import Data.List.Extra (enumerate)
import Data.Text (Text)
import GHC.Generics (Generic)
import Generic.Data (FiniteEnumeration (..))
import Graphics.Vty qualified as V
import Swarm.Language.Syntax.Direction (AbsoluteDir (..), Direction (..), directionSyntax)

-- | Swarm named TUI event type.
--
-- See Note [how Swarm event handlers work]
data SwarmEvent
  = Main MainEvent
  | REPL REPLEvent
  | World WorldEvent
  | Robot RobotEvent
  deriving (Eq, Ord, Show, Generic)
  deriving (Enum, Bounded) via (FiniteEnumeration SwarmEvent)

swarmEvents :: KeyEvents SwarmEvent
swarmEvents =
  keyEvents $
    concat
      [ embed Main mainEvents
      , embed REPL replEvents
      , embed World worldPanelEvents
      , embed Robot robotPanelEvents
      ]
 where
  embed f = map (fmap f) . keyEventsList

defaultSwarmBindings :: [(SwarmEvent, [Binding])]
defaultSwarmBindings =
  concat
    [ embed Main defaultMainBindings
    , embed REPL defaultReplBindings
    , embed World defaultWorldPanelBindings
    , embed Robot defaultRobotPanelBindings
    ]
 where
  embed = map . first

-- ----------------------------------------------
--                 MAIN EVENTS
-- ----------------------------------------------

-- | Main abstract keybinding events while running the game itself.
--
-- See 'Swarm.TUI.Controller.MainEventHandler.'.
data MainEvent
  = QuitEvent
  | ViewHelpEvent
  | ViewRobotsEvent
  | ViewRecipesEvent
  | ViewCommandsEvent
  | ViewMessagesEvent
  | ViewStructuresEvent
  | ViewGoalEvent
  | HideRobotsEvent
  | ShowCESKDebugEvent
  | PauseEvent
  | RunSingleTickEvent
  | IncreaseTpsEvent
  | DecreaseTpsEvent
  | FocusWorldEvent
  | FocusRobotEvent
  | FocusREPLEvent
  | FocusInfoEvent
  | ToggleCreativeModeEvent
  | ToggleWorldEditorEvent
  | ToggleREPLVisibilityEvent
  | ViewBaseEvent
  | ToggleFPSEvent
  deriving (Eq, Ord, Show, Enum, Bounded)

mainEvents :: KeyEvents MainEvent
mainEvents = allKeyEvents $ \case
  QuitEvent -> "quit"
  ViewHelpEvent -> "view help"
  ViewRobotsEvent -> "view robots"
  ViewRecipesEvent -> "view recipes"
  ViewCommandsEvent -> "view commands"
  ViewMessagesEvent -> "view messages"
  ViewStructuresEvent -> "view structures"
  ViewGoalEvent -> "view goal"
  HideRobotsEvent -> "hide robots"
  ShowCESKDebugEvent -> "debug CESK"
  PauseEvent -> "pause"
  RunSingleTickEvent -> "run single tick"
  IncreaseTpsEvent -> "increse TPS"
  DecreaseTpsEvent -> "decrease TPS"
  FocusWorldEvent -> "focus World"
  FocusRobotEvent -> "focus Robot"
  FocusREPLEvent -> "focus REPL"
  FocusInfoEvent -> "focus Info"
  ToggleCreativeModeEvent -> "creative mode"
  ToggleWorldEditorEvent -> "world editor"
  ToggleREPLVisibilityEvent -> "toggle REPL"
  ViewBaseEvent -> "view base robot"
  ToggleFPSEvent -> "toggle FPS"

defaultMainBindings :: [(MainEvent, [Binding])]
defaultMainBindings = allBindings $ \case
  QuitEvent -> [ctrl 'q']
  ViewHelpEvent -> [fn 1]
  ViewRobotsEvent -> [fn 2]
  ViewRecipesEvent -> [fn 3]
  ViewCommandsEvent -> [fn 4]
  ViewMessagesEvent -> [fn 5]
  ViewStructuresEvent -> [fn 6]
  ViewGoalEvent -> [ctrl 'g']
  HideRobotsEvent -> [meta 'h']
  ShowCESKDebugEvent -> [meta 'd']
  PauseEvent -> [ctrl 'p']
  RunSingleTickEvent -> [ctrl 'o']
  IncreaseTpsEvent -> [ctrl 'x']
  DecreaseTpsEvent -> [ctrl 'z']
  FocusWorldEvent -> [meta 'w']
  FocusRobotEvent -> [meta 'e']
  FocusREPLEvent -> [meta 'r']
  FocusInfoEvent -> [meta 't']
  ToggleCreativeModeEvent -> [ctrl 'v']
  ToggleWorldEditorEvent -> []
  ToggleREPLVisibilityEvent -> [meta ',']
  ViewBaseEvent -> [meta 'c']
  ToggleFPSEvent -> [meta 'f']

-- ----------------------------------------------
--                 REPL EVENTS
-- ----------------------------------------------

-- | REPL abstract keybinding events.
--
-- See 'Swarm.TUI.Controller.REPLEventHandler'.
data REPLEvent
  = CancelRunningProgramEvent
  | TogglePilotingModeEvent
  | ToggleCustomKeyHandlingEvent
  deriving (Eq, Ord, Show, Enum, Bounded)

replEvents :: KeyEvents REPLEvent
replEvents = allKeyEvents $ \case
  CancelRunningProgramEvent -> "cancel running program"
  ToggleCustomKeyHandlingEvent -> "toggle custom key handling"
  TogglePilotingModeEvent -> "toggle piloting mode"

defaultReplBindings :: [(REPLEvent, [Binding])]
defaultReplBindings = allBindings $ \case
  CancelRunningProgramEvent -> [ctrl 'c', bind V.KEsc]
  TogglePilotingModeEvent -> [meta 'p']
  ToggleCustomKeyHandlingEvent -> [meta 'k']

-- ----------------------------------------------
--                 REPL EVENTS
-- ----------------------------------------------

newtype WorldEvent
  = MoveViewEvent AbsoluteDir
  deriving (Eq, Ord, Show, Generic)
  deriving (Enum, Bounded) via (FiniteEnumeration WorldEvent)

worldPanelEvents :: KeyEvents WorldEvent
worldPanelEvents = allKeyEvents $ \case
  MoveViewEvent d -> "move view " <> directionSyntax (DAbsolute d)

defaultWorldPanelBindings :: [(WorldEvent, [Binding])]
defaultWorldPanelBindings = allBindings $ \case
  MoveViewEvent DWest -> [bind V.KLeft, bind 'h']
  MoveViewEvent DSouth -> [bind V.KDown, bind 'j']
  MoveViewEvent DNorth -> [bind V.KUp, bind 'k']
  MoveViewEvent DEast -> [bind V.KRight, bind 'l']

-- ----------------------------------------------
--                 ROBOT EVENTS
-- ----------------------------------------------

data RobotEvent
  = MakeEntityEvent
  | ShowZeroInventoryEntitiesEvent
  | CycleInventorySortEvent
  | SwitchInventorySortDirection
  | SearchInventoryEvent
  deriving (Eq, Ord, Show, Enum, Bounded)

robotPanelEvents :: KeyEvents RobotEvent
robotPanelEvents = allKeyEvents $ \case
  MakeEntityEvent -> "make entity"
  ShowZeroInventoryEntitiesEvent -> "show zero inventory entities"
  CycleInventorySortEvent -> "cycle inventory sort"
  SwitchInventorySortDirection -> "switch inventory direction"
  SearchInventoryEvent -> "search inventory"

defaultRobotPanelBindings :: [(RobotEvent, [Binding])]
defaultRobotPanelBindings = allBindings $ \case
  MakeEntityEvent -> [bind 'm']
  ShowZeroInventoryEntitiesEvent -> [bind '0']
  CycleInventorySortEvent -> [bind ';']
  SwitchInventorySortDirection -> [bind ':']
  SearchInventoryEvent -> [bind '/']

-- ----------------
-- Helper methods

allKeyEvents :: (Ord e, Bounded e, Enum e) => (e -> Text) -> KeyEvents e
allKeyEvents f = keyEvents $ map (f &&& id) enumerate

allBindings :: (Bounded e, Enum e) => (e -> [Binding]) -> [(e, [Binding])]
allBindings f = map (\e -> (e, f e)) enumerate
