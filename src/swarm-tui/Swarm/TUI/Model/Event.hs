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
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (sconcat)
import Data.Text (Text)
import Graphics.Vty qualified as V
import Swarm.Language.Syntax.Direction (AbsoluteDir (..), Direction (..), directionSyntax)
import Swarm.Util (enumerateNonEmpty)

-- See Note [how Swarm event handlers work]

data SwarmEvent
  = Main MainEvent
  | REPL REPLEvent
  | World WorldEvent
  | Robot RobotEvent
  deriving (Eq, Ord, Show)

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

defaultSwarmBindings :: NonEmpty (SwarmEvent, [Binding])
defaultSwarmBindings =
  sconcat $
    embed Main defaultMainBindings
      :| [ embed REPL defaultReplBindings
         , embed World defaultWorldPanelBindings
         , embed Robot defaultRobotPanelBindings
         ]
 where
  embed = NE.map . first

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

defaultMainBindings :: NonEmpty (MainEvent, [Binding])
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

defaultReplBindings :: NonEmpty (REPLEvent, [Binding])
defaultReplBindings = allBindings $ \case
  CancelRunningProgramEvent -> [ctrl 'c', bind V.KEsc]
  TogglePilotingModeEvent -> [meta 'p']
  ToggleCustomKeyHandlingEvent -> [meta 'k']

-- ----------------------------------------------
--                 REPL EVENTS
-- ----------------------------------------------

data WorldEvent
  = ViewBaseEvent
  | ShowFpsEvent
  | MoveViewEvent AbsoluteDir
  deriving (Eq, Ord, Show)

instance Enum WorldEvent where
  fromEnum = \case
    ViewBaseEvent -> 0
    ShowFpsEvent -> 1
    MoveViewEvent d -> 2 + fromEnum d
  toEnum = \case
    0 -> ViewBaseEvent
    1 -> ShowFpsEvent
    n -> MoveViewEvent . toEnum $ n - 2

instance Bounded WorldEvent where
  minBound = ViewBaseEvent
  maxBound = MoveViewEvent maxBound

worldPanelEvents :: KeyEvents WorldEvent
worldPanelEvents = allKeyEvents $ \case
  ViewBaseEvent -> "view base"
  ShowFpsEvent -> "show fps"
  MoveViewEvent d -> "move view " <> directionSyntax (DAbsolute d)

defaultWorldPanelBindings :: NonEmpty (WorldEvent, [Binding])
defaultWorldPanelBindings = allBindings $ \case
  ViewBaseEvent -> [bind 'c']
  ShowFpsEvent -> [bind 'f']
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

defaultRobotPanelBindings :: NonEmpty (RobotEvent, [Binding])
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

allBindings :: (Bounded e, Enum e) => (e -> [Binding]) -> NonEmpty (e, [Binding])
allBindings f = NE.map (\e -> (e, f e)) enumerateNonEmpty
