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
  swarmEvents,
  defaultSwarmBindings,
) where

import Brick.Keybindings
import Data.Bifunctor (first)
import Data.Text (Text)
import Graphics.Vty qualified as V

data SwarmEvent
  = Main MainEvent
  | REPL REPLEvent
  deriving (Eq, Ord, Show)

swarmEvents :: KeyEvents SwarmEvent
swarmEvents = keyEvents (embed Main mainEvents ++ embed REPL replEvents)

defaultSwarmBindings :: [(SwarmEvent, [Binding])]
defaultSwarmBindings = embedB Main defaultMainBindings ++ embedB REPL defaultReplBindings
 where
  embedB f = map (first f)

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
  deriving (Eq, Ord, Show, Enum)

mainEvents :: KeyEvents MainEvent
mainEvents =
  keyEvents
    [ ("quit", QuitEvent)
    , ("view goal", ViewGoalEvent)
    ]

defaultMainBindings :: [(MainEvent, [Binding])]
defaultMainBindings =
  [ (QuitEvent, [ctrl 'q'])
  , (ViewHelpEvent, [fn 1])
  , (ViewRobotsEvent, [fn 2])
  , (ViewRecipesEvent, [fn 3])
  , (ViewCommandsEvent, [fn 4])
  , (ViewMessagesEvent, [fn 5])
  , (ViewStructuresEvent, [fn 6])
  , (ViewGoalEvent, [ctrl 'g'])
  , (HideRobotsEvent, [meta 'h'])
  , (ShowCESKDebugEvent, [meta 'd'])
  ]

data REPLEvent
  = CancelRunningProgramEvent
  | TogglePilotingModeEvent
  | ToggleCustomKeyHandlingEvent
  deriving (Eq, Ord, Show, Enum)

replEvents :: KeyEvents REPLEvent
replEvents =
  keyEvents
    [ ("cancel running program", CancelRunningProgramEvent)
    , ("toggle custom key handling", ToggleCustomKeyHandlingEvent)
    , ("toggle piloting mode", TogglePilotingModeEvent)
    ]

defaultReplBindings :: [(REPLEvent, [Binding])]
defaultReplBindings =
  [ (CancelRunningProgramEvent, [ctrl 'c', bind V.KEsc])
  , (TogglePilotingModeEvent, [meta 'p'])
  , (ToggleCustomKeyHandlingEvent, [meta 'k'])
  ]

-- ----------------

-- * Helper methods

embed :: Ord b => (a -> b) -> KeyEvents a -> [(Text, b)]
embed f = map (fmap f) . keyEventsList
