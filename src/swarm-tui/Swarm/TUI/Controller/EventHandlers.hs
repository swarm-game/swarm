{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm (abstract) event handlers allow players to customize some keybindings.
-- This all comes together in 'Swarm.TUI.Controller' which calls the handlers
-- for parts of UI and also handles mouse events and frame updates.
module Swarm.TUI.Controller.EventHandlers (
  -- * Documentation
  createKeyDispatchers,
  allEventHandlers,

  -- ** Main game handler
  mainEventHandlers,

  -- ** REPL panel handler
  replEventHandlers,

  -- ** World panel handler
  worldEventHandlers,

  -- ** Robot panel handler
  robotEventHandlers,
  handleRobotPanelEvent,

  -- ** Frame
  runFrameUI,
  runGameTickUI,
  ticksPerFrameCap,
) where

import Brick hiding (on)
import Brick.Keybindings as BK
import Control.Effect.Accum
import Control.Effect.Throw
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import Swarm.Game.Failure (SystemFailure (..))
import Swarm.TUI.Controller.EventHandlers.Frame (runFrameUI, runGameTickUI, ticksPerFrameCap)
import Swarm.TUI.Controller.EventHandlers.Main (mainEventHandlers)
import Swarm.TUI.Controller.EventHandlers.REPL (replEventHandlers)
import Swarm.TUI.Controller.EventHandlers.Robot (handleRobotPanelEvent, robotEventHandlers)
import Swarm.TUI.Controller.EventHandlers.World (worldEventHandlers)
import Swarm.TUI.Model
import Swarm.TUI.Model.Event (SwarmEvent, swarmEvents)

-- ~~~~ Note [how Swarm event handlers work]
--
-- Allowing players to customize keybindings requires storing the configuration in AppState.
-- By doing it as declaratively as possible, Brick also allows us to detect conflicts.
--
-- The high level overview is this:
-- 1. The 'SwarmEvent' is a enumeration of abstracts key events ('PauseEvent', etc.)
-- 2. The 'AppState' definition contains the key configuration and dispatchers (keys to handlers)
-- 3. Here in 'Swarm.TUI.Controller.EventHandlers' we declare the handlers for abstract events
--    and also some non-customizable key handlers (e.g. escape and enter).
-- 4. When provided with 'KeyConfig' (can include customized keybindings) we can
--    'createKeyDispatchers' in 'Swarm.TUI.Model.StateUpdate' and store them in 'AppState'.
-- 5. Finally in 'Swarm.TUI.Controller' the Brick event handler calls the stored dispatchers.

-- | Create key dispatchers that call (abstract) event handlers based on given key config.
--
-- Fails if any key events have conflict within one dispatcher or when a main dispatcher
-- has conflict with one of the subdispatchers.
createKeyDispatchers ::
  (Has (Throw SystemFailure) sig m) =>
  KeyConfig SwarmEvent ->
  m SwarmKeyDispatchers
createKeyDispatchers config = do
  mainGameDispatcher <- buildDispatcher mainEventHandlers
  let buildSubMainDispatcher = buildSubDispatcher "Main game events" mainGameDispatcher
  replDispatcher <- buildSubMainDispatcher "REPL panel events" replEventHandlers
  worldDispatcher <- buildSubMainDispatcher "World view panel events" worldEventHandlers
  robotDispatcher <- buildSubMainDispatcher "Robot inventory panel events" robotEventHandlers
  return SwarmKeyDispatchers {..}
 where
  -- this error handling code is modified version of the brick demo app:
  -- https://github.com/jtdaugherty/brick/blob/764e66897/programs/CustomKeybindingDemo.hs#L216
  buildDispatcher handlers = case keyDispatcher config handlers of
    Left collisions ->
      throwLoadingFailure $
        "Error: some key events have the same keys bound to them.\n"
          : handlerErrors collisions
    Right d -> return d
  buildSubDispatcher parentName parentDispatcher name handlers = do
    d <- buildDispatcher handlers
    let collisions = conflicts parentDispatcher d
    if null collisions
      then return d
      else
        throwLoadingFailure $
          ("Error: some key events have keys bound to them in '" <> parentName <> "' and in '" <> name <> "'")
            : handlerErrors collisions

  throwLoadingFailure = throwError . CustomFailure . T.intercalate "\n"
  handlerErrors collisions = flip map collisions $ \(b, hs) ->
    let hsm = "Handlers with the '" <> BK.ppBinding b <> "' binding:"
        hss = flip map hs $ \h ->
          let trigger = case BK.kehEventTrigger $ BK.khHandler h of
                ByKey k -> "triggered by the key '" <> BK.ppBinding k <> "'"
                ByEvent e -> "triggered by the event '" <> fromMaybe "<unknown>" (BK.keyEventName swarmEvents e) <> "'"
              desc = BK.handlerDescription $ BK.kehHandler $ BK.khHandler h
           in "  " <> desc <> " (" <> trigger <> ")"
     in T.intercalate "\n" (hsm : hss)

-- | Take two dispatchers (that do not have conflict themselves) and find conflicting keys between them.
conflicts :: SwarmKeyDispatcher -> SwarmKeyDispatcher -> [(Binding, [KeyHandler SwarmEvent (EventM Name AppState)])]
conflicts d1 d2 = combine <$> badGroups
 where
  l1 = keyDispatcherToList d1
  l2 = keyDispatcherToList d2
  gs = groupBy ((==) `on` fst) $ sortBy (compare `on` fst) (l1 <> l2)
  badGroups = filter ((1 <) . length) $ mapMaybe NE.nonEmpty gs
  combine :: NE.NonEmpty (Binding, KeyHandler k m) -> (Binding, [KeyHandler k m])
  combine as =
    let b = fst $ NE.head as
     in (b, snd <$> NE.toList as)

allEventHandlers :: [KeyEventHandler SwarmEvent (EventM Name AppState)]
allEventHandlers =
  concat
    [ mainEventHandlers
    , replEventHandlers
    , worldEventHandlers
    , robotEventHandlers
    ]
