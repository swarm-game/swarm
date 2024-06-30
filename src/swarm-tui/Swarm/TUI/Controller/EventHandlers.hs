{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Sum types representing the Swarm events
-- abstracted away from keybindings.
module Swarm.TUI.Controller.EventHandlers (
  -- * Documentation
  createEventHandlers,

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

-- | Create event handlers with given key config.
--
-- Fails if any key events have conflict within one dispatcher.
createEventHandlers ::
  (Has (Throw SystemFailure) sig m) =>
  KeyConfig SwarmEvent ->
  m SwarmKeyDispatchers
createEventHandlers config = do
  mainGameDispatcher <- buildDispatcher mainEventHandlers
  replDispatcher <- buildDispatcher replEventHandlers
  worldDispatcher <- buildDispatcher worldEventHandlers
  robotDispatcher <- buildDispatcher robotEventHandlers
  return SwarmKeyDispatchers {..}
 where
  -- this error handling code is modified version of the brick demo app:
  -- https://github.com/jtdaugherty/brick/blob/764e66897/programs/CustomKeybindingDemo.hs#L216
  buildDispatcher handlers = case keyDispatcher config handlers of
    Right d -> return d
    Left collisions -> do
      let errorHeader = "Error: some key events have the same keys bound to them.\n"
      let handlerErrors = flip map collisions $ \(b, hs) ->
            let hsm = "Handlers with the '" <> BK.ppBinding b <> "' binding:"
                hss = flip map hs $ \h ->
                  let trigger = case BK.kehEventTrigger $ BK.khHandler h of
                        ByKey k -> "triggered by the key '" <> BK.ppBinding k <> "'"
                        ByEvent e -> "triggered by the event '" <> fromMaybe "<unknown>" (BK.keyEventName swarmEvents e) <> "'"
                      desc = BK.handlerDescription $ BK.kehHandler $ BK.khHandler h
                   in "  " <> desc <> " (" <> trigger <> ")"
             in T.intercalate "\n" (hsm : hss)
      throwError $ CustomFailure (T.intercalate "\n" $ errorHeader : handlerErrors)

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
