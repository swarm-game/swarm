{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Here is the REPL player configurable key event handler.
module Swarm.TUI.Controller.EventHandlers.REPL (
  replEventHandlers,
) where

import Brick
import Brick.Keybindings qualified as B
import Control.Lens as Lens
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Swarm.Game.CESK (cancel)
import Swarm.Game.Robot.Concrete
import Swarm.Game.State
import Swarm.Game.State.Substate
import Swarm.TUI.Controller.Util
import Swarm.TUI.Model
import Swarm.TUI.Model.Event
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.UI.Gameplay

-- | Handle a user input key event for the REPL.
--
-- See 'Swarm.TUI.Controller.handleREPLEvent'.
replEventHandlers :: [B.KeyEventHandler SwarmEvent (EventM Name AppState)]
replEventHandlers = allHandlers REPL $ \case
  CancelRunningProgramEvent -> ("Cancel running base robot program", cancelRunningBase)
  TogglePilotingModeEvent -> ("Toggle piloting mode", onlyCreative togglePilotingMode)
  ToggleCustomKeyHandlingEvent -> ("Toggle custom key handling mode", toggleCustomKeyHandling)

-- | Cancel the running base CESK machine and clear REPL input text.
--
-- It is handled in top REPL handler so we can always cancel the currently running
-- base program no matter what REPL control mode we are in.
cancelRunningBase :: EventM Name AppState ()
cancelRunningBase = do
  working <- use $ playState . gameState . gameControls . replWorking
  when working $ playState . gameState . baseRobot . machine %= cancel
  Brick.zoom (playState . uiGameplay . uiREPL) $ do
    replPromptType .= CmdPrompt []
    replPromptText .= ""

togglePilotingMode :: EventM Name AppState ()
togglePilotingMode = do
  s <- get
  let theRepl = s ^. playState . uiGameplay . uiREPL
      uinput = theRepl ^. replPromptText
      curMode = theRepl ^. replControlMode
  case curMode of
    Piloting -> playState . uiGameplay . uiREPL . replControlMode .= Typing
    _ ->
      if T.null uinput
        then playState . uiGameplay . uiREPL . replControlMode .= Piloting
        else do
          addREPLHistItem $ mkREPLError "Please clear the REPL before engaging pilot mode."
          invalidateCacheEntry REPLHistoryCache

toggleCustomKeyHandling :: EventM Name AppState ()
toggleCustomKeyHandling = do
  s <- get
  when (isJust (s ^. playState . gameState . gameControls . inputHandler)) $ do
    curMode <- use $ playState . uiGameplay . uiREPL . replControlMode
    (playState . uiGameplay . uiREPL . replControlMode) .= case curMode of Handling -> Typing; _ -> Handling
