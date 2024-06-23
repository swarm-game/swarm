{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- TODO: describe
module Swarm.TUI.Controller.REPLEventHandler (
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
import Swarm.TUI.Model.UI

replEventHandlers :: [B.KeyEventHandler SwarmEvent (EventM Name AppState)]
replEventHandlers =
  [ B.onEvent (REPL CancelRunningProgramEvent) "Cancel running base robot program" $ do
      -- Handled here so we can always cancel the currently running
      -- base program no matter what REPL control mode we are in.
      working <- use $ gameState . gameControls . replWorking
      when working $ gameState . baseRobot . machine %= cancel
      Brick.zoom (uiState . uiGameplay . uiREPL) $ do
        replPromptType .= CmdPrompt []
        replPromptText .= ""
  , B.onEvent (REPL TogglePilotingModeEvent) "Toggle piloting mode" . onlyCreative $ do
      s <- get
      let theRepl = s ^. uiState . uiGameplay . uiREPL
          uinput = theRepl ^. replPromptText
      curMode <- use $ uiState . uiGameplay . uiREPL . replControlMode
      case curMode of
        Piloting -> uiState . uiGameplay . uiREPL . replControlMode .= Typing
        _ ->
          if T.null uinput
            then uiState . uiGameplay . uiREPL . replControlMode .= Piloting
            else do
              let err = REPLError "Please clear the REPL before engaging pilot mode."
              uiState . uiGameplay . uiREPL . replHistory %= addREPLItem err
              invalidateCacheEntry REPLHistoryCache
  , B.onEvent (REPL ToggleCustomKeyHandlingEvent) "Toggle custom key handling mode" $ do
      s <- get
      when (isJust (s ^. gameState . gameControls . inputHandler)) $ do
        curMode <- use $ uiState . uiGameplay . uiREPL . replControlMode
        (uiState . uiGameplay . uiREPL . replControlMode) .= case curMode of Handling -> Typing; _ -> Handling
  ]
