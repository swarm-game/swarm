{-# LANGUAGE PatternSynonyms #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Controller.Util where

import Brick hiding (Direction)
import Brick.Focus
import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty qualified as V
import Swarm.Game.State
import Swarm.TUI.Model
import Swarm.TUI.Model.UI
import Swarm.TUI.View.Util (generateModal)

-- | Pattern synonyms to simplify brick event handler
pattern Key :: V.Key -> BrickEvent n e
pattern Key k = VtyEvent (V.EvKey k [])

pattern CharKey, ControlChar, MetaChar :: Char -> BrickEvent n e
pattern CharKey c = VtyEvent (V.EvKey (V.KChar c) [])
pattern ControlChar c = VtyEvent (V.EvKey (V.KChar c) [V.MCtrl])
pattern MetaChar c = VtyEvent (V.EvKey (V.KChar c) [V.MMeta])

pattern ShiftKey :: V.Key -> BrickEvent n e
pattern ShiftKey k = VtyEvent (V.EvKey k [V.MShift])

pattern EscapeKey :: BrickEvent n e
pattern EscapeKey = VtyEvent (V.EvKey V.KEsc [])

pattern FKey :: Int -> BrickEvent n e
pattern FKey c = VtyEvent (V.EvKey (V.KFun c) [])

openModal :: ModalType -> EventM Name AppState ()
openModal mt = do
  newModal <- gets $ flip generateModal mt
  ensurePause
  uiState . uiModal ?= newModal
  -- Beep
  case mt of
    ScenarioEndModal _ -> do
      vty <- getVtyHandle
      liftIO $ V.ringTerminalBell $ V.outputIface vty
    _ -> return ()
 where
  -- Set the game to AutoPause if needed
  ensurePause = do
    pause <- use $ gameState . paused
    unless (pause || isRunningModal mt) $ do
      gameState . runStatus .= AutoPause

-- | The running modals do not autopause the game.
isRunningModal :: ModalType -> Bool
isRunningModal = \case
  RobotsModal -> True
  MessagesModal -> True
  _ -> False

setFocus :: FocusablePanel -> EventM Name AppState ()
setFocus name = uiState . uiFocusRing %= focusSetCurrent (FocusablePanel name)
