{-# LANGUAGE PatternSynonyms #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Controller.Util where

import Brick hiding (Direction)
import Brick.Focus
import Control.Lens
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as M
import Graphics.Vty qualified as V
import Swarm.Game.State
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
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

pattern BackspaceKey :: BrickEvent n e
pattern BackspaceKey = VtyEvent (V.EvKey V.KBS [])

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

immediatelyRedrawWorld :: EventM Name AppState ()
immediatelyRedrawWorld = do
  invalidateCacheEntry WorldCache
  loadVisibleRegion

-- | Make sure all tiles covering the visible part of the world are
--   loaded.
loadVisibleRegion :: EventM Name AppState ()
loadVisibleRegion = do
  mext <- lookupExtent WorldExtent
  forM_ mext $ \(Extent _ _ size) -> do
    gs <- use gameState
    let vr = viewingRegion gs (over both fromIntegral size)
    gameState . multiWorld %= M.adjust (W.loadRegion (vr ^. planar)) (vr ^. subworld)

mouseLocToWorldCoords :: Brick.Location -> EventM Name GameState (Maybe (Cosmic W.Coords))
mouseLocToWorldCoords (Brick.Location mouseLoc) = do
  mext <- lookupExtent WorldExtent
  case mext of
    Nothing -> pure Nothing
    Just ext -> do
      region <- gets $ flip viewingRegion (bimap fromIntegral fromIntegral (extentSize ext))
      let regionStart = W.unCoords (fst $ region ^. planar)
          mouseLoc' = bimap fromIntegral fromIntegral mouseLoc
          mx = snd mouseLoc' + fst regionStart
          my = fst mouseLoc' + snd regionStart
       in pure . Just $ Cosmic (region ^. subworld) $ W.Coords (mx, my)
