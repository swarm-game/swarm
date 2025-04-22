{-# LANGUAGE PatternSynonyms #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Keyboard key event patterns and drawing utilities
module Swarm.TUI.Controller.Util where

import Brick hiding (Direction)
import Brick.Focus
import Brick.Keybindings
import Control.Carrier.Lift qualified as Fused
import Control.Carrier.State.Lazy qualified as Fused
import Control.Lens as Lens
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO), liftIO)
import Control.Monad.State (MonadState, execState)
import Data.List.Extra (enumerate)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Graphics.Vty qualified as V
import Swarm.Effect (TimeIOC, runTimeIO)
import Swarm.Game.CESK (continue)
import Swarm.Game.Device
import Swarm.Game.Robot (robotCapabilities)
import Swarm.Game.Robot.Concrete
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Robot
import Swarm.Game.State.Substate
import Swarm.Game.Step (finishGameTick)
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.Game.World.Coords
import Swarm.Language.Capability (Capability (CDebug))
import Swarm.Language.Syntax hiding (Key)
import Swarm.TUI.Model (
  AppState,
  PlayState,
  ScenarioState,
  gameState,
  modalScroll,
  playState,
  scenarioState,
  uiGameplay,
 )
import Swarm.TUI.Model.Menu
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Repl (REPLHistItem, REPLPrompt, REPLState, addREPLItem, replHistory, replPromptText, replPromptType)
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.View.Util (generateModal, generateScenarioEndModal)
import System.Clock (Clock (..), getTime)

-- | Pattern synonyms to simplify brick event handler
pattern Key :: V.Key -> BrickEvent n e
pattern Key k = VtyEvent (V.EvKey k [])

pattern CharKey, ControlChar, MetaChar :: Char -> BrickEvent n e
pattern CharKey c = VtyEvent (V.EvKey (V.KChar c) [])
pattern ControlChar c = VtyEvent (V.EvKey (V.KChar c) [V.MCtrl])
pattern MetaChar c = VtyEvent (V.EvKey (V.KChar c) [V.MMeta])

pattern ShiftKey :: V.Key -> BrickEvent n e
pattern ShiftKey k = VtyEvent (V.EvKey k [V.MShift])

pattern MetaKey :: V.Key -> BrickEvent n e
pattern MetaKey k = VtyEvent (V.EvKey k [V.MMeta])

pattern EscapeKey :: BrickEvent n e
pattern EscapeKey = VtyEvent (V.EvKey V.KEsc [])

pattern BackspaceKey :: BrickEvent n e
pattern BackspaceKey = VtyEvent (V.EvKey V.KBS [])

pattern FKey :: Int -> BrickEvent n e
pattern FKey c = VtyEvent (V.EvKey (V.KFun c) [])

openEndScenarioModal :: Menu -> EndScenarioModalType -> EventM Name PlayState ()
openEndScenarioModal m mt = do
  resetViewport modalScroll
  newModal <- gets $ flip (generateScenarioEndModal m) mt
  Brick.zoom scenarioState ensurePause
  scenarioState . uiGameplay . uiDialogs . uiModal ?= newModal
  -- Beep
  case mt of
    ScenarioFinishModal _ -> do
      vty <- getVtyHandle
      liftIO $ V.ringTerminalBell $ V.outputIface vty
    _ -> return ()
 where
  -- Set the game to AutoPause if needed
  ensurePause = do
    pause <- use $ gameState . temporal . paused
    unless pause $ gameState . temporal . runStatus .= AutoPause

openMidScenarioModal :: MidScenarioModalType -> EventM Name ScenarioState ()
openMidScenarioModal mt = do
  resetViewport modalScroll
  newModal <- gets $ flip generateModal mt
  ensurePause
  uiGameplay . uiDialogs . uiModal ?= newModal
 where
  -- Set the game to AutoPause if needed
  ensurePause = do
    pause <- use $ gameState . temporal . paused
    unless (pause || isRunningModal (MidScenarioModal mt)) $ gameState . temporal . runStatus .= AutoPause

-- | The running modals do not autopause the game.
isRunningModal :: ModalType -> Bool
isRunningModal = \case
  MidScenarioModal RobotsModal -> True
  MidScenarioModal MessagesModal -> True
  _ -> False

-- | Set the game to Running if it was (auto) paused otherwise to paused.
--
-- Also resets the last frame time to now. If we are pausing, it
-- doesn't matter; if we are unpausing, this is critical to
-- ensure the next frame doesn't think it has to catch up from
-- whenever the game was paused!
safeTogglePause :: EventM Name ScenarioState ()
safeTogglePause = do
  curTime <- liftIO $ getTime Monotonic
  uiGameplay . uiTiming . lastFrameTime .= curTime
  uiGameplay . uiShowDebug .= False
  p <- gameState . temporal . runStatus Lens.<%= toggleRunStatus
  when (p == Running) $ zoomGameStateFromScenarioState finishGameTick

-- | Only unpause the game if leaving autopaused modal.
--
-- Note that the game could have been paused before opening
-- the modal, in that case, leave the game paused.
safeAutoUnpause :: EventM Name ScenarioState ()
safeAutoUnpause = do
  runs <- use $ gameState . temporal . runStatus
  when (runs == AutoPause) safeTogglePause

toggleMidScenarioModal :: MidScenarioModalType -> EventM Name ScenarioState ()
toggleMidScenarioModal mt = do
  modal <- use $ uiGameplay . uiDialogs . uiModal
  case modal of
    Nothing -> openMidScenarioModal mt
    Just _ -> uiGameplay . uiDialogs . uiModal .= Nothing >> safeAutoUnpause

toggleEndScenarioModal :: EndScenarioModalType -> Menu -> EventM Name PlayState ()
toggleEndScenarioModal mt m = do
  modal <- use $ scenarioState . uiGameplay . uiDialogs . uiModal
  case modal of
    Nothing -> openEndScenarioModal m mt
    Just _ -> scenarioState . uiGameplay . uiDialogs . uiModal .= Nothing >> Brick.zoom scenarioState safeAutoUnpause

setFocus :: FocusablePanel -> EventM Name ScenarioState ()
setFocus name = uiGameplay . uiFocusRing %= focusSetCurrent (FocusablePanel name)

immediatelyRedrawWorld :: EventM Name GameState ()
immediatelyRedrawWorld = do
  invalidateCacheEntry WorldCache
  loadVisibleRegion

-- | Make sure all tiles covering the visible part of the world are
--   loaded.
loadVisibleRegion :: EventM Name GameState ()
loadVisibleRegion = do
  mext <- lookupExtent WorldExtent
  forM_ mext $ \(Extent _ _ size) -> do
    vc <- use $ robotInfo . viewCenter
    let vr = viewingRegion vc (over both fromIntegral size)
    landscape . multiWorld %= M.adjust (W.loadRegion (vr ^. planar)) (vr ^. subworld)

mouseLocToWorldCoords :: Brick.Location -> EventM Name GameState (Maybe (Cosmic Coords))
mouseLocToWorldCoords (Brick.Location mouseLoc) = do
  mext <- lookupExtent WorldExtent
  forM mext $ \ext -> do
    region <- gets $ flip viewingRegion (bimap fromIntegral fromIntegral (extentSize ext)) . view (robotInfo . viewCenter)
    let regionStart = unCoords (fst $ region ^. planar)
        mouseLoc' = bimap fromIntegral fromIntegral mouseLoc
        mx = snd mouseLoc' + fst regionStart
        my = fst mouseLoc' + snd regionStart
     in pure $ Cosmic (region ^. subworld) $ Coords (mx, my)

hasDebugCapability :: Bool -> GameState -> Bool
hasDebugCapability isCreative s =
  maybe isCreative (S.member CDebug . getCapabilitySet) $
    s ^? to focusedRobot . _Just . robotCapabilities

-- | Resets the viewport scroll position
resetViewport :: ViewportScroll Name -> EventM Name s ()
resetViewport n = do
  vScrollToBeginning n
  hScrollToBeginning n

-- | Modifies the game state using a fused-effect state action.
zoomGameStateFromAppState ::
  (MonadState AppState m, MonadIO m) =>
  Fused.StateC GameState (TimeIOC (Fused.LiftC IO)) a ->
  m a
zoomGameStateFromAppState f = do
  gs <- use z
  (gs', a) <- liftIO . Fused.runM . runTimeIO $ Fused.runState gs f
  z .= gs'
  return a
 where
  z :: Lens' AppState GameState
  z = playState . scenarioState . gameState

-- | Modifies the game state using a fused-effect state action.
zoomGameStateFromScenarioState ::
  (MonadState ScenarioState m, MonadIO m) =>
  Fused.StateC GameState (TimeIOC (Fused.LiftC IO)) a ->
  m a
zoomGameStateFromScenarioState f = do
  gs <- use gameState
  (gs', a) <- liftIO (Fused.runM (runTimeIO (Fused.runState gs f)))
  gameState .= gs'
  return a

-- | Modifies the game state using a fused-effect state action.
zoomGameStateFromPlayState ::
  (MonadState PlayState m, MonadIO m) =>
  Fused.StateC GameState (TimeIOC (Fused.LiftC IO)) a ->
  m a
zoomGameStateFromPlayState f = do
  gs <- use $ scenarioState . gameState
  (gs', a) <- liftIO (Fused.runM (runTimeIO (Fused.runState gs f)))
  scenarioState . gameState .= gs'
  return a

onlyCreative :: (MonadState ScenarioState m) => m () -> m ()
onlyCreative a = do
  c <- use $ gameState . creativeMode
  when c a

-- | Create a list of handlers with embedding events and using pattern matching.
allHandlers ::
  (Ord e2, Enum e1, Bounded e1) =>
  (e1 -> e2) ->
  (e1 -> (Text, EventM Name s ())) ->
  [KeyEventHandler e2 (EventM Name s)]
allHandlers eEmbed f = map handleEvent1 enumerate
 where
  handleEvent1 e1 = let (n, a) = f e1 in onEvent (eEmbed e1) n a

runBaseTerm :: (MonadState ScenarioState m) => Maybe TSyntax -> m ()
runBaseTerm = mapM_ startBaseProgram
 where
  -- The player typed something at the REPL and hit Enter; this
  -- function takes the resulting term (if the REPL
  -- input is valid) and sets up the base robot to run it.
  startBaseProgram t = do
    -- Set the REPL status to Working
    gameState . gameControls . replStatus .= REPLWorking (t ^. sType) Nothing
    -- Set up the robot's CESK machine to evaluate/execute the
    -- given term.
    gameState . baseRobot . machine %= continue t
    -- Finally, be sure to activate the base robot.
    gameState %= execState (zoomRobots $ activateRobot 0)

-- | Set the REPL to the given text and REPL prompt type.
modifyResetREPL :: Text -> REPLPrompt -> REPLState -> REPLState
modifyResetREPL t r = (replPromptText .~ t) . (replPromptType .~ r)

-- | Reset the REPL state to the given text and REPL prompt type.
resetREPL :: MonadState ScenarioState m => Text -> REPLPrompt -> m ()
resetREPL t p = uiGameplay . uiREPL %= modifyResetREPL t p

-- | Add an item to the REPL history.
addREPLHistItem :: MonadState ScenarioState m => REPLHistItem -> m ()
addREPLHistItem item = uiGameplay . uiREPL . replHistory %= addREPLItem item
