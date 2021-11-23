{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  Swarm.TUI.Controller
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Event handlers for the TUI.
module Swarm.TUI.Controller (
  -- * Event handling
  handleEvent,
  shutdown,

  -- ** Handling 'Frame' events
  runFrameUI,
  runFrame,
  runFrameTicks,
  runGameTickUI,
  runGameTick,
  updateUI,

  -- ** REPL panel
  handleREPLEvent,
  validateREPLForm,
  adjReplHistIndex,
  TimeDir (..),

  -- ** World panel
  handleWorldEvent,
  keyToDir,
  scrollView,
  adjustTPS,

  -- ** Info panel
  handleInfoPanelEvent,
) where

import Control.Lens
import Control.Lens.Extras (is)
import Control.Monad.Except
import Control.Monad.State
import Data.Bits
import Data.Either (isRight)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Linear
import System.Clock
import Witch (into)

import Brick hiding (Direction)
import Brick.Focus
import Brick.Forms
import qualified Brick.Widgets.List as BL
import qualified Graphics.Vty as V

import qualified Control.Carrier.Lift as Fused
import qualified Control.Carrier.State.Lazy as Fused
import Swarm.Game.CESK (cancel, emptyStore, initMachine)
import Swarm.Game.Entity hiding (empty)
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.Step (gameTick)
import Swarm.Game.Value (Value (VUnit), prettyValue)
import qualified Swarm.Game.World as W
import Swarm.Language.Capability
import Swarm.Language.Context
import Swarm.Language.Pipeline
import Swarm.Language.Pretty
import Swarm.Language.Syntax
import Swarm.Language.Types
import Swarm.TUI.List
import Swarm.TUI.Model
import Swarm.Util hiding ((<<.=))

-- | Pattern synonyms to simplify brick event handler
pattern ControlKey, MetaKey :: Char -> BrickEvent n e
pattern ControlKey c = VtyEvent (V.EvKey (V.KChar c) [V.MCtrl])
pattern MetaKey c = VtyEvent (V.EvKey (V.KChar c) [V.MMeta])

pattern FKey :: Int -> BrickEvent n e
pattern FKey c = VtyEvent (V.EvKey (V.KFun c) [])

-- | The top-level event handler for the TUI.
handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent s (AppEvent Frame)
  | s ^. gameState . paused = continueWithoutRedraw s
  | otherwise = runFrameUI s
handleEvent s (VtyEvent (V.EvResize _ _)) = do
  invalidateCacheEntry WorldCache
  continue s
handleEvent s (VtyEvent (V.EvKey (V.KChar '\t') [])) = continue $ s & uiState . uiFocusRing %~ focusNext
handleEvent s (VtyEvent (V.EvKey V.KBackTab [])) = continue $ s & uiState . uiFocusRing %~ focusPrev
handleEvent s (VtyEvent (V.EvKey V.KEsc []))
  | isJust (s ^. uiState . uiError) = continue $ s & uiState . uiError .~ Nothing
  | isJust (s ^. uiState . uiModal) = continue $ s & uiState . uiModal .~ Nothing
handleEvent s ev = do
  -- intercept special keys that works on all panels
  case ev of
    ControlKey 'q' -> shutdown s
    MetaKey 'w' -> setFocus s WorldPanel
    MetaKey 'e' -> setFocus s RobotPanel
    MetaKey 'r' -> setFocus s REPLPanel
    MetaKey 't' -> setFocus s InfoPanel
    FKey 1 -> toggleModal s HelpModal
    _anyOtherEvent
      | isJust (s ^. uiState . uiModal) -> continueWithoutRedraw s
      | otherwise ->
        -- and dispatch the other to the focused panel handler
        case focusGetCurrent (s ^. uiState . uiFocusRing) of
          Just REPLPanel -> handleREPLEvent s ev
          Just WorldPanel -> handleWorldEvent s ev
          Just RobotPanel -> handleRobotPanelEvent s ev
          Just InfoPanel -> handleInfoPanelEvent s ev
          _ -> continueWithoutRedraw s

setFocus :: AppState -> Name -> EventM Name (Next AppState)
setFocus s name = continue $ s & uiState . uiFocusRing %~ focusSetCurrent name

toggleModal :: AppState -> Modal -> EventM Name (Next AppState)
toggleModal s modal = do
  curTime <- liftIO $ getTime Monotonic
  continue $
    s & case s ^. uiState . uiModal of
      Nothing -> (uiState . uiModal ?~ modal) . ensurePause
      Just _ -> (uiState . uiModal .~ Nothing) . maybeUnpause . resetLastFrameTime curTime
 where
  -- Set the game to AutoPause if needed
  ensurePause
    | s ^. gameState . paused = id
    | otherwise = gameState . runStatus .~ AutoPause
  -- Set the game to Running if it was auto paused
  maybeUnpause
    | s ^. gameState . runStatus == AutoPause = gameState . runStatus .~ Running
    | otherwise = id
  -- When unpausing, it is critical to ensure the next frame doesn't
  -- catch up from the time spent in pause.
  -- TODO: manage unpause more safely to also cover
  -- the world event handler for the KChar 'p'.
  resetLastFrameTime curTime = uiState . lastFrameTime .~ curTime

-- | Shut down the application.  Currently all it does is write out
--   the updated REPL history to a @.swarm_history@ file.
shutdown :: AppState -> EventM Name (Next AppState)
shutdown s = do
  let hist = mapMaybe getREPLEntry $ getLatestREPLHistoryItems maxBound history
  liftIO $ (`T.appendFile` T.unlines hist) =<< getSwarmHistoryPath True
  let s' = s & uiState . uiReplHistory %~ restartREPLHistory
  halt s'
 where
  history = s ^. uiState . uiReplHistory

------------------------------------------------------------
-- Handling Frame events
------------------------------------------------------------

-- | Run the game for a single /frame/ (/i.e./ screen redraw), then
--   update the UI.  Depending on how long it is taking to draw each
--   frame, and how many ticks per second we are trying to achieve,
--   this may involve stepping the game any number of ticks (including
--   zero).
runFrameUI :: AppState -> EventM Name (Next AppState)
runFrameUI s = do
  (redraw, newState) <- runStateT (runFrame >> updateUI) s
  (if redraw then continue else continueWithoutRedraw) newState

-- | Run the game for a single frame, without updating the UI.
runFrame :: StateT AppState (EventM Name) ()
runFrame = do
  -- Reset the needsRedraw flag.  While procssing the frame and stepping the robots,
  -- the flag will get set to true if anything changes that requires redrawing the
  -- world (e.g. a robot moving or disappearing).
  gameState . needsRedraw .= False

  -- The logic here is taken from https://gafferongames.com/post/fix_your_timestep/ .

  -- Find out how long the previous frame took, by subtracting the
  -- previous time from the current time.
  prevTime <- use (uiState . lastFrameTime)
  curTime <- liftIO $ getTime Monotonic
  let frameTime = diffTimeSpec curTime prevTime

  -- Remember now as the new previous time.
  uiState . lastFrameTime .= curTime

  -- We now have some additional accumulated time to play with.  The
  -- idea is to now "catch up" by doing as many ticks as are supposed
  -- to fit in the accumulated time.  Some accumulated time may be
  -- left over, but it will roll over to the next frame.  This way we
  -- deal smoothly with things like a variable frame rate, the frame
  -- rate not being a nice multiple of the desired ticks per second,
  -- etc.
  uiState . accumulatedTime += frameTime

  -- Figure out how many ticks per second we're supposed to do,
  -- and compute the timestep `dt` for a single tick.
  lgTPS <- use (uiState . lgTicksPerSecond)
  let oneSecond = 1_000_000_000 -- one second = 10^9 nanoseconds
      dt
        | lgTPS >= 0 = oneSecond `div` (1 `shiftL` lgTPS)
        | otherwise = oneSecond * (1 `shiftL` abs lgTPS)

  -- Update TPS/FPS counters every second
  infoUpdateTime <- use (uiState . lastInfoTime)
  let updateTime = toNanoSecs $ diffTimeSpec curTime infoUpdateTime
  when (updateTime >= oneSecond) $ do
    -- Wait for at least one second to have elapsed
    when (infoUpdateTime /= 0) $ do
      -- set how much frame got processed per second
      frames <- use (uiState . frameCount)
      uiState . uiFPS .= fromIntegral (frames * fromInteger oneSecond) / fromIntegral updateTime

      -- set how much ticks got processed per frame
      uiTicks <- use (uiState . tickCount)
      uiState . uiTPF .= fromIntegral uiTicks / fromIntegral frames

      -- ensure this frame gets drawn
      gameState . needsRedraw .= True

    -- Reset the counter and wait another seconds for the next update
    uiState . tickCount .= 0
    uiState . frameCount .= 0
    uiState . lastInfoTime .= curTime

  -- Increment the frame count
  uiState . frameCount += 1

  -- Now do as many ticks as we need to catch up.
  uiState . frameTickCount .= 0
  runFrameTicks (fromNanoSecs dt)

ticksPerFrameCap :: Int
ticksPerFrameCap = 30

-- | Do zero or more ticks, with each tick notionally taking the given
--   timestep, until we have used up all available accumulated time,
--   OR until we have hit the cap on ticks per frame, whichever comes
--   first.
runFrameTicks :: TimeSpec -> StateT AppState (EventM Name) ()
runFrameTicks dt = do
  a <- use (uiState . accumulatedTime)
  t <- use (uiState . frameTickCount)

  -- Is there still time left?  Or have we hit the cap on ticks per frame?
  when (a >= dt && t < ticksPerFrameCap) $ do
    -- If so, do a tick, count it, subtract dt from the accumulated time,
    -- and loop!
    runGameTick
    uiState . tickCount += 1
    uiState . frameTickCount += 1
    uiState . accumulatedTime -= dt
    runFrameTicks dt

-- | Run the game for a single tick, and update the UI.
runGameTickUI :: AppState -> EventM Name (Next AppState)
runGameTickUI s = execStateT (runGameTick >> updateUI) s >>= continue

-- | Modifies the game state using a fused-effect state action.
zoomGameState :: (MonadState AppState m, MonadIO m) => Fused.StateC GameState (Fused.LiftC IO) a -> m ()
zoomGameState f = do
  gs <- use gameState
  gs' <- liftIO (Fused.runM (Fused.execState gs f))
  gameState .= gs'

-- | Run the game for a single tick (/without/ updating the UI).
--   Every robot is given a certain amount of maximum computation to
--   perform a single world action (like moving, turning, grabbing,
--   etc.).
runGameTick :: StateT AppState (EventM Name) ()
runGameTick = zoomGameState gameTick

-- | Update the UI.  This function is used after running the
--   game for some number of ticks.
updateUI :: StateT AppState (EventM Name) Bool
updateUI = do
  loadVisibleRegion

  -- If the game state indicates a redraw is needed, invalidate the
  -- world cache so it will be redrawn.
  g <- use gameState
  when (g ^. needsRedraw) $ lift (invalidateCacheEntry WorldCache)

  -- Check if the inventory list needs to be updated.
  listRobotHash <- fmap fst <$> use (uiState . uiInventory)
  -- The hash of the robot whose inventory is currently displayed (if any)

  fr <- use (gameState . to focusedRobot)
  let focusedRobotHash = view inventoryHash <$> fr
  -- The hash of the focused robot (if any)

  -- If the hashes don't match (either because which robot (or
  -- whether any robot) is focused changed, or the focused robot's
  -- inventory changed), regenerate the list.
  inventoryUpdated <-
    if listRobotHash /= focusedRobotHash
      then do
        zoom uiState $ populateInventoryList fr
        pure True
      else pure False

  -- Now check if the base finished running a program entered at the REPL.
  replUpdated <- case g ^. replStatus of
    -- It did, and the result was the unit value.  Just reset replStatus.
    REPLWorking _ (Just VUnit) -> do
      gameState . replStatus .= REPLDone
      pure True

    -- It did, and returned some other value.  Pretty-print the
    -- result as a REPL output, with its type, and reset the replStatus.
    REPLWorking pty (Just v) -> do
      let out = T.intercalate " " [into (prettyValue v), ":", prettyText (stripCmd pty)]
      uiState . uiReplHistory %= addREPLItem (REPLOutput out)
      gameState . replStatus .= REPLDone
      pure True

    -- Otherwise, do nothing.
    _ -> pure False

  -- If the focused robot's log has been updated, attempt to
  -- automatically switch to it and scroll all the way down so the new
  -- message can be seen.
  uiState . uiScrollToEnd .= False
  logUpdated <- do
    case maybe False (view robotLogUpdated) fr of
      False -> pure False
      True -> do
        -- Reset the log updated flag
        zoomGameState clearFocusedRobotLogUpdated

        -- Find and focus an installed "logger" device in the inventory list.
        let isLogger (InstalledEntry e) = e ^. entityName == "logger"
            isLogger _ = False
            focusLogger = BL.listFindBy isLogger

        uiState . uiInventory . _Just . _2 %= focusLogger

        -- Now inform the UI that it should scroll the info panel to
        -- the very end.
        uiState . uiScrollToEnd .= True
        pure True

  -- Decide whether the info panel has more content scrolled off the
  -- top and/or bottom, so we can draw some indicators to show it if
  -- so.  Note, because we only know the update size and position of
  -- the viewport *after* it has been rendered, this means the top and
  -- bottom indicators will only be updated one frame *after* the info
  -- panel updates, but this isn't really that big of deal.
  infoPanelUpdated <- do
    mvp <- lift $ lookupViewport InfoViewport
    case mvp of
      Nothing -> return False
      Just vp -> do
        let topMore = (vp ^. vpTop) > 0
            botMore = (vp ^. vpTop + snd (vp ^. vpSize)) < snd (vp ^. vpContentSize)
        oldTopMore <- uiState . uiMoreInfoTop <<.= topMore
        oldBotMore <- uiState . uiMoreInfoBot <<.= botMore
        return $ oldTopMore /= topMore || oldBotMore /= botMore

  -- Decide whether to show a pop-up modal congratulating the user on
  -- successfully completing the current challenge.
  winModalUpdated <- do
    w <- use (gameState . winCondition)
    case w of
      Won False -> do
        gameState . winCondition .= Won True
        uiState . uiModal .= Just WinModal
        return True
      _ -> return False

  let redraw = g ^. needsRedraw || inventoryUpdated || replUpdated || logUpdated || infoPanelUpdated || winModalUpdated
  pure redraw

-- | Make sure all tiles covering the visible part of the world are
--   loaded.
loadVisibleRegion :: StateT AppState (EventM Name) ()
loadVisibleRegion = do
  mext <- lift $ lookupExtent WorldExtent
  case mext of
    Nothing -> return ()
    Just (Extent _ _ size) -> do
      gs <- use gameState
      gameState . world %= W.loadRegion (viewingRegion gs (over both fromIntegral size))

stripCmd :: Polytype -> Polytype
stripCmd (Forall xs (TyCmd ty)) = Forall xs ty
stripCmd pty = pty

------------------------------------------------------------
-- REPL events
------------------------------------------------------------

-- | Handle a user input event for the REPL.
handleREPLEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleREPLEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) =
  continue $
    s
      & gameState . robotMap . ix "base" . machine %~ cancel
handleREPLEvent s (VtyEvent (V.EvKey V.KEnter [])) =
  if not $ s ^. gameState . replWorking
    then case processTerm' topTypeCtx topCapCtx entry of
      Right t@(ProcessedTerm _ (Module ty _) _ _) ->
        continue $
          s
            & uiState . uiReplForm %~ updateFormState ""
            & uiState . uiReplType .~ Nothing
            & uiState . uiReplHistory %~ addREPLItem (REPLEntry entry)
            & uiState . uiError .~ Nothing
            & gameState . replStatus .~ REPLWorking ty Nothing
            & gameState . robotMap . ix "base" . machine .~ initMachine t topValCtx topStore
            & gameState %~ execState (activateRobot "base")
      Left err ->
        continue $
          s
            & uiState . uiError ?~ txt err
    else continueWithoutRedraw s
 where
  entry = formState (s ^. uiState . uiReplForm)
  topTypeCtx = s ^. gameState . robotMap . ix "base" . robotContext . defTypes
  topCapCtx = s ^. gameState . robotMap . ix "base" . robotContext . defCaps
  topValCtx = s ^. gameState . robotMap . ix "base" . robotContext . defVals
  topStore =
    fromMaybe emptyStore $
      s ^? gameState . robotMap . at "base" . _Just . robotContext . defStore
handleREPLEvent s (VtyEvent (V.EvKey V.KUp [])) =
  continue $ s & adjReplHistIndex Older
handleREPLEvent s (VtyEvent (V.EvKey V.KDown [])) =
  continue $ s & adjReplHistIndex Newer
handleREPLEvent s ev = do
  f' <- handleFormEvent ev (s ^. uiState . uiReplForm)
  continue $ validateREPLForm (s & uiState . uiReplForm .~ f')

-- | Validate the REPL input when it changes: see if it parses and
--   typechecks, and set the color accordingly.
validateREPLForm :: AppState -> AppState
validateREPLForm s =
  s
    & uiState . uiReplForm %~ validate
    & uiState . uiReplType .~ theType
 where
  topTypeCtx = s ^. gameState . robotMap . ix "base" . robotContext . defTypes
  topCapCtx = s ^. gameState . robotMap . ix "base" . robotContext . defCaps
  result = processTerm' topTypeCtx topCapCtx (s ^. uiState . uiReplForm . to formState)
  theType = case result of
    Right (ProcessedTerm _ (Module ty _) _ _) -> Just ty
    _ -> Nothing
  validate = setFieldValid (isRight result) REPLInput

-- | Update our current position in the REPL history.
adjReplHistIndex :: TimeDir -> AppState -> AppState
adjReplHistIndex d s =
  ns
    & (if replIndexIsAtInput (s ^. repl) then saveLastEntry else id)
    & (if oldEntry /= newEntry then showNewEntry else id)
    & validateREPLForm
 where
  -- new AppState after moving the repl index
  ns = s & repl %~ moveReplHistIndex d oldEntry

  repl :: Lens' AppState REPLHistory
  repl = uiState . uiReplHistory

  replLast = s ^. uiState . uiReplLast
  saveLastEntry = uiState . uiReplLast .~ formState (s ^. uiState . uiReplForm)
  showNewEntry = uiState . uiReplForm %~ updateFormState newEntry
  -- get REPL data
  getCurrEntry = fromMaybe replLast . getCurrentItemText . view repl
  oldEntry = getCurrEntry s
  newEntry = getCurrEntry ns

------------------------------------------------------------
-- World events
------------------------------------------------------------

worldScrollDist :: Int64
worldScrollDist = 8

-- | Handle a user input event in the world view panel.
handleWorldEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
-- scrolling the world view in Creative mode
handleWorldEvent s (VtyEvent (V.EvKey k []))
  | s ^. gameState . creativeMode
      && k
        `elem` [ V.KUp
               , V.KDown
               , V.KLeft
               , V.KRight
               , V.KChar 'h'
               , V.KChar 'j'
               , V.KChar 'k'
               , V.KChar 'l'
               ] =
    scrollView s (^+^ (worldScrollDist *^ keyToDir k)) >>= continue
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar 'c') [])) = do
  invalidateCacheEntry WorldCache
  continue $ s & gameState . viewCenterRule .~ VCRobot "base"

-- pausing and stepping
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar 'p') [])) = do
  curTime <- liftIO $ getTime Monotonic
  continue $
    s
      & gameState . runStatus %~ (\status -> if status == Running then ManualPause else Running)
      -- Also reset the last frame time to now. If we are pausing, it
      -- doesn't matter; if we are unpausing, this is critical to
      -- ensure the next frame doesn't think it has to catch up from
      -- whenever the game was paused!
      & uiState . lastFrameTime .~ curTime
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar 's') []))
  | s ^. gameState . paused = runGameTickUI s
  | otherwise = continueWithoutRedraw s
-- speed controls
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar '<') [])) =
  continue $ adjustTPS (-) s
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar '>') [])) =
  continue $ adjustTPS (+) s
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar ',') [])) =
  continue $ adjustTPS (-) s
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar '.') [])) =
  continue $ adjustTPS (+) s
-- show fps
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar 'f') [])) =
  continue $ (s & uiState . uiShowFPS %~ not)
-- for testing only: toggle between classic & creative modes
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar 'm') [])) =
  continue (s & gameState . creativeMode %~ not)
-- Fall-through case: don't do anything.
handleWorldEvent s _ = continueWithoutRedraw s

-- | Manually scroll the world view.
scrollView :: AppState -> (V2 Int64 -> V2 Int64) -> EventM Name AppState
scrollView s update = do
  -- Manually invalidate the 'WorldCache' instead of just setting
  -- 'needsRedraw'.  I don't quite understand why the latter doesn't
  -- always work, but there seems to be some sort of race condition
  -- where 'needsRedraw' gets reset before the UI drawing code runs.
  invalidateCacheEntry WorldCache
  return $ s & gameState %~ modifyViewCenter update

-- | Convert a directional key into a direction.
keyToDir :: V.Key -> V2 Int64
keyToDir V.KUp = north
keyToDir V.KDown = south
keyToDir V.KRight = east
keyToDir V.KLeft = west
keyToDir (V.KChar 'h') = west
keyToDir (V.KChar 'j') = south
keyToDir (V.KChar 'k') = north
keyToDir (V.KChar 'l') = east
keyToDir _ = V2 0 0

-- | Adjust the ticks per second speed.
adjustTPS :: (Int -> Int -> Int) -> AppState -> AppState
adjustTPS (+/-) = uiState . lgTicksPerSecond %~ (+/- 1)

------------------------------------------------------------
-- Robot panel events
------------------------------------------------------------

-- | Handle user input events in the robot panel.
handleRobotPanelEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleRobotPanelEvent s (VtyEvent (V.EvKey V.KEnter [])) = do
  let mList = s ^? uiState . uiInventory . _Just . _2
  case mList >>= BL.listSelectedElement of
    Nothing -> continueWithoutRedraw s
    Just (_, Separator _) -> continueWithoutRedraw s
    Just (_, InventoryEntry _ e) -> makeEntity s e
    Just (_, InstalledEntry e) -> makeEntity s e
handleRobotPanelEvent s (VtyEvent ev) = do
  let mList = s ^? uiState . uiInventory . _Just . _2
  case mList of
    Nothing -> continueWithoutRedraw s
    Just l -> do
      l' <- handleListEventWithSeparators ev (is _Separator) l
      let s' = s & uiState . uiInventory . _Just . _2 .~ l'
      continue s'
handleRobotPanelEvent s _ = continueWithoutRedraw s

-- | Attempt to make an entity selected from the inventory, if the
--   base is not currently busy.
makeEntity :: AppState -> Entity -> EventM Name (Next AppState)
makeEntity s e = do
  let mkTy = Forall [] $ TyCmd TyUnit
      mkProg = TApp (TConst Make) (TString (e ^. entityName))
      mkPT = ProcessedTerm mkProg (Module mkTy empty) (S.singleton CMake) empty
      topStore =
        fromMaybe emptyStore $
          s ^? gameState . robotMap . at "base" . _Just . robotContext . defStore

  case isActive <$> (s ^. gameState . robotMap . at "base") of
    Just False ->
      continue $
        s
          & gameState . replStatus .~ REPLWorking mkTy Nothing
          & gameState . robotMap . ix "base" . machine .~ initMachine mkPT empty topStore
          & gameState %~ execState (activateRobot "base")
    _ -> continueWithoutRedraw s

------------------------------------------------------------
-- Info panel events
------------------------------------------------------------

-- | Handle user events in the info panel (just scrolling).
handleInfoPanelEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleInfoPanelEvent s = \case
  VtyEvent (V.EvKey V.KDown []) -> vScrollBy infoScroll 1 >> continue s
  VtyEvent (V.EvKey V.KUp []) -> vScrollBy infoScroll (-1) >> continue s
  VtyEvent (V.EvKey (V.KChar 'k') []) -> vScrollBy infoScroll 1 >> continue s
  VtyEvent (V.EvKey (V.KChar 'j') []) -> vScrollBy infoScroll (-1) >> continue s
  VtyEvent (V.EvKey V.KPageDown []) -> vScrollPage infoScroll Brick.Down >> continue s
  VtyEvent (V.EvKey V.KPageUp []) -> vScrollPage infoScroll Brick.Up >> continue s
  VtyEvent (V.EvKey V.KHome []) -> vScrollToBeginning infoScroll >> continue s
  VtyEvent (V.EvKey V.KEnd []) -> vScrollToEnd infoScroll >> continue s
  _ -> continueWithoutRedraw s
