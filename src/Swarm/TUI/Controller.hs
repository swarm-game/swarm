-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.TUI.Controller
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Event handlers for the TUI.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}

module Swarm.TUI.Controller
  ( -- * Event handling

    handleEvent
  , shutdown

    -- ** Handling 'Frame' events

  , runFrameUI, runFrame, runFrameTicks
  , runGameTickUI, runGameTick
  , updateUI

    -- ** REPL panel

  , handleREPLEvent, validateREPLForm, adjReplHistIndex

    -- ** World panel

  , handleWorldEvent
  , keyToDir, scrollView
  , adjustTPS

    -- ** Info panel

  , handleInfoPanelEvent

  )
  where

import           Control.Lens
import           Control.Lens.Extras       (is)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bits
import           Data.Either               (isRight)
import           Data.Int                  (Int64)
import           Data.Maybe                (isJust)
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import           Linear
import           System.Clock
import           Witch                     (into)

import           Brick                     hiding (Direction)
import           Brick.Focus
import           Brick.Forms
import qualified Brick.Widgets.List        as BL
import qualified Graphics.Vty              as V

import           Swarm.Game.CEK            (idleMachine, initMachine)
import           Swarm.Game.Entity         hiding (empty)
import           Swarm.Game.Robot
import           Swarm.Game.State
import           Swarm.Game.Step           (gameTick)
import           Swarm.Game.Value          (Value (VUnit), prettyValue)
import qualified Swarm.Game.World          as W
import           Swarm.Language.Capability
import           Swarm.Language.Context
import           Swarm.Language.Pipeline
import           Swarm.Language.Pretty
import           Swarm.Language.Syntax
import           Swarm.Language.Types
import           Swarm.TUI.List
import           Swarm.TUI.Model
import           Swarm.Util

-- | The top-level event handler for the TUI.
handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent s (AppEvent Frame)
  | s ^. gameState . paused = continueWithoutRedraw s
  | otherwise               = runFrameUI s

handleEvent s (VtyEvent (V.EvResize _ _))            = do
  invalidateCacheEntry WorldCache
  continue s
handleEvent s (VtyEvent (V.EvKey (V.KChar '\t') [])) = continue $ s & uiState . uiFocusRing %~ focusNext
handleEvent s (VtyEvent (V.EvKey V.KBackTab []))     = continue $ s & uiState . uiFocusRing %~ focusPrev
handleEvent s (VtyEvent (V.EvKey V.KEsc []))
  | isJust (s ^. uiState . uiError) = continue $ s & uiState . uiError .~ Nothing
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = shutdown s
handleEvent s ev =
  case focusGetCurrent (s ^. uiState . uiFocusRing) of
    Just REPLPanel  -> handleREPLEvent s ev
    Just WorldPanel -> handleWorldEvent s ev
    Just InfoPanel  -> handleInfoPanelEvent s ev
    _               -> continueWithoutRedraw s

-- | Shut down the application.  Currently all it does is write out
--   the updated REPL history to a @.swarm_history@ file.
shutdown :: AppState -> EventM Name (Next AppState)
shutdown s = do
  let s'   = s & uiState . uiReplHistory . traverse %~ markOld
      hist = filter isEntry (s' ^. uiState . uiReplHistory)
  liftIO $ writeFile ".swarm_history" (show hist)
  halt s'

  where
    markOld (REPLEntry _ e) = REPLEntry False e
    markOld r               = r

    isEntry REPLEntry{} = True
    isEntry _           = False

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
  let oneSecond = 1_000_000_000  -- one second = 10^9 nanoseconds
      dt
        | lgTPS >= 0 = oneSecond `div` (1 `shiftL` lgTPS)
        | otherwise  = oneSecond * (1 `shiftL` abs lgTPS)

  -- Now do as many ticks as we need to catch up.
  runFrameTicks (fromNanoSecs dt)
  zoom uiState rememberFrameTicks

-- | Do zero or more ticks, with each tick notionally taking the given
--   timestep, until we have used up all available accumulated time.
runFrameTicks :: TimeSpec -> StateT AppState (EventM Name) ()
runFrameTicks dt = do
  a <- use (uiState . accumulatedTime)

  -- Is there still time left?
  when (a >= dt) $ do

    -- If so, do a tick, count it, subtract dt from the accumulated time,
    -- and loop!
    runGameTick
    uiState . frameTicks += 1
    uiState . accumulatedTime -= dt
    runFrameTicks dt

-- | Run the game for a single tick, and update the UI.
runGameTickUI :: AppState -> EventM Name (Next AppState)
runGameTickUI s = execStateT (runGameTick >> updateUI) s >>= continue

-- | Run the game for a single tick (/without/ updating the UI).
--   Every robot is given a certain amount of maximum computation to
--   perform a single world action (like moving, turning, grabbing,
--   etc.).
runGameTick :: StateT AppState (EventM Name) ()
runGameTick = zoom gameState gameTick

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
  listRobotHash    <- fmap fst <$> use (uiState . uiInventory)
    -- The hash of the robot whose inventory is currently displayed (if any)

  fr <- use (gameState . to focusedRobot)
  let focusedRobotHash = view (robotEntity . entityHash) <$> fr
    -- The hash of the focused robot (if any)

  -- If the hashes don't match (either because which robot (or
  -- whether any robot) is focused changed, or the focused robot's
  -- inventory changed), regenerate the list.
  inventoryUpdated <-
    if (listRobotHash /= focusedRobotHash)
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
      uiState . uiReplHistory %= (REPLOutput out :)
      gameState . replStatus .= REPLDone
      pure True

    -- Otherwise, do nothing.
    _ -> pure False

  let redraw = g ^. needsRedraw || inventoryUpdated || replUpdated
  pure redraw

-- | Make sure all tiles covering the visible part of the world are
--   loaded.
loadVisibleRegion :: StateT AppState (EventM Name) ()
loadVisibleRegion = do
  mext <- lift $ lookupExtent WorldExtent
  case mext of
    Nothing  -> return ()
    Just (Extent _ _ size) -> do
      gs <- use gameState
      gameState . world %= W.loadRegion (viewingRegion gs (over both fromIntegral size))

stripCmd :: Polytype -> Polytype
stripCmd (Forall xs (TyCmd ty)) = Forall xs ty
stripCmd pty                    = pty

------------------------------------------------------------
-- REPL events
------------------------------------------------------------

-- | Handle a user input event for the REPL.
handleREPLEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleREPLEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]))
  = continue $ s
      & uiState . uiReplForm %~ updateFormState ""
handleREPLEvent s (VtyEvent (V.EvKey V.KEnter []))
  = case processTerm' topCtx topCapCtx entry of
      Right t@(ProcessedTerm _ (Module ty _) _ _) ->
        continue $ s
          & uiState . uiReplForm    %~ updateFormState ""
          & uiState . uiReplType    .~ Nothing
          & uiState . uiReplHistory %~ (REPLEntry True entry :)
          & uiState . uiReplHistIdx .~ (-1)
          & gameState . replStatus .~ REPLWorking ty Nothing
          & gameState . robotMap . ix "base" . machine .~ initMachine t topEnv
      Left err ->
        continue $ s
          & uiState . uiError ?~ txt err

      -- XXX check that we have the capabilities needed to run the
      -- program before even starting?
  where
    entry = formState (s ^. uiState . uiReplForm)
    (topCtx, topCapCtx) = s ^. gameState . robotMap . ix "base" . robotCtx
    topEnv = s ^. gameState . robotMap . ix "base" . robotEnv

handleREPLEvent s (VtyEvent (V.EvKey V.KUp []))
  = continue $ s & adjReplHistIndex (+)
handleREPLEvent s (VtyEvent (V.EvKey V.KDown []))
  = continue $ s & adjReplHistIndex (-)
handleREPLEvent s ev = do
  f' <- handleFormEvent ev (s ^. uiState . uiReplForm)
  continue $ validateREPLForm (s & uiState . uiReplForm .~ f')

-- | Validate the REPL input when it changes: see if it parses and
--   typechecks, and set the color accordingly.
validateREPLForm :: AppState -> AppState
validateREPLForm s = s
  & uiState . uiReplForm %~ validate
  & uiState . uiReplType .~ theType
  where
    (topCtx, topCapCtx) = s ^. gameState . robotMap . ix "base" . robotCtx
    result = processTerm' topCtx topCapCtx (s ^. uiState . uiReplForm . to formState)
    theType = case result of
      Right (ProcessedTerm _ (Module ty _) _ _) -> Just ty
      _                                         -> Nothing
    validate = setFieldValid (isRight result) REPLInput

-- | Update our current position in the REPL history.
adjReplHistIndex :: (Int -> Int -> Int) -> AppState -> AppState
adjReplHistIndex (+/-) s =
  s & uiState . uiReplHistIdx .~ newIndex
    & (if newIndex /= curIndex then uiState . uiReplForm %~ updateFormState newEntry else id)
    & validateREPLForm
  where
    entries = [e | REPLEntry _ e <- s ^. uiState . uiReplHistory]
    curIndex = s ^. uiState . uiReplHistIdx
    histLen  = length entries
    newIndex = min (histLen - 1) (max (-1) (curIndex +/- 1))
    newEntry
      | newIndex == -1 = ""
      | otherwise      = entries !! newIndex

------------------------------------------------------------
-- World events
------------------------------------------------------------

worldScrollDist :: Int64
worldScrollDist = 8

-- | Handle a user input event in the world view panel.
handleWorldEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)

-- scrolling the world view
handleWorldEvent s (VtyEvent (V.EvKey k []))
  | k `elem` [ V.KUp, V.KDown, V.KLeft, V.KRight
             , V.KChar 'h', V.KChar 'j', V.KChar 'k', V.KChar 'l' ]
  = scrollView s (^+^ (worldScrollDist *^ keyToDir k)) >>= continue
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar 'c') [])) = do
  invalidateCacheEntry WorldCache
  continue $ s & gameState . viewCenterRule .~ VCRobot "base"
               & gameState %~ recalcViewCenter

-- pausing and stepping
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar 'p') [])) = do
  curTime <- liftIO $ getTime Monotonic
  continue $ s
      & gameState . paused %~ not

      -- Also reset the last frame time to now. If we are pausing, it
      -- doesn't matter; if we are unpausing, this is critical to
      -- ensure the next frame doesn't think it has to catch up from
      -- whenever the game was paused!
      & uiState . lastFrameTime .~ curTime

handleWorldEvent s (VtyEvent (V.EvKey (V.KChar 's') []))
  | s ^. gameState . paused = runGameTickUI s
  | otherwise               = continueWithoutRedraw s

-- speed controls
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar '<') []))
  = continueWithoutRedraw $ adjustTPS (-) s
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar '>') []))
  = continueWithoutRedraw $ adjustTPS (+) s

-- for testing only: toggle between classic & creative modes
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar 'm') []))
  = continueWithoutRedraw (s & gameState . gameMode %~ cycleEnum)

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
keyToDir V.KUp         = north
keyToDir V.KDown       = south
keyToDir V.KRight      = east
keyToDir V.KLeft       = west
keyToDir (V.KChar 'h') = west
keyToDir (V.KChar 'j') = south
keyToDir (V.KChar 'k') = north
keyToDir (V.KChar 'l') = east
keyToDir _             = V2 0 0

-- | Adjust the ticks per second speed.
adjustTPS :: (Int -> Int -> Int) -> AppState -> AppState
adjustTPS (+/-) = uiState . lgTicksPerSecond %~ (+/- 1)

------------------------------------------------------------
-- Info panel events
------------------------------------------------------------

-- | Handle user input events in the info panel.
handleInfoPanelEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleInfoPanelEvent s (VtyEvent (V.EvKey V.KEnter [])) = do
  let mList = s ^? uiState . uiInventory . _Just . _2
  case mList >>= BL.listSelectedElement of
    Nothing -> continueWithoutRedraw s
    Just (_, Separator _) -> continueWithoutRedraw s
    Just (_, InventoryEntry _ e) -> do
      let topEnv = s ^. gameState . robotMap . ix "base" . robotEnv
          mkTy   = Forall [] $ TyCmd TyUnit
          mkProg = TApp (TConst Make) (TString (e ^. entityName))
          mkPT   = ProcessedTerm mkProg (Module mkTy empty) (S.singleton CMake) empty
      case isActive <$> (s ^. gameState . robotMap . at "base") of
        Just False -> continue $ s
          & gameState . replStatus .~ REPLWorking mkTy Nothing
          & gameState . robotMap . ix "base" . machine .~ initMachine mkPT topEnv
        _          -> continueWithoutRedraw s

handleInfoPanelEvent s (VtyEvent ev) = do
  let mList = s ^? uiState . uiInventory . _Just . _2
  case mList of
    Nothing -> continueWithoutRedraw s
    Just l  -> do
      l' <- handleListEventWithSeparators ev (is _Separator) l
      let s' = s & uiState . uiInventory . _Just . _2 .~ l'
      continue s'
handleInfoPanelEvent s _ = continueWithoutRedraw s
