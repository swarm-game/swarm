{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}

module Swarm.TUI where

import           Control.Lens
import           Control.Lens.Extras       (is)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bits
import           Data.Either               (isRight)
import           Data.List                 (findIndex, sortOn)
import           Data.Maybe                (fromMaybe, isJust)
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Data.Vector               as V
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

------------------------------------------------------------
-- Event handling

-- | Run the game for a single /frame/ (i.e. screen redraw).
--   Depending on how long it is taking to draw each frame, and how
--   many ticks/second we are trying to achieve, this may involve
--   stepping the game any number of ticks (including zero).
runFrameUI :: AppState -> EventM Name (Next AppState)
runFrameUI s = execStateT (runFrame >> updateUI) s >>= continue

-- | Update the game state one /frame/'s worth. Depending on how long
--   it is taking to draw each frame, and how many ticks/second we are
--   trying to achieve, this may involve stepping the game any number
--   of ticks (including zero).  Note that this function does not actually
--   update the UI at all.
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

  -- Reset the tick counter for the current frame
  uiState . ticksPerFrame .= 0

  -- Now do as many ticks as we need to catch up.
  frameTicks (fromNanoSecs dt)

-- | Do zero or more ticks, with each tick notionally taking the given
--   timestep, until we have used up all available accumulated time.
frameTicks :: TimeSpec -> StateT AppState (EventM Name) ()
frameTicks dt = do
  a <- use (uiState . accumulatedTime)

  -- Is there still time left?
  when (a >= dt) $ do

    -- If so, do a tick, count it, subtract dt from the accumulated time,
    -- and loop!
    runGameTick
    uiState . ticksPerFrame += 1
    uiState . accumulatedTime -= dt
    frameTicks dt

-- | Run the game for a single tick, and update the UI.
runGameTickUI :: AppState -> EventM Name (Next AppState)
runGameTickUI s = execStateT (runGameTick >> updateUI) s >>= continue

-- | Run the game for a single tick (/without/ updating the UI).
--   Every robot is given a certain amount of maximum computation to
--   perform a single world action (like moving, turning, grabbing,
--   etc.).
runGameTick :: StateT AppState (EventM Name) ()
runGameTick = zoom gameState gameTick

-- | Update the UI after running the game for some number of ticks.
updateUI :: StateT AppState (EventM Name) ()
updateUI = do

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
  when (listRobotHash /= focusedRobotHash) (zoom uiState $ populateInventoryList fr)

  -- Now check if the base finished running a program entered at the REPL.
  case g ^. replStatus of

    -- It did, and the result was the unit value.  Just reset replStatus.
    REPLWorking _ (Just VUnit) -> gameState . replStatus .= REPLDone

    -- It did, and returned some other value.  Pretty-print the
    -- result as a REPL output, with its type, and reset the replStatus.
    REPLWorking pty (Just v) -> do
      let out = T.intercalate " " [into (prettyValue v), ":", prettyText (stripCmd pty)]
      uiState . uiReplHistory %= (REPLOutput out :)
      gameState . replStatus .= REPLDone

    -- Otherwise, do nothing.
    _ -> return ()

stripCmd :: Polytype -> Polytype
stripCmd (Forall xs (TyCmd ty)) = Forall xs ty
stripCmd pty                    = pty


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

populateInventoryList :: MonadState UIState m => Maybe Robot -> m ()
populateInventoryList Nothing  = uiInventory .= Nothing
populateInventoryList (Just r) = do
  mList <- preuse (uiInventory . _Just . _2)
  let mkInvEntry (n,e) = InventoryEntry n e
      itemList label
        = (\case { [] -> []; xs -> Separator label : xs })
        . map mkInvEntry
        . sortOn (view entityName . snd)
        . elems
      items = (r ^. robotInventory . to (itemList "Inventory"))
           ++ (r ^. installedDevices . to (itemList "Installed devices"))

      -- Attempt to keep the selected element steady.
      sel = mList >>= BL.listSelectedElement  -- Get the currently selected element+index.
      idx = case sel of
        -- If there is no currently selected element, just focus on
        -- index 1 (not 0, to avoid the separator).
        Nothing -> 1
        -- Otherwise, try to find the same entity in the list and focus on that;
        -- if it's not there, keep the index the same.
        Just (selIdx, InventoryEntry _ e) ->
          fromMaybe selIdx (findIndex ((== Just e) . preview (_InventoryEntry . _2)) items)
        Just (selIdx, _) -> selIdx

      -- Create the new list, focused at the desired index.
      lst = BL.listMoveTo idx $ BL.list InventoryList (V.fromList items) 1

  -- Finally, populate the newly created list in the UI, and remember
  -- the hash of the current robot.
  uiInventory .= Just (r ^. robotEntity . entityHash, lst)

handleREPLEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleREPLEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]))
  = continue $ s
      & gameState . robotMap . ix "base" . machine .~ idleMachine
handleREPLEvent s (VtyEvent (V.EvKey V.KEnter []))
  = case processTerm' topCtx topCapCtx entry of
      Right t@(ProcessedTerm _ (Module ty _) _ _) ->
        continue $ s
          & uiState . uiReplForm    %~ updateFormState ""
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

validateREPLForm :: AppState -> AppState
validateREPLForm s = s & uiState . uiReplForm %~ validate
  where
    (topCtx, topCapCtx) = s ^. gameState . robotMap . ix "base" . robotCtx
    validate f = setFieldValid (isRight result) REPLInput f
      where
        result = processTerm' topCtx topCapCtx (formState f)

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

worldScrollDist :: Int
worldScrollDist = 8

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

cycleEnum :: (Eq e, Enum e, Bounded e) => e -> e
cycleEnum e
  | e == maxBound = minBound
  | otherwise     = succ e


scrollView :: AppState -> (V2 Int -> V2 Int) -> EventM Name AppState
scrollView s update =
  updateView $ s & gameState %~ modifyViewCenter update

updateView :: AppState -> EventM Name AppState
updateView s = do
  invalidateCacheEntry WorldCache
  mext <- lookupExtent WorldExtent
  case mext of
    Nothing  -> return s
    Just (Extent _ _ size) -> return $
      s & gameState . world %~ W.loadRegion (viewingRegion (s ^. gameState) size)

keyToDir :: V.Key -> V2 Int
keyToDir V.KUp         = north
keyToDir V.KDown       = south
keyToDir V.KRight      = east
keyToDir V.KLeft       = west
keyToDir (V.KChar 'h') = west
keyToDir (V.KChar 'j') = south
keyToDir (V.KChar 'k') = north
keyToDir (V.KChar 'l') = east
keyToDir _             = V2 0 0

adjustTPS :: (Int -> Int -> Int) -> AppState -> AppState
adjustTPS (+/-) = uiState . lgTicksPerSecond %~ (+/- 1)

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
      l' <- {- BL.handleListEventVi -}
        handleListEventWithSeparators ev (is _Separator) l
      let s' = s & uiState . uiInventory . _Just . _2 .~ l'
      continue s'
handleInfoPanelEvent s _ = continueWithoutRedraw s

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
