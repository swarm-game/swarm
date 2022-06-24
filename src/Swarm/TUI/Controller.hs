{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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
  quitGame,

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
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Linear
import System.Clock
import Witch (into)

import Brick hiding (Direction)
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Dialog
import qualified Brick.Widgets.List as BL
import qualified Graphics.Vty as V

import Brick.Widgets.List (handleListEvent)
import qualified Control.Carrier.Lift as Fused
import qualified Control.Carrier.State.Lazy as Fused
import Swarm.Game.CESK (cancel, emptyStore, initMachine)
import Swarm.Game.Entity hiding (empty)
import Swarm.Game.Robot
import Swarm.Game.Scenario (ScenarioCollection, ScenarioItem (..), scenarioCollectionToList)
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
import Swarm.TUI.View (generateModal)
import Swarm.Util hiding ((<<.=))

-- | Pattern synonyms to simplify brick event handler
pattern Key :: V.Key -> BrickEvent n e
pattern Key k = VtyEvent (V.EvKey k [])

pattern CharKey, ControlKey, MetaKey :: Char -> BrickEvent n e
pattern CharKey c = VtyEvent (V.EvKey (V.KChar c) [])
pattern ControlKey c = VtyEvent (V.EvKey (V.KChar c) [V.MCtrl])
pattern MetaKey c = VtyEvent (V.EvKey (V.KChar c) [V.MMeta])

pattern EscapeKey :: BrickEvent n e
pattern EscapeKey = VtyEvent (V.EvKey V.KEsc [])

pattern FKey :: Int -> BrickEvent n e
pattern FKey c = VtyEvent (V.EvKey (V.KFun c) [])

-- | The top-level event handler for the TUI.
handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent s = case s ^. uiState . uiMenu of
  NoMenu -> handleMainEvent s
  MainMenu l -> handleMainMenuEvent l s
  NewGameMenu l -> handleNewGameMenuEvent l s
  TutorialMenu -> pressAnyKey (MainMenu (mainMenu Tutorial)) s
  AboutMenu -> pressAnyKey (MainMenu (mainMenu About)) s

-- | The event handler for the main menu.
handleMainMenuEvent ::
  BL.List Name MainMenuEntry -> AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleMainMenuEvent menu s = \case
  Key V.KEnter ->
    case snd <$> BL.listSelectedElement menu of
      Nothing -> continueWithoutRedraw s
      Just x0 -> case x0 of
        NewGame ->
          continue $
            s & uiState . uiMenu -- here?
              .~ NewGameMenu (NE.fromList [mkScenarioList (s ^. uiState . uiCheatMode) (s ^. gameState . scenarios)])
        Tutorial -> continue $ s & uiState . uiMenu .~ TutorialMenu
        About -> continue $ s & uiState . uiMenu .~ AboutMenu
        Quit -> halt s
  CharKey 'q' -> halt s
  ControlKey 'q' -> halt s
  VtyEvent ev -> do
    menu' <- handleListEvent ev menu
    continue $ s & uiState . uiMenu .~ MainMenu menu'
  _ -> continueWithoutRedraw s

handleNewGameMenuEvent :: NonEmpty (BL.List Name ScenarioItem) -> AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleNewGameMenuEvent scenarioStack@(curMenu :| rest) s = \case
  Key V.KEnter ->
    case snd <$> BL.listSelectedElement curMenu of
      Nothing -> continueWithoutRedraw s
      Just (SISingle scene) -> do
        let nextMenu
              -- Go back to the scenario list
              | null rest = NewGameMenu scenarioStack
              -- Advance to the next tutorial or challenge
              | otherwise = NewGameMenu (BL.listMoveDown curMenu :| rest)

        s' <- liftIO $ scenarioToAppState scene Nothing Nothing (s & uiState . uiPrevMenu .~ nextMenu)
        continue s'
      Just (SICollection _ c) ->
        continue $
          s & uiState . uiMenu .~ NewGameMenu (NE.cons (mkScenarioList (s ^. uiState . uiCheatMode) c) scenarioStack)
  Key V.KEsc -> exitNewGameMenu s scenarioStack
  CharKey 'q' -> exitNewGameMenu s scenarioStack
  ControlKey 'q' -> halt s
  VtyEvent ev -> do
    menu' <- handleListEvent ev curMenu
    continue $ s & uiState . uiMenu .~ NewGameMenu (menu' :| rest)
  _ -> continueWithoutRedraw s

mkScenarioList :: Bool -> ScenarioCollection -> BL.List Name ScenarioItem
mkScenarioList cheat = flip (BL.list ScenarioList) 1 . V.fromList . filterTest . scenarioCollectionToList
 where
  filterTest = if cheat then id else filter (\case SICollection n _ -> n /= "Testing"; _ -> True)

exitNewGameMenu :: AppState -> NonEmpty (BL.List Name ScenarioItem) -> EventM Name (Next AppState)
exitNewGameMenu s stk =
  continue $
    s & uiState . uiMenu
      .~ case snd (NE.uncons stk) of
        Nothing -> MainMenu (mainMenu NewGame)
        Just stk' -> NewGameMenu stk'

pressAnyKey :: Menu -> AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
pressAnyKey m s (VtyEvent (V.EvKey _ _)) = continue $ s & uiState . uiMenu .~ m
pressAnyKey _ s _ = continueWithoutRedraw s

-- | The top-level event handler while we are running the game itself.
handleMainEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleMainEvent s = \case
  AppEvent Frame
    | s ^. gameState . paused -> continueWithoutRedraw s
    | Just g <- s ^. uiState . uiGoal . to goalNeedsDisplay ->
      toggleModal s (GoalModal g) <&> (uiState . uiGoal %~ markGoalRead) >>= runFrameUI
    | otherwise -> runFrameUI s
  -- ctrl-q works everywhere
  ControlKey 'q' -> toggleModal s QuitModal >>= continue
  VtyEvent (V.EvResize _ _) -> do
    invalidateCacheEntry WorldCache
    continue s
  Key V.KEsc
    | isJust (s ^. uiState . uiError) -> continue $ s & uiState . uiError .~ Nothing
    | isJust (s ^. uiState . uiModal) -> maybeUnpause s >>= (continue . (uiState . uiModal .~ Nothing))
  FKey 1 -> toggleModal s HelpModal >>= continue
  FKey 2 -> toggleModal s RobotsModal >>= continue
  FKey 3 | not (null (s ^. gameState . availableRecipes)) -> do
    s' <- toggleModal s RecipesModal
    continue (s' & gameState . availableRecipesNewCount .~ 0)
  ControlKey 'g' -> case s ^. uiState . uiGoal of
    NoGoal -> continueWithoutRedraw s
    UnreadGoal g -> toggleModal s (GoalModal g) >>= continue
    ReadGoal g -> toggleModal s (GoalModal g) >>= continue
  VtyEvent vev
    | isJust (s ^. uiState . uiModal) -> handleModalEvent s vev
  CharKey '\t' -> continue $ s & uiState . uiFocusRing %~ focusNext
  Key V.KBackTab -> continue $ s & uiState . uiFocusRing %~ focusPrev
  -- special keys that work on all panels
  MetaKey 'w' -> setFocus s WorldPanel
  MetaKey 'e' -> setFocus s RobotPanel
  MetaKey 'r' -> setFocus s REPLPanel
  MetaKey 't' -> setFocus s InfoPanel
  -- toggle creative mode if in "cheat mode"
  ControlKey 'v'
    | s ^. uiState . uiCheatMode -> continue (s & gameState . creativeMode %~ not)
  MouseDown n _ _ mouseLoc ->
    case n of
      WorldPanel -> do
        mouseCoordsM <- mouseLocToWorldCoords (s ^. gameState) mouseLoc
        continue (s & uiState . uiWorldCursor .~ mouseCoordsM)
      REPLPanel ->
        -- Do not clear the world cursor when going back to the REPL
        continueWithoutRedraw s
      _ -> continueWithoutRedraw (s & uiState . uiWorldCursor .~ Nothing)
  MouseUp n _ _mouseLoc -> do
    let s' =
          s & case n of
            InventoryListItem pos -> uiState . uiInventory . traverse . _2 %~ BL.listMoveTo pos
            _ -> id
    setFocus s' $ case n of
      -- Adapt click event origin to their right panel.
      -- For the REPL and the World view, using 'Brick.Widgets.Core.clickable' correctly set the origin.
      -- However this does not seems to work for the robot and info panel.
      -- Thus we force the destination focus here.
      InventoryList -> RobotPanel
      InventoryListItem _ -> RobotPanel
      InfoViewport -> InfoPanel
      _ -> n
  -- dispatch any other events to the focused panel handler
  ev ->
    case focusGetCurrent (s ^. uiState . uiFocusRing) of
      Just REPLPanel -> handleREPLEvent s ev
      Just WorldPanel -> handleWorldEvent s ev
      Just RobotPanel -> handleRobotPanelEvent s ev
      Just InfoPanel -> handleInfoPanelEvent s infoScroll ev
      _ -> continueWithoutRedraw s

mouseLocToWorldCoords :: GameState -> Brick.Location -> EventM Name (Maybe W.Coords)
mouseLocToWorldCoords gs (Brick.Location mouseLoc) = do
  mext <- lookupExtent WorldExtent
  pure $ case mext of
    Nothing -> Nothing
    Just ext ->
      let region = viewingRegion gs (bimap fromIntegral fromIntegral (extentSize ext))
          regionStart = W.unCoords (fst region)
          mouseLoc' = bimap fromIntegral fromIntegral mouseLoc
          mx = snd mouseLoc' + fst regionStart
          my = fst mouseLoc' + snd regionStart
       in Just $ W.Coords (mx, my)

setFocus :: AppState -> Name -> EventM Name (Next AppState)
setFocus s name = continue $ s & uiState . uiFocusRing %~ focusSetCurrent name

-- | Set the game to Running if it was auto paused
maybeUnpause :: AppState -> EventM Name AppState
maybeUnpause s
  | s ^. gameState . runStatus == AutoPause = do
    curTime <- liftIO $ getTime Monotonic
    pure $ s & (gameState . runStatus .~ Running) . resetLastFrameTime curTime
  | otherwise = pure s
 where
  -- When unpausing, it is critical to ensure the next frame doesn't
  -- catch up from the time spent in pause.
  -- TODO: manage unpause more safely to also cover
  -- the world event handler for the KChar 'p'.
  resetLastFrameTime curTime = uiState . lastFrameTime .~ curTime

toggleModal :: AppState -> ModalType -> EventM Name AppState
toggleModal s mt = case s ^. uiState . uiModal of
  Nothing -> pure $ s & (uiState . uiModal ?~ generateModal s mt) . ensurePause
  Just _ -> maybeUnpause s <&> uiState . uiModal .~ Nothing
 where
  -- these modals do not pause the game
  runningModals = [RobotsModal]
  -- Set the game to AutoPause if needed
  ensurePause
    | s ^. gameState . paused || mt `elem` runningModals = id
    | otherwise = case mt of
        RobotsModal -> id
        _ -> gameState . runStatus .~ AutoPause

handleModalEvent :: AppState -> V.Event -> EventM Name (Next AppState)
handleModalEvent s = \case
  V.EvKey V.KEnter [] -> do
    s' <- toggleModal s QuitModal
    case s ^? uiState . uiModal . _Just . modalDialog . to dialogSelection of
      Just (Just Confirm) -> quitGame s'
      _ -> continue s'
  ev -> do
    s' <- s & uiState . uiModal . _Just . modalDialog %%~ handleDialogEvent ev
    case s ^? uiState . uiModal . _Just . modalType of
      Just RecipesModal -> handleInfoPanelEvent s' recipesScroll (VtyEvent ev)
      Just RobotsModal -> handleInfoPanelEvent s' robotsScroll (VtyEvent ev)
      _ -> continue s'

-- | Quit a game.  Currently all it does is write out the updated REPL
--   history to a @.swarm_history@ file, and return to the previous menu.
quitGame :: AppState -> EventM Name (Next AppState)
quitGame s = do
  let hist = mapMaybe getREPLEntry $ getLatestREPLHistoryItems maxBound history
  liftIO $ (`T.appendFile` T.unlines hist) =<< getSwarmHistoryPath True
  case s ^. uiState . uiPrevMenu of
    NoMenu -> halt s
    menu ->
      let s' =
            s & uiState . uiReplHistory %~ restartREPLHistory
              & uiState . uiMenu .~ menu
       in continue s'
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

  shouldUpdate <- use (uiState . uiInventoryShouldUpdate)
  -- If the hashes don't match (either because which robot (or
  -- whether any robot) is focused changed, or the focused robot's
  -- inventory changed), regenerate the list.
  inventoryUpdated <-
    if listRobotHash /= focusedRobotHash || shouldUpdate
      then do
        zoom uiState $ populateInventoryList fr
        (uiState . uiInventoryShouldUpdate) .= False
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
        s <- get
        uiState . uiModal .= Just (generateModal s WinModal)
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
handleREPLEvent s (ControlKey 'c') =
  continue $
    s
      & gameState . robotMap . ix 0 . machine %~ cancel
handleREPLEvent s (Key V.KEnter) =
  if not $ s ^. gameState . replWorking
    then continue $ case entry of
      CmdPrompt uinput ->
        case processTerm' topTypeCtx topCapCtx uinput of
          Right mt ->
            maybe id startBaseProgram mt
              . (uiState . uiReplHistory %~ addREPLItem (REPLEntry uinput))
              . (uiState %~ resetWithREPLForm (set promptUpdateL "" (s ^. uiState)))
              $ s
          Left err -> s & uiState . uiError ?~ err
      SearchPrompt t hist ->
        case lastEntry t hist of
          Nothing -> s & uiState %~ resetWithREPLForm (mkReplForm $ CmdPrompt "")
          Just found
            | T.null t -> s & uiState %~ resetWithREPLForm (mkReplForm $ CmdPrompt "")
            | otherwise ->
              s & uiState %~ resetWithREPLForm (mkReplForm $ CmdPrompt found)
                & validateREPLForm
    else continueWithoutRedraw s
 where
  entry = formState (s ^. uiState . uiReplForm)
  topTypeCtx = s ^. gameState . robotMap . ix 0 . robotContext . defTypes
  topCapCtx = s ^. gameState . robotMap . ix 0 . robotContext . defCaps
  topValCtx = s ^. gameState . robotMap . ix 0 . robotContext . defVals
  topStore =
    fromMaybe emptyStore $
      s ^? gameState . robotMap . at 0 . _Just . robotContext . defStore
  startBaseProgram t@(ProcessedTerm _ (Module ty _) _ _) =
    (gameState . replStatus .~ REPLWorking ty Nothing)
      . (gameState . robotMap . ix 0 . machine .~ initMachine t topValCtx topStore)
      . (gameState %~ execState (activateRobot 0))
handleREPLEvent s (Key V.KUp) =
  continue $ s & adjReplHistIndex Older
handleREPLEvent s (Key V.KDown) =
  continue $ s & adjReplHistIndex Newer
handleREPLEvent s (ControlKey 'r') = continue $
  case s ^. uiState . uiReplForm . to formState of
    CmdPrompt uinput ->
      let newform = mkReplForm $ SearchPrompt uinput (s ^. uiState . uiReplHistory)
       in s & uiState . uiReplForm .~ newform
    SearchPrompt ftext rh -> case lastEntry ftext rh of
      Nothing -> s
      Just found ->
        let newform = mkReplForm $ SearchPrompt ftext (removeEntry found rh)
         in s & uiState . uiReplForm .~ newform
handleREPLEvent s EscapeKey =
  case s ^. uiState . uiReplForm . to formState of
    CmdPrompt _ -> continueWithoutRedraw s
    SearchPrompt _ _ ->
      continue $ s & uiState %~ resetWithREPLForm (mkReplForm $ CmdPrompt "")
handleREPLEvent s ev = do
  f' <- handleFormEvent ev (s ^. uiState . uiReplForm)
  continue $
    case formState f' of
      CmdPrompt _ -> validateREPLForm (s & uiState . uiReplForm .~ f')
      SearchPrompt t _ ->
        let newform = set promptUpdateL t (s ^. uiState)
         in s & uiState . uiReplForm .~ newform

-- | Validate the REPL input when it changes: see if it parses and
--   typechecks, and set the color accordingly.
validateREPLForm :: AppState -> AppState
validateREPLForm s =
  case replPrompt of
    CmdPrompt uinput ->
      let result = processTerm' topTypeCtx topCapCtx uinput
          theType = case result of
            Right (Just (ProcessedTerm _ (Module ty _) _ _)) -> Just ty
            _ -> Nothing
       in s & uiState . uiReplForm %~ validate result
            & uiState . uiReplType .~ theType
    SearchPrompt _ _ -> s
 where
  replPrompt = s ^. uiState . uiReplForm . to formState
  topTypeCtx = s ^. gameState . robotMap . ix 0 . robotContext . defTypes
  topCapCtx = s ^. gameState . robotMap . ix 0 . robotContext . defCaps
  validate result = setFieldValid (isRight result) REPLInput

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
  saveLastEntry = uiState . uiReplLast .~ (s ^. uiState . uiReplForm . to formState . promptTextL)
  showNewEntry = uiState . uiReplForm %~ updateFormState (CmdPrompt newEntry)
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
handleWorldEvent s = \case
  Key k
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
                 ] ->
      scrollView s (^+^ (worldScrollDist *^ keyToDir k)) >>= continue
  CharKey 'c' -> do
    invalidateCacheEntry WorldCache
    continue $ s & gameState . viewCenterRule .~ VCRobot 0

  -- pausing and stepping
  CharKey 'p' -> do
    curTime <- liftIO $ getTime Monotonic
    continue $
      s
        & gameState . runStatus %~ (\status -> if status == Running then ManualPause else Running)
        -- Also reset the last frame time to now. If we are pausing, it
        -- doesn't matter; if we are unpausing, this is critical to
        -- ensure the next frame doesn't think it has to catch up from
        -- whenever the game was paused!
        & uiState . lastFrameTime .~ curTime
  CharKey 's'
    | s ^. gameState . paused -> runGameTickUI s
    | otherwise -> continueWithoutRedraw s
  -- speed controls
  CharKey '<' -> continue $ adjustTPS (-) s
  CharKey '>' -> continue $ adjustTPS (+) s
  CharKey ',' -> continue $ adjustTPS (-) s
  CharKey '.' -> continue $ adjustTPS (+) s
  -- show fps
  CharKey 'f' -> continue $ s & uiState . uiShowFPS %~ not
  -- Fall-through case: don't do anything.
  _ -> continueWithoutRedraw s

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
handleRobotPanelEvent s (Key V.KEnter) =
  maybe (continueWithoutRedraw s) (descriptionModal s) (focusedEntity s)
handleRobotPanelEvent s (CharKey 'm') =
  maybe (continueWithoutRedraw s) (makeEntity s) (focusedEntity s)
handleRobotPanelEvent s (CharKey '0') = do
  continue $ s & (uiState . uiShowZero %~ not) . (uiState . uiInventoryShouldUpdate .~ True)
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
          s ^? gameState . robotMap . at 0 . _Just . robotContext . defStore

  case isActive <$> (s ^. gameState . robotMap . at 0) of
    Just False ->
      continue $
        s
          & gameState . replStatus .~ REPLWorking mkTy Nothing
          & gameState . robotMap . ix 0 . machine .~ initMachine mkPT empty topStore
          & gameState %~ execState (activateRobot 0)
    _ -> continueWithoutRedraw s

-- | Display a modal window with the description of an entity.
descriptionModal :: AppState -> Entity -> EventM Name (Next AppState)
descriptionModal s e =
  continue $ s & uiState . uiModal ?~ generateModal s (DescriptionModal e)

------------------------------------------------------------
-- Info panel events
------------------------------------------------------------

-- | Handle user events in the info panel (just scrolling).
handleInfoPanelEvent :: AppState -> ViewportScroll Name -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleInfoPanelEvent s vs = \case
  Key V.KDown -> vScrollBy vs 1 >> continue s
  Key V.KUp -> vScrollBy vs (-1) >> continue s
  CharKey 'k' -> vScrollBy vs 1 >> continue s
  CharKey 'j' -> vScrollBy vs (-1) >> continue s
  Key V.KPageDown -> vScrollPage vs Brick.Down >> continue s
  Key V.KPageUp -> vScrollPage vs Brick.Up >> continue s
  Key V.KHome -> vScrollToBeginning vs >> continue s
  Key V.KEnd -> vScrollToEnd vs >> continue s
  _ -> continueWithoutRedraw s
