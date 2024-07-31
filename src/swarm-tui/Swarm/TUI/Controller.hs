{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Event handlers for the TUI.
module Swarm.TUI.Controller (
  -- * Event handling
  handleEvent,
  quitGame,

  -- ** Handling 'Swarm.TUI.Model.Frame' events
  runFrameUI,
  ticksPerFrameCap,
  runGameTickUI,

  -- ** REPL panel
  runBaseWebCode,
  handleREPLEvent,
  validateREPLForm,
  adjReplHistIndex,
  TimeDir (..),

  -- ** Info panel
  handleInfoPanelEvent,
) where

-- See Note [liftA2 re-export from Prelude]
import Prelude hiding (Applicative (..))

import Brick hiding (Direction, Location)
import Brick.Focus
import Brick.Keybindings qualified as B
import Brick.Widgets.Dialog
import Brick.Widgets.Edit (Editor, applyEdit, handleEditorEvent)
import Brick.Widgets.List (handleListEvent)
import Brick.Widgets.List qualified as BL
import Control.Applicative (pure)
import Control.Category ((>>>))
import Control.Lens as Lens
import Control.Monad (unless, void, when)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState, execState)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Zipper qualified as TZ
import Data.Text.Zipper.Generic.Words qualified as TZ
import Data.Vector qualified as V
import Graphics.Vty qualified as V
import Swarm.Game.Achievement.Definitions
import Swarm.Game.CESK (CESK (Out), Frame (FApp, FExec, FSuspend))
import Swarm.Game.Entity hiding (empty)
import Swarm.Game.Land
import Swarm.Game.ResourceLoading (getSwarmHistoryPath)
import Swarm.Game.Robot.Concrete
import Swarm.Game.ScenarioInfo
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Robot
import Swarm.Game.State.Runtime
import Swarm.Game.State.Substate
import Swarm.Language.Capability (
  Capability (CGod),
  constCaps,
 )
import Swarm.Language.Context
import Swarm.Language.Key (KeyCombo, mkKeyCombo)
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig)
import Swarm.Language.Parser.Lex (reservedWords)
import Swarm.Language.Parser.Util (showErrorPos)
import Swarm.Language.Pipeline (processParsedTerm', processTerm')
import Swarm.Language.Syntax hiding (Key)
import Swarm.Language.Typecheck (
  ContextualTypeErr (..),
 )
import Swarm.Language.Value (Value (VKey), envTypes)
import Swarm.Log
import Swarm.TUI.Controller.EventHandlers
import Swarm.TUI.Controller.SaveScenario (saveScenarioInfoOnQuit)
import Swarm.TUI.Controller.Util
import Swarm.TUI.Editor.Controller qualified as EC
import Swarm.TUI.Editor.Model
import Swarm.TUI.Launch.Controller
import Swarm.TUI.Launch.Model
import Swarm.TUI.Launch.Prep (prepareLaunchDialog)
import Swarm.TUI.List
import Swarm.TUI.Model
import Swarm.TUI.Model.Goal
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Popup (progressPopups)
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.StateUpdate
import Swarm.TUI.Model.Structure
import Swarm.TUI.Model.UI
import Swarm.Util hiding (both, (<<.=))
import Swarm.Version (NewReleaseFailure (..))

-- ~~~~ Note [liftA2 re-export from Prelude]
--
-- As of base-4.18 (GHC 9.6), liftA2 is re-exported from Prelude.  See
-- https://github.com/haskell/core-libraries-committee/issues/50 .  In
-- order to compile warning-free on both GHC 9.6 and older versions,
-- we hide the import of Applicative functions from Prelude and import
-- explicitly from Control.Applicative.  In theory, if at some point
-- in the distant future we end up dropping support for GHC < 9.6 then
-- we could get rid of both explicit imports and just get liftA2 and
-- pure implicitly from Prelude.

-- | The top-level event handler for the TUI.
handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent = \case
  -- the query for upstream version could finish at any time, so we have to handle it here
  AppEvent (UpstreamVersion ev) -> do
    let logReleaseEvent l sev e = runtimeState . eventLog %= logEvent l sev "Release" (T.pack $ show e)
    case ev of
      Left e ->
        let sev = case e of
              FailedReleaseQuery {} -> Error
              OnDevelopmentBranch {} -> Info
              _ -> Warning
         in logReleaseEvent SystemLog sev e
      Right _ -> pure ()
    runtimeState . upstreamRelease .= ev
  e -> do
    -- Handle popup display at the very top level, so it is
    -- unaffected by any other state, e.g. even when starting or
    -- quitting a game, moving around the menu, the popup
    -- display will continue as normal.
    upd <- case e of
      AppEvent Frame -> Brick.zoom (uiState . uiPopups) progressPopups
      _ -> pure False

    s <- get
    if s ^. uiState . uiPlaying
      then handleMainEvent upd e
      else do
        e & case s ^. uiState . uiMenu of
          -- If we reach the NoMenu case when uiPlaying is False, just
          -- quit the app.  We should actually never reach this code (the
          -- quitGame function would have already halted the app).
          NoMenu -> const halt
          MainMenu l -> handleMainMenuEvent l
          NewGameMenu l ->
            if s ^. uiState . uiLaunchConfig . controls . fileBrowser . fbIsDisplayed
              then handleFBEvent
              else case s ^. uiState . uiLaunchConfig . controls . isDisplayedFor of
                Nothing -> handleNewGameMenuEvent l
                Just siPair -> handleLaunchOptionsEvent siPair
          MessagesMenu -> handleMainMessagesEvent
          AchievementsMenu l -> handleMainAchievementsEvent l
          AboutMenu -> pressAnyKey (MainMenu (mainMenu About))

-- | The event handler for the main menu.
--
-- TODO: #2010 Finish porting Controller to KeyEventHandlers
handleMainMenuEvent ::
  BL.List Name MainMenuEntry -> BrickEvent Name AppEvent -> EventM Name AppState ()
handleMainMenuEvent menu = \case
  Key V.KEnter ->
    case snd <$> BL.listSelectedElement menu of
      Nothing -> pure ()
      Just x0 -> case x0 of
        NewGame -> do
          cheat <- use $ uiState . uiCheatMode
          ss <- use $ runtimeState . scenarios
          uiState . uiMenu .= NewGameMenu (pure $ mkScenarioList cheat ss)
        Tutorial -> do
          -- Set up the menu stack as if the user had chosen "New Game > Tutorials"
          cheat <- use $ uiState . uiCheatMode
          ss <- use $ runtimeState . scenarios
          let tutorialCollection = getTutorials ss
              topMenu =
                BL.listFindBy
                  ((== tutorialsDirname) . T.unpack . scenarioItemName)
                  (mkScenarioList cheat ss)
              tutorialMenu = mkScenarioList cheat tutorialCollection
              menuStack = tutorialMenu :| pure topMenu
          uiState . uiMenu .= NewGameMenu menuStack

          -- Extract the first tutorial challenge and run it
          let firstTutorial = case scOrder tutorialCollection of
                Just (t : _) -> case M.lookup t (scMap tutorialCollection) of
                  Just (SISingle siPair) -> siPair
                  _ -> error "No first tutorial found!"
                _ -> error "No first tutorial found!"
          startGame firstTutorial Nothing
        Achievements -> uiState . uiMenu .= AchievementsMenu (BL.list AchievementList (V.fromList listAchievements) 1)
        Messages -> do
          runtimeState . eventLog . notificationsCount .= 0
          uiState . uiMenu .= MessagesMenu
        About -> do
          uiState . uiMenu .= AboutMenu
          attainAchievement $ GlobalAchievement LookedAtAboutScreen
        Quit -> halt
  CharKey 'q' -> halt
  ControlChar 'q' -> halt
  VtyEvent ev -> do
    menu' <- nestEventM' menu (handleListEvent ev)
    uiState . uiMenu .= MainMenu menu'
  _ -> pure ()

-- | If we are in a New Game menu, advance the menu to the next item in order.
--
--   NOTE: be careful to maintain the invariant that the currently selected
--   menu item is always the same as the currently played scenario!  `quitGame`
--   is the only place this function should be called.
advanceMenu :: Menu -> Menu
advanceMenu = _NewGameMenu . ix 0 %~ BL.listMoveDown

handleMainAchievementsEvent ::
  BL.List Name CategorizedAchievement ->
  BrickEvent Name AppEvent ->
  EventM Name AppState ()
handleMainAchievementsEvent l e = case e of
  Key V.KEsc -> returnToMainMenu
  CharKey 'q' -> returnToMainMenu
  ControlChar 'q' -> returnToMainMenu
  VtyEvent ev -> do
    l' <- nestEventM' l (handleListEvent ev)
    uiState . uiMenu .= AchievementsMenu l'
  _ -> pure ()
 where
  returnToMainMenu = uiState . uiMenu .= MainMenu (mainMenu Messages)

handleMainMessagesEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleMainMessagesEvent = \case
  Key V.KEsc -> returnToMainMenu
  CharKey 'q' -> returnToMainMenu
  ControlChar 'q' -> returnToMainMenu
  _ -> pure ()
 where
  returnToMainMenu = uiState . uiMenu .= MainMenu (mainMenu Messages)

-- TODO: #2010 Finish porting Controller to KeyEventHandlers
handleNewGameMenuEvent ::
  NonEmpty (BL.List Name ScenarioItem) ->
  BrickEvent Name AppEvent ->
  EventM Name AppState ()
handleNewGameMenuEvent scenarioStack@(curMenu :| rest) = \case
  Key V.KEnter ->
    case snd <$> BL.listSelectedElement curMenu of
      Nothing -> pure ()
      Just (SISingle siPair) -> invalidateCache >> startGame siPair Nothing
      Just (SICollection _ c) -> do
        cheat <- use $ uiState . uiCheatMode
        uiState . uiMenu .= NewGameMenu (NE.cons (mkScenarioList cheat c) scenarioStack)
  CharKey 'o' -> showLaunchDialog
  CharKey 'O' -> showLaunchDialog
  Key V.KEsc -> exitNewGameMenu scenarioStack
  CharKey 'q' -> exitNewGameMenu scenarioStack
  ControlChar 'q' -> halt
  VtyEvent ev -> do
    menu' <- nestEventM' curMenu (handleListEvent ev)
    uiState . uiMenu .= NewGameMenu (menu' :| rest)
  _ -> pure ()
 where
  showLaunchDialog = case snd <$> BL.listSelectedElement curMenu of
    Just (SISingle siPair) -> Brick.zoom (uiState . uiLaunchConfig) $ prepareLaunchDialog siPair
    _ -> pure ()

exitNewGameMenu :: NonEmpty (BL.List Name ScenarioItem) -> EventM Name AppState ()
exitNewGameMenu stk = do
  uiState
    . uiMenu
    .= case snd (NE.uncons stk) of
      Nothing -> MainMenu (mainMenu NewGame)
      Just stk' -> NewGameMenu stk'

pressAnyKey :: Menu -> BrickEvent Name AppEvent -> EventM Name AppState ()
pressAnyKey m (VtyEvent (V.EvKey _ _)) = uiState . uiMenu .= m
pressAnyKey _ _ = pure ()

-- | The top-level event handler while we are running the game itself.
handleMainEvent :: Bool -> BrickEvent Name AppEvent -> EventM Name AppState ()
handleMainEvent forceRedraw ev = do
  s <- get
  let keyHandler = s ^. keyEventHandling . keyDispatchers . to mainGameDispatcher
  case ev of
    AppEvent ae -> case ae of
      Frame
        -- If the game is paused, don't run any game ticks, but do redraw the screen
        -- if a redraw is forced.
        | s ^. gameState . temporal . paused -> unless forceRedraw continueWithoutRedraw
        | otherwise -> runFrameUI forceRedraw
      Web (RunWebCode c) -> runBaseWebCode c
      _ -> continueWithoutRedraw
    VtyEvent (V.EvResize _ _) -> invalidateCache
    EscapeKey | Just m <- s ^. uiState . uiGameplay . uiModal -> closeModal m
    -- Pass to key handler (allows users to configure bindings)
    -- See Note [how Swarm event handlers work]
    VtyEvent (V.EvKey k m)
      | isJust (B.lookupVtyEvent k m keyHandler) -> void $ B.handleKey keyHandler k m
    -- pass keys on to modal event handler if a modal is open
    VtyEvent vev
      | isJust (s ^. uiState . uiGameplay . uiModal) -> handleModalEvent vev
    MouseDown (TerrainListItem pos) V.BLeft _ _ ->
      uiState . uiGameplay . uiWorldEditor . terrainList %= BL.listMoveTo pos
    MouseDown (EntityPaintListItem pos) V.BLeft _ _ ->
      uiState . uiGameplay . uiWorldEditor . entityPaintList %= BL.listMoveTo pos
    MouseDown WorldPositionIndicator _ _ _ -> uiState . uiGameplay . uiWorldCursor .= Nothing
    MouseDown (FocusablePanel WorldPanel) V.BMiddle _ mouseLoc ->
      -- Eye Dropper tool
      EC.handleMiddleClick mouseLoc
    MouseDown (FocusablePanel WorldPanel) V.BRight _ mouseLoc ->
      -- Eraser tool
      EC.handleRightClick mouseLoc
    MouseDown (FocusablePanel WorldPanel) V.BLeft [V.MCtrl] mouseLoc ->
      -- Paint with the World Editor
      EC.handleCtrlLeftClick mouseLoc
    MouseDown n _ _ mouseLoc ->
      case n of
        FocusablePanel WorldPanel -> do
          mouseCoordsM <- Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
          shouldUpdateCursor <- EC.updateAreaBounds mouseCoordsM
          when shouldUpdateCursor $
            uiState . uiGameplay . uiWorldCursor .= mouseCoordsM
        REPLInput -> handleREPLEvent ev
        _ -> continueWithoutRedraw
    MouseUp n _ _mouseLoc -> do
      case n of
        InventoryListItem pos -> uiState . uiGameplay . uiInventory . uiInventoryList . traverse . _2 %= BL.listMoveTo pos
        x@(WorldEditorPanelControl y) -> do
          uiState . uiGameplay . uiWorldEditor . editorFocusRing %= focusSetCurrent x
          EC.activateWorldEditorFunction y
        _ -> return ()
      flip whenJust setFocus $ case n of
        -- Adapt click event origin to the right panel.  For the world
        -- view, we just use 'Brick.Widgets.Core.clickable'.  However,
        -- the other panels all have a viewport, requiring us to
        -- explicitly set their focus here.
        InventoryList -> Just RobotPanel
        InventoryListItem _ -> Just RobotPanel
        InfoViewport -> Just InfoPanel
        REPLViewport -> Just REPLPanel
        REPLInput -> Just REPLPanel
        WorldEditorPanelControl _ -> Just WorldEditorPanel
        _ -> Nothing
      case n of
        FocusablePanel x -> setFocus x
        _ -> return ()
    -- dispatch any other events to the focused panel handler
    _ev -> do
      fring <- use $ uiState . uiGameplay . uiFocusRing
      case focusGetCurrent fring of
        Just (FocusablePanel x) -> case x of
          REPLPanel -> handleREPLEvent ev
          -- Pass to key handler (allows users to configure bindings)
          -- See Note [how Swarm event handlers work]
          WorldPanel | VtyEvent (V.EvKey k m) <- ev -> do
            wh <- use $ keyEventHandling . keyDispatchers . to worldDispatcher
            void $ B.handleKey wh k m
          WorldPanel | otherwise -> continueWithoutRedraw
          WorldEditorPanel -> EC.handleWorldEditorPanelEvent ev
          RobotPanel -> handleRobotPanelEvent ev
          InfoPanel -> handleInfoPanelEvent infoScroll ev
        _ -> continueWithoutRedraw

closeModal :: Modal -> EventM Name AppState ()
closeModal m = do
  safeAutoUnpause
  uiState . uiGameplay . uiModal .= Nothing
  -- message modal is not autopaused, so update notifications when leaving it
  when ((m ^. modalType) == MessagesModal) $ do
    t <- use $ gameState . temporal . ticks
    gameState . messageInfo . lastSeenMessageTime .= t

-- TODO: #2010 Finish porting Controller to KeyEventHandlers
handleModalEvent :: V.Event -> EventM Name AppState ()
handleModalEvent = \case
  V.EvKey V.KEnter [] -> do
    mdialog <- preuse $ uiState . uiGameplay . uiModal . _Just . modalDialog
    toggleModal QuitModal
    case dialogSelection =<< mdialog of
      Just (Button QuitButton, _) -> quitGame
      Just (Button KeepPlayingButton, _) -> toggleModal KeepPlayingModal
      Just (Button StartOverButton, StartOver currentSeed siPair) -> do
        invalidateCache
        restartGame currentSeed siPair
      Just (Button NextButton, Next siPair) -> do
        quitGame
        invalidateCache
        startGame siPair Nothing
      _ -> return ()
  ev -> do
    Brick.zoom (uiState . uiGameplay . uiModal . _Just . modalDialog) (handleDialogEvent ev)
    modal <- preuse $ uiState . uiGameplay . uiModal . _Just . modalType
    case modal of
      Just TerrainPaletteModal ->
        refreshList $ uiState . uiGameplay . uiWorldEditor . terrainList
      Just EntityPaletteModal -> do
        refreshList $ uiState . uiGameplay . uiWorldEditor . entityPaintList
      Just GoalModal -> case ev of
        V.EvKey (V.KChar '\t') [] -> uiState . uiGameplay . uiGoal . focus %= focusNext
        _ -> do
          focused <- use $ uiState . uiGameplay . uiGoal . focus
          case focusGetCurrent focused of
            Just (GoalWidgets w) -> case w of
              ObjectivesList -> do
                lw <- use $ uiState . uiGameplay . uiGoal . listWidget
                newList <- refreshGoalList lw
                uiState . uiGameplay . uiGoal . listWidget .= newList
              GoalSummary -> handleInfoPanelEvent modalScroll (VtyEvent ev)
            _ -> handleInfoPanelEvent modalScroll (VtyEvent ev)
      Just StructuresModal -> case ev of
        V.EvKey (V.KChar '\t') [] -> uiState . uiGameplay . uiStructure . structurePanelFocus %= focusNext
        _ -> do
          focused <- use $ uiState . uiGameplay . uiStructure . structurePanelFocus
          case focusGetCurrent focused of
            Just (StructureWidgets w) -> case w of
              StructuresList ->
                refreshList $ uiState . uiGameplay . uiStructure . structurePanelListWidget
              StructureSummary -> handleInfoPanelEvent modalScroll (VtyEvent ev)
            _ -> handleInfoPanelEvent modalScroll (VtyEvent ev)
      _ -> handleInfoPanelEvent modalScroll (VtyEvent ev)
   where
    refreshGoalList lw = nestEventM' lw $ handleListEventWithSeparators ev shouldSkipSelection
    refreshList z = Brick.zoom z $ BL.handleListEvent ev

-- | Quit a game.
--
-- * writes out the updated REPL history to a @.swarm_history@ file
-- * saves current scenario status (InProgress/Completed)
-- * advances the menu to the next scenario IF the current one was won
-- * returns to the previous menu
quitGame :: EventM Name AppState ()
quitGame = do
  -- Write out REPL history.
  history <- use $ uiState . uiGameplay . uiREPL . replHistory
  let hist = mapMaybe getREPLSubmitted $ getLatestREPLHistoryItems maxBound history
  liftIO $ (`T.appendFile` T.unlines hist) =<< getSwarmHistoryPath True

  -- Save scenario status info.
  saveScenarioInfoOnQuit

  -- Automatically advance the menu to the next scenario iff the
  -- player has won the current one.
  wc <- use $ gameState . winCondition
  case wc of
    WinConditions (Won _ _) _ -> uiState . uiMenu %= advanceMenu
    _ -> return ()

  -- Either quit the entire app (if the scenario was chosen directly
  -- from the command line) or return to the menu (if the scenario was
  -- chosen from the menu).
  menu <- use $ uiState . uiMenu
  case menu of
    NoMenu -> halt
    _ -> uiState . uiPlaying .= False

------------------------------------------------------------
-- REPL events
------------------------------------------------------------

-- | Handle a user input event for the REPL.
handleREPLEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleREPLEvent x = do
  s <- get
  let controlMode = s ^. uiState . uiGameplay . uiREPL . replControlMode
  let keyHandler = s ^. keyEventHandling . keyDispatchers . to replDispatcher
  case x of
    -- Pass to key handler (allows users to configure bindings)
    -- See Note [how Swarm event handlers work]
    VtyEvent (V.EvKey k m)
      | isJust (B.lookupVtyEvent k m keyHandler) ->
          void $ B.handleKey keyHandler k m
    -- Handle other events in a way appropriate to the current REPL
    -- control mode.
    _ -> case controlMode of
      Typing -> handleREPLEventTyping x
      Piloting -> handleREPLEventPiloting x
      Handling -> case x of
        -- Handle keypresses using the custom installed handler
        VtyEvent (V.EvKey k mods) -> runInputHandler (mkKeyCombo mods k)
        -- Handle all other events normally
        _ -> handleREPLEventTyping x

-- | Run the installed input handler on a key combo entered by the user.
runInputHandler :: KeyCombo -> EventM Name AppState ()
runInputHandler kc = do
  mhandler <- use $ gameState . gameControls . inputHandler
  case mhandler of
    -- Shouldn't be possible to get here if there is no input handler, but
    -- if we do somehow, just do nothing.
    Nothing -> return ()
    Just (_, handler) -> do
      -- Make sure the base is currently idle; if so, apply the
      -- installed input handler function to a `key` value
      -- representing the typed input.
      working <- use $ gameState . gameControls . replWorking
      unless working $ do
        s <- get
        let env = s ^. gameState . baseEnv
            store = s ^. gameState . baseStore
            handlerCESK = Out (VKey kc) store [FApp handler, FExec, FSuspend env]
        gameState . baseRobot . machine .= handlerCESK
        gameState %= execState (zoomRobots $ activateRobot 0)

-- | Handle a user "piloting" input event for the REPL.
--
-- TODO: #2010 Finish porting Controller to KeyEventHandlers
handleREPLEventPiloting :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleREPLEventPiloting x = case x of
  Key V.KUp -> inputCmd "move"
  Key V.KDown -> inputCmd "turn back"
  Key V.KLeft -> inputCmd "turn left"
  Key V.KRight -> inputCmd "turn right"
  ShiftKey V.KUp -> inputCmd "turn north"
  ShiftKey V.KDown -> inputCmd "turn south"
  ShiftKey V.KLeft -> inputCmd "turn west"
  ShiftKey V.KRight -> inputCmd "turn east"
  Key V.KDel -> inputCmd "selfdestruct"
  CharKey 'g' -> inputCmd "grab"
  CharKey 'h' -> inputCmd "harvest"
  CharKey 'd' -> inputCmd "drill forward"
  CharKey 'x' -> inputCmd "drill down"
  CharKey 's' -> inputCmd "scan forward"
  CharKey 'b' -> inputCmd "blocked"
  CharKey 'u' -> inputCmd "upload base"
  CharKey 'p' -> inputCmd "push"
  _ -> inputCmd "noop"
 where
  inputCmd cmdText = do
    uiState . uiGameplay . uiREPL %= setCmd (cmdText <> ";")
    modify validateREPLForm
    handleREPLEventTyping $ Key V.KEnter

  setCmd nt theRepl =
    theRepl
      & replPromptText .~ nt
      & replPromptType .~ CmdPrompt []

runBaseWebCode :: (MonadState AppState m) => T.Text -> m ()
runBaseWebCode uinput = do
  s <- get
  unless (s ^. gameState . gameControls . replWorking) $
    runBaseCode uinput

runBaseCode :: (MonadState AppState m) => T.Text -> m ()
runBaseCode uinput = do
  addREPLHistItem (mkREPLSubmission uinput)
  resetREPL "" (CmdPrompt [])
  env <- use $ gameState . baseEnv
  case processTerm' env uinput of
    Right mt -> do
      uiState . uiGameplay . uiREPL . replHistory . replHasExecutedManualInput .= True
      runBaseTerm mt
    Left err -> do
      addREPLHistItem (mkREPLError err)

-- | Handle a user input event for the REPL.
--
-- TODO: #2010 Finish porting Controller to KeyEventHandlers
handleREPLEventTyping :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleREPLEventTyping = \case
  -- Scroll the REPL on PageUp or PageDown
  Key V.KPageUp -> vScrollPage replScroll Brick.Up
  Key V.KPageDown -> vScrollPage replScroll Brick.Down
  k -> do
    -- On any other key event, jump to the bottom of the REPL then handle the event
    vScrollToEnd replScroll
    case k of
      MetaChar 'm' ->
        Brick.zoom (uiState . uiGameplay . uiREPL . replPromptEditor) $
          handleEditorEvent (Key V.KEnter)
      Key V.KEnter -> do
        s <- get
        let theRepl = s ^. uiState . uiGameplay . uiREPL
            uinput = theRepl ^. replPromptText

        if not $ s ^. gameState . gameControls . replWorking
          then case theRepl ^. replPromptType of
            CmdPrompt _ -> do
              runBaseCode uinput
              invalidateCacheEntry REPLHistoryCache
            SearchPrompt hist ->
              case lastEntry uinput hist of
                Nothing -> resetREPL "" (CmdPrompt [])
                Just found
                  | T.null uinput -> resetREPL "" (CmdPrompt [])
                  | otherwise -> do
                      resetREPL found (CmdPrompt [])
                      modify validateREPLForm
          else continueWithoutRedraw
      Key V.KUp -> modify $ adjReplHistIndex Older
      Key V.KDown -> do
        repl <- use $ uiState . uiGameplay . uiREPL
        let hist = repl ^. replHistory
            uinput = repl ^. replPromptText
        case repl ^. replPromptType of
          CmdPrompt {}
            | hist ^. replIndex == replLength hist && not (T.null uinput) ->
                -- Special case for hitting "Down" arrow while entering a new non-empty input:
                -- save the input in the history and make the REPL blank.
                do
                  addREPLHistItem (mkREPLSaved uinput)
                  resetREPL "" (CmdPrompt [])
                  modify validateREPLForm
          -- Otherwise, just move around in the history as normal.
          _ -> modify $ adjReplHistIndex Newer
      ControlChar 'r' -> do
        s <- get
        let uinput = s ^. uiState . uiGameplay . uiREPL . replPromptText
        case s ^. uiState . uiGameplay . uiREPL . replPromptType of
          CmdPrompt _ -> uiState . uiGameplay . uiREPL . replPromptType .= SearchPrompt (s ^. uiState . uiGameplay . uiREPL . replHistory)
          SearchPrompt rh -> case lastEntry uinput rh of
            Nothing -> pure ()
            Just found -> uiState . uiGameplay . uiREPL . replPromptType .= SearchPrompt (removeEntry found rh)
      CharKey '\t' -> do
        s <- get
        let names = s ^.. gameState . baseEnv . envTypes . to assocs . traverse . _1
        uiState . uiGameplay . uiREPL %= tabComplete (CompletionContext (s ^. gameState . creativeMode)) names (s ^. gameState . landscape . terrainAndEntities . entityMap)
        modify validateREPLForm
      EscapeKey -> do
        formSt <- use $ uiState . uiGameplay . uiREPL . replPromptType
        case formSt of
          CmdPrompt {} -> continueWithoutRedraw
          SearchPrompt _ -> resetREPL "" (CmdPrompt [])
      ControlChar 'd' -> do
        text <- use $ uiState . uiGameplay . uiREPL . replPromptText
        if text == T.empty
          then toggleModal QuitModal
          else continueWithoutRedraw
      MetaKey V.KBS ->
        uiState . uiGameplay . uiREPL . replPromptEditor %= applyEdit TZ.deletePrevWord
      -- finally if none match pass the event to the editor
      ev -> do
        Brick.zoom (uiState . uiGameplay . uiREPL . replPromptEditor) $ case ev of
          CharKey c | c `elem` ("([{" :: String) -> insertMatchingPair c
          _ -> handleEditorEvent ev
        uiState . uiGameplay . uiREPL . replPromptType %= \case
          CmdPrompt _ -> CmdPrompt [] -- reset completions on any event passed to editor
          SearchPrompt a -> SearchPrompt a
        modify validateREPLForm

insertMatchingPair :: Char -> EventM Name (Editor Text Name) ()
insertMatchingPair c = modify . applyEdit $ TZ.insertChar c >>> TZ.insertChar (close c) >>> TZ.moveLeft
 where
  close = \case
    '(' -> ')'
    '[' -> ']'
    '{' -> '}'
    _ -> c

data CompletionType
  = FunctionName
  | EntityName
  deriving (Eq)

newtype CompletionContext = CompletionContext {ctxCreativeMode :: Bool}
  deriving (Eq)

-- | Reserved words corresponding to commands that can only be used in
--   creative mode.  We only autocomplete to these when in creative mode.
creativeWords :: Set Text
creativeWords =
  S.fromList
    . map (syntax . constInfo)
    . filter (\w -> constCaps w == Just CGod)
    $ allConst

-- | Try to complete the last word in a partially-entered REPL prompt using
--   reserved words and names in scope (in the case of function names) or
--   entity names (in the case of string literals).
tabComplete :: CompletionContext -> [Var] -> EntityMap -> REPLState -> REPLState
tabComplete CompletionContext {..} names em theRepl = case theRepl ^. replPromptType of
  SearchPrompt _ -> theRepl
  CmdPrompt mms
    -- Case 1: If completion candidates have already been
    -- populated via case (3), cycle through them.
    -- Note that tabbing through the candidates *does* update the value
    -- of "t", which one might think would narrow the candidate list
    -- to only that match and therefore halt the cycling.
    -- However, the candidate list only gets recomputed (repopulated)
    -- if the user subsequently presses a non-Tab key. Thus the current
    -- value of "t" is ignored for all Tab presses subsequent to the
    -- first.
    | (m : ms) <- mms -> setCmd (replacementFunc m) (ms ++ [m])
    -- Case 2: Require at least one letter to be typed in order to offer completions for
    -- function names.
    -- We allow suggestions for Entity Name strings without anything having been typed.
    | T.null lastWord && completionType == FunctionName -> setCmd t []
    -- Case 3: Typing another character in the REPL clears the completion candidates from
    -- the CmdPrompt, so when Tab is pressed again, this case then gets executed and
    -- repopulates them.
    | otherwise -> case candidateMatches of
        [] -> setCmd t []
        [m] -> setCmd (completeWith m) []
        -- Perform completion with the first candidate, then populate the list
        -- of all candidates with the current completion moved to the back
        -- of the queue.
        (m : ms) -> setCmd (completeWith m) (ms ++ [m])
 where
  -- checks the "parity" of the number of quotes. If odd, then there is an open quote.
  hasOpenQuotes = (== 1) . (`mod` 2) . T.count "\""

  completionType =
    if hasOpenQuotes t
      then EntityName
      else FunctionName

  replacementFunc = T.append $ T.dropWhileEnd replacementBoundaryPredicate t
  completeWith m = T.append t $ T.drop (T.length lastWord) m
  lastWord = T.takeWhileEnd replacementBoundaryPredicate t
  candidateMatches = filter (lastWord `T.isPrefixOf`) replacementCandidates

  (replacementCandidates, replacementBoundaryPredicate) = case completionType of
    EntityName -> (entityNames, (/= '"'))
    FunctionName -> (possibleWords, isIdentChar)

  possibleWords =
    names <> case ctxCreativeMode of
      True -> S.toList reservedWords
      False -> S.toList $ reservedWords `S.difference` creativeWords

  entityNames = M.keys $ entitiesByName em

  t = theRepl ^. replPromptText
  setCmd nt ms =
    theRepl
      & replPromptText .~ nt
      & replPromptType .~ CmdPrompt ms

-- | Validate the REPL input when it changes: see if it parses and
--   typechecks, and set the color accordingly.
validateREPLForm :: AppState -> AppState
validateREPLForm s =
  case replPrompt of
    CmdPrompt _
      | T.null uinput ->
          let theType = s ^. gameState . gameControls . replStatus . replActiveType
           in s & uiState . uiGameplay . uiREPL . replType .~ theType
    CmdPrompt _
      | otherwise ->
          let env = s ^. gameState . baseEnv
              (theType, errSrcLoc) = case readTerm' defaultParserConfig uinput of
                Left err ->
                  let ((y1, x1), (y2, x2), _msg) = showErrorPos err
                   in (Nothing, Left (SrcLoc (y2x y1 + x1) (y2x y2 + x2)))
                Right Nothing -> (Nothing, Right ())
                Right (Just theTerm) -> case processParsedTerm' env theTerm of
                  Right t -> (Just (t ^. sType), Right ())
                  Left err -> (Nothing, Left (cteSrcLoc err))
           in s
                & uiState . uiGameplay . uiREPL . replValid .~ errSrcLoc
                & uiState . uiGameplay . uiREPL . replType .~ theType
    SearchPrompt _ -> s
 where
  uinput = s ^. uiState . uiGameplay . uiREPL . replPromptText
  replPrompt = s ^. uiState . uiGameplay . uiREPL . replPromptType
  uLineCounts = T.length <$> T.lines uinput
  y2x n = n + sum (take n uLineCounts)

-- | Update our current position in the REPL history.
adjReplHistIndex :: TimeDir -> AppState -> AppState
adjReplHistIndex d s =
  s
    & uiState . uiGameplay . uiREPL %~ moveREPL
    & validateREPLForm
 where
  moveREPL :: REPLState -> REPLState
  moveREPL theRepl =
    newREPL
      & (if replIndexIsAtInput (theRepl ^. replHistory) then saveLastEntry else id)
      & (if oldEntry /= newEntry then showNewEntry else id)
   where
    -- new AppState after moving the repl index
    newREPL :: REPLState
    newREPL = theRepl & replHistory %~ moveReplHistIndex d oldEntry

    saveLastEntry = replLast .~ (theRepl ^. replPromptText)
    showNewEntry = (replPromptEditor .~ newREPLEditor newEntry) . (replPromptType .~ CmdPrompt [])
    -- get REPL data
    getCurrEntry = fromMaybe (theRepl ^. replLast) . getCurrentItemText . view replHistory
    oldEntry = getCurrEntry theRepl
    newEntry = getCurrEntry newREPL

------------------------------------------------------------
-- Info panel events
------------------------------------------------------------

-- | Handle user events in the info panel (just scrolling).
--
-- TODO: #2010 Finish porting Controller to KeyEventHandlers
handleInfoPanelEvent :: ViewportScroll Name -> BrickEvent Name AppEvent -> EventM Name AppState ()
handleInfoPanelEvent vs = \case
  Key V.KDown -> vScrollBy vs 1
  Key V.KUp -> vScrollBy vs (-1)
  CharKey 'k' -> vScrollBy vs 1
  CharKey 'j' -> vScrollBy vs (-1)
  Key V.KPageDown -> vScrollPage vs Brick.Down
  Key V.KPageUp -> vScrollPage vs Brick.Up
  Key V.KHome -> vScrollToBeginning vs
  Key V.KEnd -> vScrollToEnd vs
  _ -> return ()
