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

import Brick hiding (Direction, Location)
import Brick.Animation (stopAnimationManager)
import Brick.Focus
import Brick.Keybindings qualified as B
import Brick.Widgets.Dialog
import Brick.Widgets.Edit (Editor, applyEdit, editContentsL, handleEditorEvent)
import Brick.Widgets.List (handleListEvent, listElements)
import Brick.Widgets.List qualified as BL
import Brick.Widgets.TabularList.Grid qualified as BG
import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Control.Lens as Lens
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState, execState)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
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
import Swarm.Game.Robot.Concrete
import Swarm.Game.Scenario (scenarioMetadata, scenarioName)
import Swarm.Game.Scenario.Scoring.Best (scenarioBestByTime)
import Swarm.Game.Scenario.Scoring.GenericMetrics
import Swarm.Game.Scenario.Status (ScenarioPath (..), ScenarioWith (..), getScenario)
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
import Swarm.Language.Pipeline (processParsedTerm')
import Swarm.Language.Syntax hiding (Key)
import Swarm.Language.Typecheck (
  ContextualTypeErr (..),
 )
import Swarm.Language.Value (Value (VKey), emptyEnv, envTypes)
import Swarm.Log
import Swarm.ResourceLoading (getSwarmHistoryPath)
import Swarm.TUI.Controller.EventHandlers
import Swarm.TUI.Controller.EventHandlers.Robot (showEntityDescription)
import Swarm.TUI.Controller.SaveScenario (saveScenarioInfoOnQuit)
import Swarm.TUI.Controller.UpdateUI
import Swarm.TUI.Controller.Util
import Swarm.TUI.Editor.Controller qualified as EC
import Swarm.TUI.Editor.Model
import Swarm.TUI.Launch.Controller
import Swarm.TUI.Launch.Model
import Swarm.TUI.Launch.Prep (prepareLaunchDialog)
import Swarm.TUI.List
import Swarm.TUI.Model
import Swarm.TUI.Model.Dialog hiding (Completed)
import Swarm.TUI.Model.Menu
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.StateUpdate
import Swarm.TUI.Model.UI
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.View.Popup (startPopupAnimation)
import Swarm.TUI.View.Robot
import Swarm.TUI.View.Robot.Type
import Swarm.Util hiding (both, (<<.=))

-- | The top-level event handler for the TUI.
handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent e = do
  playing <- use $ uiState . uiPlaying
  case e of
    -- the query for upstream version could finish at any time, so we have to handle it here
    AppEvent (UpstreamVersion ev) -> handleUpstreamVersionResponse ev
    AppEvent (Web (RunWebCode {..})) | not playing -> liftIO . webReply $ Rejected NoActiveGame
    AppEvent (PopupEvent event) -> event >> continueWithoutRedraw
    _ -> do
      -- Handle popup display at the very top level, so it is
      -- unaffected by any other state, e.g. even when starting or
      -- quitting a game, moving around the menu, the popup
      -- display will continue as normal.
      popupAnimState <- use $ playState . progression . uiPopupAnimationState
      forceRedraw <- case popupAnimState of
        AnimInactive -> do
          Brick.zoom (playState . progression . uiPopups) nextPopup
          startPopupIfNeeded
          pure False
        AnimScheduled -> pure False
        AnimActive _ -> pure True

      if playing
        then handleMainEvent forceRedraw e
        else handleMenuEvent e

startPopupIfNeeded :: EventM Name AppState ()
startPopupIfNeeded = do
  mPopup <- use $ playState . progression . uiPopups . currentPopup
  case mPopup of
    Just popup -> do
      -- Ensures we don't grab another popup while waiting for the animation manager to start the event.
      -- The animation state will be set to AnimActive when the animation manager actually starts the animation
      playState . progression . uiPopupAnimationState .= AnimScheduled
      animMgr <- use animationMgr
      startPopupAnimation animMgr popup
    Nothing -> pure ()

-- | Halt the app, properly cleaning up the animation manager.
haltApp :: EventM Name AppState ()
haltApp = use animationMgr >>= stopAnimationManager >> halt

handleUpstreamVersionResponse :: Either (Severity, Text) String -> EventM Name AppState ()
handleUpstreamVersionResponse ev = do
  case ev of
    Left (sev, e) -> runtimeState . eventLog %= logEvent SystemLog sev "Release" e
    Right _ -> pure ()
  runtimeState . upstreamRelease .= ev

handleMenuEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleMenuEvent e =
  use (uiState . uiMenu) >>= \case
    -- If we reach the NoMenu case when uiPlaying is False, just
    -- quit the app.  We should actually never reach this code (the
    -- quitGame function would have already halted the app).
    NoMenu -> haltApp
    MainMenu l -> handleMainMenuEvent l e
    NewGameMenu l -> do
      launchControls <- use $ uiState . uiLaunchConfig . controls
      if launchControls ^. fileBrowser . fbIsDisplayed
        then handleFBEvent e
        else case launchControls ^. isDisplayedFor of
          Nothing -> handleNewGameMenuEvent l e
          Just siPair -> handleLaunchOptionsEvent siPair e
    MessagesMenu -> handleMainMessagesEvent e
    AchievementsMenu l -> handleMainAchievementsEvent l e
    AboutMenu -> pressAnyKey (MainMenu (mainMenu About)) e

-- | The event handler for the main menu.
--
-- TODO: #2010 Finish porting Controller to KeyEventHandlers
handleMainMenuEvent ::
  BL.List Name MainMenuEntry -> BrickEvent Name AppEvent -> EventM Name AppState ()
handleMainMenuEvent menu = \case
  Key V.KEnter ->
    forM_ (snd <$> BL.listSelectedElement menu) $ \case
      NewGame -> do
        ss <- use $ playState . progression . scenarios
        uiState . uiMenu .= NewGameMenu (pure $ mkScenarioList $ pathifyCollection ss)
      Tutorial -> do
        ss <- use $ playState . progression . scenarios

        -- Extract the first unsolved tutorial challenge
        let tutorialCollection = getTutorials ss
            tutorials = scenarioCollectionToList tutorialCollection
            -- Find first unsolved tutorial, or first tutorial if all are solved
            firstUnsolved :: Maybe (ScenarioItem ScenarioInfo)
            firstUnsolved = find unsolved tutorials <|> listToMaybe tutorials
            unsolved = \case
              SISingle (ScenarioWith _ si) -> case si ^. scenarioStatus of
                Played _ _ best
                  | Metric Completed _ <- best ^. scenarioBestByTime -> False
                  | otherwise -> True
                _ -> True
              _ -> False

        case firstUnsolved of
          Just (SISingle firstUnsolvedInfo) -> do
            let firstUnsolvedName = firstUnsolvedInfo ^. getScenario . scenarioMetadata . scenarioName

            -- Now set up the menu stack as if the user had chosen "New Game > Tutorials > t"
            -- where t is the tutorial scenario we identified as the first unsolved one
            let topMenu =
                  BL.listFindBy
                    ((== tutorialsDirname) . T.unpack . scenarioItemName)
                    (mkScenarioList $ pathifyCollection ss)
                tutorialMenu =
                  BL.listFindBy
                    ((== firstUnsolvedName) . scenarioItemName)
                    (mkScenarioList $ pathifyCollection tutorialCollection)
                menuStack = tutorialMenu :| pure topMenu

            -- Finally, set the menu stack, and start the scenario!
            uiState . uiMenu .= NewGameMenu menuStack

            let remainingTutorials = maybe mempty (getScenariosAfterSelection tutorialMenu) $ BL.listSelected tutorialMenu
            startGame (pathifyCollection firstUnsolvedInfo :| remainingTutorials) Nothing

          -- This shouldn't normally happen, but it could if the
          -- correct data files aren't installed.  In that case, log
          -- an error.
          _ -> runtimeState . eventLog %= logEvent SystemLog Error "Tutorials" "No tutorials found!"
      Achievements -> uiState . uiMenu .= AchievementsMenu (BL.list AchievementList (V.fromList listAchievements) 1)
      Messages -> do
        runtimeState . eventLog . notificationsCount .= 0
        uiState . uiMenu .= MessagesMenu
      About -> do
        uiState . uiMenu .= AboutMenu
        Brick.zoom (playState . progression) $
          attainAchievement $
            GlobalAchievement LookedAtAboutScreen
      Quit -> haltApp
  CharKey 'q' -> haltApp
  ControlChar 'q' -> haltApp
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
  NonEmpty (BL.List Name (ScenarioItem ScenarioPath)) ->
  BrickEvent Name AppEvent ->
  EventM Name AppState ()
handleNewGameMenuEvent scenarioStack@(curMenu :| rest) = \case
  Key V.KEnter ->
    forM_ (BL.listSelectedElement curMenu) $ \(pos, item) -> case item of
      SISingle siPair -> do
        invalidateCache
        let remaining = getScenariosAfterSelection curMenu pos
        startGame (siPair :| remaining) Nothing
      SICollection _ c -> uiState . uiMenu .= NewGameMenu (NE.cons (mkScenarioList c) scenarioStack)
  CharKey 'o' -> showLaunchDialog
  CharKey 'O' -> showLaunchDialog
  Key V.KEsc -> exitNewGameMenu scenarioStack
  CharKey 'q' -> exitNewGameMenu scenarioStack
  ControlChar 'q' -> haltApp
  VtyEvent ev -> do
    menu' <- nestEventM' curMenu (handleListEvent ev)
    uiState . uiMenu .= NewGameMenu (menu' :| rest)
  _ -> pure ()
 where
  showLaunchDialog = case snd <$> BL.listSelectedElement curMenu of
    Just (SISingle (ScenarioWith s (ScenarioPath p))) -> do
      ss <- use $ playState . progression . scenarios
      let si = getScenarioInfoFromPath ss p
      Brick.zoom (uiState . uiLaunchConfig) $ prepareLaunchDialog $ ScenarioWith s si
    _ -> pure ()

exitNewGameMenu ::
  NonEmpty (BL.List Name (ScenarioItem ScenarioPath)) ->
  EventM Name AppState ()
exitNewGameMenu stk =
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
      -- If the game is paused, don't run any game ticks, but do redraw if needed.
      Frame ->
        if s ^. playState . scenarioState . gameState . temporal . paused
          then updateAndRedrawUI forceRedraw
          else runFrameUI forceRedraw
      Web (RunWebCode e r) -> Brick.zoom (playState . scenarioState) $ runBaseWebCode e r
      -- UpstreamVersion event should already be handled by top-level handler, so
      -- in theory this case cannot happen.
      UpstreamVersion _ -> pure ()
      -- PopupEvent event should already be handled by top-level handler, so this shouldn't happen.
      PopupEvent _ -> pure ()
    VtyEvent (V.EvResize _ _) -> invalidateCache
    EscapeKey
      | Just m <- s ^. playState . scenarioState . uiGameplay . uiDialogs . uiModal ->
          Brick.zoom (playState . scenarioState) $
            if s ^. playState . scenarioState . uiGameplay . uiDialogs . uiRobot . isDetailsOpened
              then uiGameplay . uiDialogs . uiRobot . isDetailsOpened .= False
              else closeModal m
    -- Pass to key handler (allows users to configure bindings)
    -- See Note [how Swarm event handlers work]
    VtyEvent (V.EvKey k m)
      | isJust (B.lookupVtyEvent k m keyHandler) -> void $ B.handleKey keyHandler k m
    -- pass keys on to modal event handler if a modal is open
    VtyEvent vev
      | isJust (s ^. playState . scenarioState . uiGameplay . uiDialogs . uiModal) -> handleModalEvent vev
    MouseDown (TerrainListItem pos) V.BLeft _ _ ->
      playState . scenarioState . uiGameplay . uiWorldEditor . terrainList %= BL.listMoveTo pos
    MouseDown (EntityPaintListItem pos) V.BLeft _ _ ->
      playState . scenarioState . uiGameplay . uiWorldEditor . entityPaintList %= BL.listMoveTo pos
    MouseDown WorldPositionIndicator _ _ _ ->
      playState . scenarioState . uiGameplay . uiWorldCursor .= Nothing
    MouseDown (FocusablePanel WorldPanel) V.BMiddle _ mouseLoc ->
      -- Eye Dropper tool
      Brick.zoom (playState . scenarioState) $ EC.handleMiddleClick mouseLoc
    MouseDown (FocusablePanel WorldPanel) V.BRight _ mouseLoc ->
      -- Eraser tool
      Brick.zoom (playState . scenarioState) $ EC.handleRightClick mouseLoc
    MouseDown (FocusablePanel WorldPanel) V.BLeft [V.MCtrl] mouseLoc ->
      -- Paint with the World Editor
      Brick.zoom (playState . scenarioState) $ EC.handleCtrlLeftClick mouseLoc
    MouseDown n _ _ mouseLoc ->
      case n of
        FocusablePanel WorldPanel -> Brick.zoom (playState . scenarioState) $ do
          mouseCoordsM <- Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
          shouldUpdateCursor <- EC.updateAreaBounds mouseCoordsM
          when shouldUpdateCursor $
            uiGameplay . uiWorldCursor .= mouseCoordsM
        REPLInput -> handleREPLEvent ev
        (UIShortcut "Help") -> Brick.zoom (playState . scenarioState) $ toggleMidScenarioModal HelpModal
        (UIShortcut "Robots") -> Brick.zoom (playState . scenarioState) $ toggleMidScenarioModal RobotsModal
        (UIShortcut "Commands") -> Brick.zoom (playState . scenarioState) $ toggleDiscoveryNotificationModal CommandsModal availableCommands
        (UIShortcut "Recipes") -> Brick.zoom (playState . scenarioState) $ toggleDiscoveryNotificationModal RecipesModal availableRecipes
        (UIShortcut "Messages") -> Brick.zoom (playState . scenarioState) toggleMessagesModal
        (UIShortcut "pause") -> Brick.zoom (playState . scenarioState) $ whenRunningPlayState safeTogglePause
        (UIShortcut "unpause") -> Brick.zoom (playState . scenarioState) $ whenRunningPlayState safeTogglePause
        (UIShortcut "step") -> whenRunningAppState runSingleTick
        (UIShortcut "speed-up") -> Brick.zoom (playState . scenarioState) $ whenRunningPlayState . modify $ adjustTPS (+)
        (UIShortcut "speed-down") -> Brick.zoom (playState . scenarioState) $ whenRunningPlayState . modify $ adjustTPS (-)
        (UIShortcut "hide REPL") -> Brick.zoom (playState . scenarioState) toggleREPLVisibility
        (UIShortcut "show REPL") -> Brick.zoom (playState . scenarioState) toggleREPLVisibility
        (UIShortcut "debug") -> showCESKDebug
        (UIShortcut "hide robots") -> Brick.zoom (playState . scenarioState . uiGameplay) hideRobots
        (UIShortcut "goal") -> Brick.zoom (playState . scenarioState) viewGoal
        _ -> continueWithoutRedraw
    MouseUp n _ _mouseLoc ->
      Brick.zoom (playState . scenarioState) $ do
        case n of
          InventoryListItem pos -> do
            uiGameplay . uiInventory . uiInventoryList . traverse . _2 %= BL.listMoveTo pos
            vScrollToBeginning infoScroll
          x@(WorldEditorPanelControl y) -> do
            uiGameplay . uiWorldEditor . editorFocusRing %= focusSetCurrent x
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
      fring <- use $ playState . scenarioState . uiGameplay . uiFocusRing
      case focusGetCurrent fring of
        Just (FocusablePanel x) -> case x of
          REPLPanel -> handleREPLEvent ev
          -- Pass to key handler (allows users to configure bindings)
          -- See Note [how Swarm event handlers work]
          WorldPanel | VtyEvent (V.EvKey k m) <- ev -> do
            wh <- use $ keyEventHandling . keyDispatchers . to worldDispatcher
            void $ B.handleKey wh k m
          WorldPanel | otherwise -> continueWithoutRedraw
          WorldEditorPanel -> Brick.zoom (playState . scenarioState) $ EC.handleWorldEditorPanelEvent ev
          RobotPanel -> handleRobotPanelEvent ev
          InfoPanel -> Brick.zoom (playState . scenarioState) $ handleInfoPanelEvent infoScroll ev
        _ -> continueWithoutRedraw

closeModal :: Modal -> EventM Name ScenarioState ()
closeModal m = do
  safeAutoUnpause
  uiGameplay . uiDialogs . uiModal .= Nothing
  -- message modal is not autopaused, so update notifications when leaving it
  when (m ^. modalType == MidScenarioModal MessagesModal) $ do
    t <- use $ gameState . temporal . ticks
    gameState . messageInfo . lastSeenMessageTime .= t

-- TODO: #2010 Finish porting Controller to KeyEventHandlers
handleModalEvent :: V.Event -> EventM Name AppState ()
handleModalEvent = \case
  V.EvKey V.KEnter [] -> do
    modal <- preuse $ playState . scenarioState . uiGameplay . uiDialogs . uiModal . _Just . modalType
    case modal of
      Just (MidScenarioModal RobotsModal) -> do
        robotDialog <- use $ playState . scenarioState . uiGameplay . uiDialogs . uiRobot
        unless (robotDialog ^. isDetailsOpened) $ do
          g <- use $ playState . scenarioState . gameState
          let widget = robotDialog ^. robotsGridList
          forM_ (getSelectedRobot g widget) $ \rob -> Brick.zoom (playState . scenarioState . uiGameplay . uiDialogs . uiRobot) $ do
            isDetailsOpened .= True
            Brick.zoom robotDetailsPaneState $ updateRobotDetailsPane rob
      _ -> do
        menu <- use $ uiState . uiMenu

        mdialog <- preuse $ playState . scenarioState . uiGameplay . uiDialogs . uiModal . _Just . modalDialog
        Brick.zoom playState $ toggleEndScenarioModal QuitModal menu

        let isNoMenu = case menu of
              NoMenu -> True
              _ -> False

        case dialogSelection =<< mdialog of
          Just (Button QuitButton, _) -> quitGame isNoMenu
          Just (Button KeepPlayingButton, _) -> Brick.zoom playState $ toggleEndScenarioModal KeepPlayingModal menu
          Just (Button StartOverButton, StartOver currentSeed siPair) -> do
            invalidateCache
            restartGame currentSeed siPair
          Just (Button NextButton, Next remainingScenarios) -> do
            quitGame isNoMenu
            invalidateCache
            startGame remainingScenarios Nothing
          _ -> return ()
  ev -> Brick.zoom (playState . scenarioState) $ do
    Brick.zoom (uiGameplay . uiDialogs . uiModal . _Just . modalDialog) (handleDialogEvent ev)
    modal <- preuse $ uiGameplay . uiDialogs . uiModal . _Just . modalType
    case modal of
      Just (MidScenarioModal TerrainPaletteModal) ->
        refreshList $ uiGameplay . uiWorldEditor . terrainList
      Just (MidScenarioModal EntityPaletteModal) -> refreshList $ uiGameplay . uiWorldEditor . entityPaintList
      Just (MidScenarioModal GoalModal) -> case ev of
        V.EvKey (V.KChar '\t') [] -> uiGameplay . uiDialogs . uiGoal . focus %= focusNext
        _ -> do
          focused <- use $ uiGameplay . uiDialogs . uiGoal . focus
          case focusGetCurrent focused of
            Just (GoalWidgets w) -> case w of
              ObjectivesList -> do
                lw <- use $ uiGameplay . uiDialogs . uiGoal . listWidget
                newList <- refreshGoalList lw
                uiGameplay . uiDialogs . uiGoal . listWidget .= newList
              GoalSummary -> handleInfoPanelEvent modalScroll (VtyEvent ev)
            _ -> handleInfoPanelEvent modalScroll (VtyEvent ev)
      Just (MidScenarioModal StructuresModal) -> case ev of
        V.EvKey (V.KChar '\t') [] -> uiGameplay . uiDialogs . uiStructure . structurePanelFocus %= focusNext
        _ -> do
          focused <- use $ uiGameplay . uiDialogs . uiStructure . structurePanelFocus
          case focusGetCurrent focused of
            Just (StructureWidgets w) -> case w of
              StructuresList ->
                refreshList $ uiGameplay . uiDialogs . uiStructure . structurePanelListWidget
              StructureSummary -> handleInfoPanelEvent modalScroll (VtyEvent ev)
            _ -> handleInfoPanelEvent modalScroll (VtyEvent ev)
      Just (MidScenarioModal RobotsModal) -> do
        uiGame <- use uiGameplay
        g <- use gameState
        Brick.zoom (uiGameplay . uiDialogs . uiRobot) $ case ev of
          V.EvKey (V.KChar '\t') [] -> robotDetailsPaneState . detailFocus %= focusNext
          _ -> do
            isInDetailsMode <- use isDetailsOpened
            if isInDetailsMode
              then Brick.zoom (robotDetailsPaneState . logsList) $ handleListEvent ev
              else do
                Brick.zoom robotsGridList $ BG.handleGridListEvent (robotGridRenderers uiGame g) ev
                -- Ensure list widget content is updated immediately
                mRob <- use $ robotsGridList . to (getSelectedRobot g)
                forM_ mRob $ Brick.zoom robotDetailsPaneState . updateRobotDetailsPane
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
quitGame :: Bool -> EventM Name AppState ()
quitGame isNoMenu = do
  -- Write out REPL history.
  history <- use $ playState . scenarioState . uiGameplay . uiREPL . replHistory
  let hist = mapMaybe getREPLSubmitted $ getLatestREPLHistoryItems maxBound history
  liftIO $ (`T.appendFile` T.unlines hist) =<< getSwarmHistoryPath True

  -- Save scenario status info.
  dOps <- use $ uiState . uiDebugOptions
  Brick.zoom playState $ saveScenarioInfoOnQuit dOps

  -- Automatically advance the menu to the next scenario iff the
  -- player has won the current one.
  wc <- use $ playState . scenarioState . gameState . winCondition
  case wc of
    WinConditions (Won _ _) _ -> uiState . uiMenu %= advanceMenu
    _ -> return ()

  -- Either quit the entire app (if the scenario was chosen directly
  -- from the command line) or return to the menu (if the scenario was
  -- chosen from the menu).
  if isNoMenu
    then haltApp
    else uiState . uiPlaying .= False

------------------------------------------------------------
-- REPL events
------------------------------------------------------------

-- | Handle a user input event for the REPL.
handleREPLEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleREPLEvent x = do
  s <- get
  let controlMode = s ^. playState . scenarioState . uiGameplay . uiREPL . replControlMode
  let keyHandler = s ^. keyEventHandling . keyDispatchers . to replDispatcher
  let menu = s ^. uiState . uiMenu
  case x of
    -- Pass to key handler (allows users to configure bindings)
    -- See Note [how Swarm event handlers work]
    VtyEvent (V.EvKey k m)
      | isJust (B.lookupVtyEvent k m keyHandler) ->
          void $ B.handleKey keyHandler k m
    -- Handle other events in a way appropriate to the current REPL
    -- control mode.
    _ -> Brick.zoom playState $ case controlMode of
      Typing -> handleREPLEventTyping menu x
      Piloting -> handleREPLEventPiloting menu x
      Replaying -> pure () -- handled above, so that user does not change REPL
      Handling -> case x of
        -- Handle keypresses using the custom installed handler
        VtyEvent (V.EvKey k mods) -> Brick.zoom scenarioState $ runInputHandler (mkKeyCombo mods k)
        -- Handle all other events normally
        _ -> handleREPLEventTyping menu x

-- | Run the installed input handler on a key combo entered by the user.
runInputHandler :: KeyCombo -> EventM Name ScenarioState ()
runInputHandler kc = do
  mhandler <- use $ gameState . gameControls . inputHandler
  forM_ mhandler $ \(_, handler) -> do
    -- Shouldn't be possible to get here if there is no input handler, but
    -- if we do somehow, just do nothing.

    -- Make sure the base is currently idle; if so, apply the
    -- installed input handler function to a `key` value
    -- representing the typed input.
    working <- use $ gameState . gameControls . replWorking
    unless working $ do
      s <- get
      let env = fromMaybe emptyEnv $ s ^? gameState . baseEnv
          store = s ^. gameState . baseStore
          handlerCESK = Out (VKey kc) store [FApp handler, FExec, FSuspend env]
      gameState . baseRobot . machine .= handlerCESK
      gameState %= execState (zoomRobots $ activateRobot 0)

-- | Handle a user "piloting" input event for the REPL.
--
-- TODO: #2010 Finish porting Controller to KeyEventHandlers
handleREPLEventPiloting :: Menu -> BrickEvent Name AppEvent -> EventM Name PlayState ()
handleREPLEventPiloting m x = case x of
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
    scenarioState . uiGameplay . uiREPL %= setCmd (cmdText <> ";")
    Brick.zoom scenarioState $ modify validateREPLForm
    handleREPLEventTyping m $ Key V.KEnter

  setCmd nt theRepl =
    theRepl
      & replPromptText .~ nt
      & replPromptType .~ CmdPrompt []

runBaseWebCode :: (MonadState ScenarioState m, MonadIO m) => T.Text -> (WebInvocationState -> IO ()) -> m ()
runBaseWebCode uinput ureply = do
  s <- get
  if s ^. gameState . gameControls . replWorking
    then liftIO . ureply $ Rejected AlreadyRunning
    else do
      gameState . gameControls . replListener .= ureply . Complete . T.unpack
      runBaseCode uinput
        >>= liftIO . ureply . \case
          Left err -> Rejected . ParseError $ T.unpack err
          Right () -> InProgress

-- | Handle a user input event for the REPL.
--
-- TODO: #2010 Finish porting Controller to KeyEventHandlers
handleREPLEventTyping :: Menu -> BrickEvent Name AppEvent -> EventM Name PlayState ()
handleREPLEventTyping m = \case
  -- Scroll the REPL on PageUp or PageDown
  Key V.KPageUp -> vScrollPage replScroll Brick.Up
  Key V.KPageDown -> vScrollPage replScroll Brick.Down
  k -> do
    -- On any other key event, jump to the bottom of the REPL then handle the event
    vScrollToEnd replScroll
    case k of
      Key V.KEnter -> Brick.zoom scenarioState $ do
        s <- get
        let theRepl = s ^. uiGameplay . uiREPL
            uinput = theRepl ^. replPromptText

        if not $ s ^. gameState . gameControls . replWorking
          then case theRepl ^. replPromptType of
            CmdPrompt _ -> do
              void $ runBaseCode uinput
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
      Key V.KUp -> Brick.zoom scenarioState $ modify $ adjReplHistIndex Older
      Key V.KDown -> Brick.zoom scenarioState $ do
        repl <- use $ uiGameplay . uiREPL
        let hist = repl ^. replHistory
            uinput = repl ^. replPromptText
        case repl ^. replPromptType of
          CmdPrompt {}
            | hist ^. replIndex == replLength hist && not (T.null uinput) ->
                -- Special case for hitting "Down" arrow while entering a new non-empty input:
                -- save the input in the history and make the REPL blank.
                do
                  addREPLHistItem (REPLEntry Stashed) uinput
                  resetREPL "" (CmdPrompt [])
                  modify validateREPLForm
          -- Otherwise, just move around in the history as normal.
          _ -> modify $ adjReplHistIndex Newer
      ControlChar 'r' ->
        Brick.zoom (scenarioState . uiGameplay . uiREPL) $ do
          uir <- get
          let uinput = uir ^. replPromptText
          case uir ^. replPromptType of
            CmdPrompt _ -> replPromptType .= SearchPrompt (uir ^. replHistory)
            SearchPrompt rh -> forM_ (lastEntry uinput rh) $ \found ->
              replPromptType .= SearchPrompt (removeEntry found rh)
      CharKey '\t' -> Brick.zoom scenarioState $ do
        s <- get
        let names = s ^.. gameState . baseEnv . envTypes . to assocs . traverse . _1
        uiGameplay . uiREPL %= tabComplete (CompletionContext (s ^. gameState . creativeMode)) names (s ^. gameState . landscape . terrainAndEntities . entityMap)
        modify validateREPLForm
      EscapeKey -> Brick.zoom scenarioState $ do
        formSt <- use $ uiGameplay . uiREPL . replPromptType
        case formSt of
          CmdPrompt {} -> continueWithoutRedraw
          SearchPrompt _ -> resetREPL "" (CmdPrompt [])
      ControlChar 'd' -> do
        text <- use $ scenarioState . uiGameplay . uiREPL . replPromptText
        if text == T.empty
          then toggleEndScenarioModal QuitModal m
          else continueWithoutRedraw
      MetaKey V.KBS ->
        Brick.zoom scenarioState $
          uiGameplay . uiREPL . replPromptEditor %= applyEdit TZ.deletePrevWord
      -- finally if none match pass the event to the editor
      ev -> do
        Brick.zoom (scenarioState . uiGameplay . uiREPL . replPromptEditor) $ case ev of
          CharKey c
            | c `elem` ("([{" :: String) -> insertMatchingPair c
            | c `elem` (")]}" :: String) -> insertOrMovePast c
          _ -> handleEditorEvent ev
        scenarioState . uiGameplay . uiREPL . replPromptType %= \case
          CmdPrompt _ -> CmdPrompt [] -- reset completions on any event passed to editor
          SearchPrompt a -> SearchPrompt a

        -- Now re-validate the input, unless only the cursor moved.
        case ev of
          Key V.KLeft -> pure ()
          Key V.KRight -> pure ()
          _ -> Brick.zoom scenarioState $ modify validateREPLForm

insertMatchingPair :: Char -> EventM Name (Editor Text Name) ()
insertMatchingPair c = modify . applyEdit $ TZ.insertChar c >>> TZ.insertChar (close c) >>> TZ.moveLeft
 where
  close = \case
    '(' -> ')'
    '[' -> ']'
    '{' -> '}'
    _ -> c

-- | Insert a character in an editor unless it matches the character
--   already at the cursor, in which case we just move past it
--   instead, without inserting an extra copy.
insertOrMovePast :: Char -> EventM Name (Editor Text Name) ()
insertOrMovePast c = do
  e <- get
  modify . applyEdit $ case TZ.currentChar (e ^. editContentsL) of
    Just c' | c' == c -> TZ.moveRight
    _ -> TZ.insertChar c

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
tabComplete :: CompletionContext -> [Text] -> EntityMap -> REPLState -> REPLState
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
    names <> (if ctxCreativeMode then S.toList reservedWords else S.toList $ reservedWords `S.difference` creativeWords)

  entityNames = M.keys $ entitiesByName em

  t = theRepl ^. replPromptText
  setCmd nt ms =
    theRepl
      & replPromptText .~ nt
      & replPromptType .~ CmdPrompt ms

-- | Validate the REPL input when it changes: see if it parses and
--   typechecks, and set the color accordingly.
validateREPLForm :: ScenarioState -> ScenarioState
validateREPLForm s =
  case replPrompt of
    CmdPrompt _
      | T.null uinput ->
          let theType = s ^. gameState . gameControls . replStatus . replActiveType
           in s & uiGameplay . uiREPL . replType .~ theType
    CmdPrompt _
      | otherwise ->
          let env = fromMaybe emptyEnv $ s ^? gameState . baseEnv
              (theType, errSrcLoc) = case readTerm' defaultParserConfig uinput of
                Left err ->
                  let ((_y1, x1), (_y2, x2), _msg) = showErrorPos err
                   in (Nothing, Left (SrcLoc x1 x2))
                Right Nothing -> (Nothing, Right ())
                Right (Just theTerm) -> case processParsedTerm' env theTerm of
                  Right t -> (Just (t ^. sType), Right ())
                  Left err -> (Nothing, Left (cteSrcLoc err))
           in s
                & uiGameplay . uiREPL . replValid .~ errSrcLoc
                & uiGameplay . uiREPL . replType .~ theType
    SearchPrompt _ -> s
 where
  uinput = s ^. uiGameplay . uiREPL . replPromptText
  replPrompt = s ^. uiGameplay . uiREPL . replPromptType

-- | Update our current position in the REPL history.
adjReplHistIndex :: TimeDir -> ScenarioState -> ScenarioState
adjReplHistIndex d s =
  s
    & uiGameplay . uiREPL %~ moveREPL
    & validateREPLForm
 where
  moveREPL :: REPLState -> REPLState
  moveREPL theRepl =
    newREPL
      & applyWhen (replIndexIsAtInput (theRepl ^. replHistory)) saveLastEntry
      & applyWhen (oldEntry /= newEntry) showNewEntry
   where
    -- new AppState after moving the repl index
    newREPL :: REPLState
    newREPL = theRepl & replHistory %~ moveReplHistIndex d oldEntry

    saveLastEntry = replLast .~ theRepl ^. replPromptText
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
handleInfoPanelEvent :: ViewportScroll Name -> BrickEvent Name AppEvent -> EventM Name ScenarioState ()
handleInfoPanelEvent vs = \case
  Key V.KDown -> vScrollBy vs 1
  Key V.KUp -> vScrollBy vs (-1)
  CharKey 'k' -> vScrollBy vs 1
  CharKey 'j' -> vScrollBy vs (-1)
  Key V.KPageDown -> vScrollPage vs Brick.Down
  Key V.KPageUp -> vScrollPage vs Brick.Up
  Key V.KHome -> vScrollToBeginning vs
  Key V.KEnd -> vScrollToEnd vs
  Key V.KEnter -> showEntityDescription
  _ -> return ()

-- * Util

getScenariosAfterSelection ::
  BL.GenericList n V.Vector (ScenarioItem a) ->
  Int ->
  [ScenarioWith a]
getScenariosAfterSelection m selIndex =
  [x | SISingle x <- V.toList remaining]
 where
  remaining = snd $ BL.splitAt (selIndex + 1) $ listElements m
