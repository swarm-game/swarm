{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
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
  ticksPerFrameCap,
  runFrameTicks,
  runGameTickUI,
  runGameTick,
  updateUI,

  -- ** REPL panel
  runBaseWebCode,
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

  -- ** Utils
  getTutorials,
) where

import Brick hiding (Direction, Location)
import Brick.Focus
import Brick.Widgets.Dialog
import Brick.Widgets.Edit (handleEditorEvent)
import Brick.Widgets.List (handleListEvent)
import Brick.Widgets.List qualified as BL
import Control.Applicative (liftA2, pure)
import Control.Carrier.Lift qualified as Fused
import Control.Carrier.State.Lazy qualified as Fused
import Control.Lens as Lens
import Control.Lens.Extras as Lens (is)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState, execState)
import Data.Bits
import Data.Either (isRight)
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (getZonedTime)
import Data.Vector qualified as V
import Graphics.Vty qualified as V
import Linear
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Achievement.Persistence
import Swarm.Game.CESK (CESK (Out), Frame (FApp, FExec), cancel, emptyStore, initMachine)
import Swarm.Game.Entity hiding (empty)
import Swarm.Game.Location
import Swarm.Game.ResourceLoading (getSwarmHistoryPath)
import Swarm.Game.Robot
import Swarm.Game.ScenarioInfo
import Swarm.Game.State
import Swarm.Game.Step (finishGameTick, gameTick)
import Swarm.Language.Capability (Capability (CDebug, CMake))
import Swarm.Language.Context
import Swarm.Language.Key (KeyCombo, mkKeyCombo)
import Swarm.Language.Module
import Swarm.Language.Parse (reservedWords)
import Swarm.Language.Pipeline
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Pretty
import Swarm.Language.Requirement qualified as R
import Swarm.Language.Syntax hiding (Key)
import Swarm.Language.Typed (Typed (..))
import Swarm.Language.Types
import Swarm.Language.Value (Value (VKey, VUnit), prettyValue, stripVResult)
import Swarm.TUI.Controller.Util
import Swarm.TUI.Editor.Controller qualified as EC
import Swarm.TUI.Editor.Model
import Swarm.TUI.Inventory.Sorting (cycleSortDirection, cycleSortOrder)
import Swarm.TUI.Launch.Controller
import Swarm.TUI.Launch.Model
import Swarm.TUI.Launch.Prep (prepareLaunchDialog)
import Swarm.TUI.List
import Swarm.TUI.Model
import Swarm.TUI.Model.Goal
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.StateUpdate
import Swarm.TUI.Model.UI
import Swarm.TUI.View.Objective qualified as GR
import Swarm.TUI.View.Util (generateModal)
import Swarm.Util hiding (both, (<<.=))
import Swarm.Version (NewReleaseFailure (..))
import System.Clock
import System.FilePath (splitDirectories)
import Witch (into)
import Prelude hiding (Applicative (..)) -- See Note [liftA2 re-export from Prelude]

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

tutorialsDirname :: FilePath
tutorialsDirname = "Tutorials"

-- | The top-level event handler for the TUI.
handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent = \case
  -- the query for upstream version could finish at any time, so we have to handle it here
  AppEvent (UpstreamVersion ev) -> do
    let logReleaseEvent l e = runtimeState . eventLog %= logEvent l ("Release", -7) (T.pack $ show e)
    case ev of
      Left e@(FailedReleaseQuery _e) -> logReleaseEvent (ErrorTrace Error) e
      Left e -> logReleaseEvent (ErrorTrace Warning) e
      Right _ -> pure ()
    runtimeState . upstreamRelease .= ev
  e -> do
    s <- get
    if s ^. uiState . uiPlaying
      then handleMainEvent e
      else
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
handleMainMenuEvent ::
  BL.List Name MainMenuEntry -> BrickEvent Name AppEvent -> EventM Name AppState ()
handleMainMenuEvent menu = \case
  Key V.KEnter ->
    case snd <$> BL.listSelectedElement menu of
      Nothing -> continueWithoutRedraw
      Just x0 -> case x0 of
        NewGame -> do
          cheat <- use $ uiState . uiCheatMode
          ss <- use $ runtimeState . scenarios
          uiState . uiMenu .= NewGameMenu (NE.fromList [mkScenarioList cheat ss])
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
              menuStack = NE.fromList [tutorialMenu, topMenu]
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
  _ -> continueWithoutRedraw

getTutorials :: ScenarioCollection -> ScenarioCollection
getTutorials sc = case M.lookup tutorialsDirname (scMap sc) of
  Just (SICollection _ c) -> c
  _ -> error $ "No tutorials exist: " ++ show sc

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
  _ -> continueWithoutRedraw
 where
  returnToMainMenu = uiState . uiMenu .= MainMenu (mainMenu Messages)

handleMainMessagesEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleMainMessagesEvent = \case
  Key V.KEsc -> returnToMainMenu
  CharKey 'q' -> returnToMainMenu
  ControlChar 'q' -> returnToMainMenu
  _ -> return ()
 where
  returnToMainMenu = uiState . uiMenu .= MainMenu (mainMenu Messages)

handleNewGameMenuEvent ::
  NonEmpty (BL.List Name ScenarioItem) ->
  BrickEvent Name AppEvent ->
  EventM Name AppState ()
handleNewGameMenuEvent scenarioStack@(curMenu :| rest) = \case
  Key V.KEnter ->
    case snd <$> BL.listSelectedElement curMenu of
      Nothing -> continueWithoutRedraw
      Just (SISingle siPair) -> startGame siPair Nothing
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
  _ -> continueWithoutRedraw
 where
  showLaunchDialog = case snd <$> BL.listSelectedElement curMenu of
    Just (SISingle siPair) -> Brick.zoom (uiState . uiLaunchConfig) $ prepareLaunchDialog siPair
    _ -> continueWithoutRedraw

exitNewGameMenu :: NonEmpty (BL.List Name ScenarioItem) -> EventM Name AppState ()
exitNewGameMenu stk = do
  uiState
    . uiMenu
    .= case snd (NE.uncons stk) of
      Nothing -> MainMenu (mainMenu NewGame)
      Just stk' -> NewGameMenu stk'

pressAnyKey :: Menu -> BrickEvent Name AppEvent -> EventM Name AppState ()
pressAnyKey m (VtyEvent (V.EvKey _ _)) = uiState . uiMenu .= m
pressAnyKey _ _ = continueWithoutRedraw

-- | The top-level event handler while we are running the game itself.
handleMainEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleMainEvent ev = do
  s <- get
  mt <- preuse $ uiState . uiModal . _Just . modalType
  let isRunning = maybe True isRunningModal mt
  let isPaused = s ^. gameState . paused
  let isCreative = s ^. gameState . creativeMode
  let hasDebug = fromMaybe isCreative $ s ^? gameState . to focusedRobot . _Just . robotCapabilities . Lens.contains CDebug
  case ev of
    AppEvent ae -> case ae of
      Frame
        | s ^. gameState . paused -> continueWithoutRedraw
        | otherwise -> runFrameUI
      Web (RunWebCode c) -> runBaseWebCode c
      _ -> continueWithoutRedraw
    -- ctrl-q works everywhere
    ControlChar 'q' ->
      case s ^. gameState . winCondition of
        WinConditions (Won _) _ -> toggleModal $ ScenarioEndModal WinModal
        WinConditions (Unwinnable _) _ -> toggleModal $ ScenarioEndModal LoseModal
        _ -> toggleModal QuitModal
    VtyEvent (V.EvResize _ _) -> invalidateCacheEntry WorldCache
    Key V.KEsc
      | isJust (s ^. uiState . uiError) -> uiState . uiError .= Nothing
      | Just m <- s ^. uiState . uiModal -> do
          safeAutoUnpause
          uiState . uiModal .= Nothing
          -- message modal is not autopaused, so update notifications when leaving it
          case m ^. modalType of
            MessagesModal -> do
              gameState . lastSeenMessageTime .= s ^. gameState . ticks
            _ -> return ()
    FKey 1 -> toggleModal HelpModal
    FKey 2 -> toggleModal RobotsModal
    FKey 3 | not (null (s ^. gameState . availableRecipes . notificationsContent)) -> do
      toggleModal RecipesModal
      gameState . availableRecipes . notificationsCount .= 0
    FKey 4 | not (null (s ^. gameState . availableCommands . notificationsContent)) -> do
      toggleModal CommandsModal
      gameState . availableCommands . notificationsCount .= 0
    FKey 5 | not (null (s ^. gameState . messageNotifications . notificationsContent)) -> do
      toggleModal MessagesModal
      gameState . lastSeenMessageTime .= s ^. gameState . ticks
    -- show goal
    ControlChar 'g' ->
      if hasAnythingToShow $ s ^. uiState . uiGoal . goalsContent
        then toggleModal GoalModal
        else continueWithoutRedraw
    -- hide robots
    MetaChar 'h' -> do
      t <- liftIO $ getTime Monotonic
      h <- use $ uiState . uiHideRobotsUntil
      if h >= t
        then -- ignore repeated keypresses
          continueWithoutRedraw
        else -- hide for two seconds
        do
          uiState . uiHideRobotsUntil .= t + TimeSpec 2 0
          invalidateCacheEntry WorldCache
    -- debug focused robot
    MetaChar 'd' | isPaused && hasDebug -> do
      debug <- uiState . uiShowDebug Lens.<%= not
      if debug
        then gameState . gameStep .= RobotStep SBefore
        else zoomGameState finishGameTick >> void updateUI
    -- pausing and stepping
    ControlChar 'p' | isRunning -> safeTogglePause
    ControlChar 'o' | isRunning -> do
      gameState . runStatus .= ManualPause
      runGameTickUI
    -- speed controls
    ControlChar 'x' | isRunning -> modify $ adjustTPS (+)
    ControlChar 'z' | isRunning -> modify $ adjustTPS (-)
    -- special keys that work on all panels
    MetaChar 'w' -> setFocus WorldPanel
    MetaChar 'e' -> setFocus RobotPanel
    MetaChar 'r' -> setFocus REPLPanel
    MetaChar 't' -> setFocus InfoPanel
    -- pass keys on to modal event handler if a modal is open
    VtyEvent vev
      | isJust (s ^. uiState . uiModal) -> handleModalEvent vev
    -- toggle creative mode if in "cheat mode"

    MouseDown (TerrainListItem pos) V.BLeft _ _ ->
      uiState . uiWorldEditor . terrainList %= BL.listMoveTo pos
    MouseDown (EntityPaintListItem pos) V.BLeft _ _ ->
      uiState . uiWorldEditor . entityPaintList %= BL.listMoveTo pos
    ControlChar 'v'
      | s ^. uiState . uiCheatMode -> gameState . creativeMode %= not
    -- toggle world editor mode if in "cheat mode"
    ControlChar 'e'
      | s ^. uiState . uiCheatMode -> do
          uiState . uiWorldEditor . isWorldEditorEnabled %= not
          setFocus WorldEditorPanel
    MouseDown WorldPositionIndicator _ _ _ -> uiState . uiWorldCursor .= Nothing
    MouseDown (FocusablePanel WorldPanel) V.BMiddle _ mouseLoc ->
      -- Eye Dropper tool
      EC.handleMiddleClick mouseLoc
    MouseDown (FocusablePanel WorldPanel) V.BRight _ mouseLoc ->
      -- Eraser tool
      EC.handleRightClick mouseLoc
    MouseDown (FocusablePanel WorldPanel) V.BLeft [V.MCtrl] mouseLoc ->
      -- Paint with the World Editor
      EC.handleCtrlLeftClick mouseLoc
    -- toggle collapse/expand REPL
    MetaChar ',' -> do
      invalidateCacheEntry WorldCache
      uiState . uiShowREPL %= not
    MouseDown n _ _ mouseLoc ->
      case n of
        FocusablePanel WorldPanel -> do
          mouseCoordsM <- Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
          shouldUpdateCursor <- EC.updateAreaBounds mouseCoordsM
          when shouldUpdateCursor $
            uiState . uiWorldCursor .= mouseCoordsM
        REPLInput -> handleREPLEvent ev
        _ -> continueWithoutRedraw
    MouseUp n _ _mouseLoc -> do
      case n of
        InventoryListItem pos -> uiState . uiInventory . traverse . _2 %= BL.listMoveTo pos
        x@(WorldEditorPanelControl y) -> do
          uiState . uiWorldEditor . editorFocusRing %= focusSetCurrent x
          EC.activateWorldEditorFunction y
        _ -> return ()
      flip whenJust setFocus $ case n of
        -- Adapt click event origin to their right panel.
        -- For the REPL and the World view, using 'Brick.Widgets.Core.clickable' correctly set the origin.
        -- However this does not seems to work for the robot and info panel.
        -- Thus we force the destination focus here.
        InventoryList -> Just RobotPanel
        InventoryListItem _ -> Just RobotPanel
        InfoViewport -> Just InfoPanel
        REPLInput -> Just REPLPanel
        WorldEditorPanelControl _ -> Just WorldEditorPanel
        _ -> Nothing
      case n of
        FocusablePanel x -> setFocus x
        _ -> return ()
    -- dispatch any other events to the focused panel handler
    _ev -> do
      fring <- use $ uiState . uiFocusRing
      case focusGetCurrent fring of
        Just (FocusablePanel x) -> ($ ev) $ case x of
          REPLPanel -> handleREPLEvent
          WorldPanel -> handleWorldEvent
          WorldEditorPanel -> EC.handleWorldEditorPanelEvent
          RobotPanel -> handleRobotPanelEvent
          InfoPanel -> handleInfoPanelEvent infoScroll
        _ -> continueWithoutRedraw

-- | Set the game to Running if it was (auto) paused otherwise to paused.
--
-- Also resets the last frame time to now. If we are pausing, it
-- doesn't matter; if we are unpausing, this is critical to
-- ensure the next frame doesn't think it has to catch up from
-- whenever the game was paused!
safeTogglePause :: EventM Name AppState ()
safeTogglePause = do
  curTime <- liftIO $ getTime Monotonic
  uiState . lastFrameTime .= curTime
  uiState . uiShowDebug .= False
  p <- gameState . runStatus Lens.<%= toggleRunStatus
  when (p == Running) $ zoomGameState finishGameTick

-- | Only unpause the game if leaving autopaused modal.
--
-- Note that the game could have been paused before opening
-- the modal, in that case, leave the game paused.
safeAutoUnpause :: EventM Name AppState ()
safeAutoUnpause = do
  runs <- use $ gameState . runStatus
  when (runs == AutoPause) safeTogglePause

toggleModal :: ModalType -> EventM Name AppState ()
toggleModal mt = do
  modal <- use $ uiState . uiModal
  case modal of
    Nothing -> openModal mt
    Just _ -> uiState . uiModal .= Nothing >> safeAutoUnpause

handleModalEvent :: V.Event -> EventM Name AppState ()
handleModalEvent = \case
  V.EvKey V.KEnter [] -> do
    mdialog <- preuse $ uiState . uiModal . _Just . modalDialog
    toggleModal QuitModal
    case dialogSelection =<< mdialog of
      Just (Button QuitButton, _) -> quitGame
      Just (Button KeepPlayingButton, _) -> toggleModal KeepPlayingModal
      Just (Button StartOverButton, StartOver currentSeed siPair) -> restartGame currentSeed siPair
      Just (Button NextButton, Next siPair) -> quitGame >> startGame siPair Nothing
      _ -> return ()
  ev -> do
    Brick.zoom (uiState . uiModal . _Just . modalDialog) (handleDialogEvent ev)
    modal <- preuse $ uiState . uiModal . _Just . modalType
    case modal of
      Just TerrainPaletteModal ->
        refreshList $ uiState . uiWorldEditor . terrainList
      Just EntityPaletteModal -> do
        refreshList $ uiState . uiWorldEditor . entityPaintList
      Just GoalModal -> case ev of
        V.EvKey (V.KChar '\t') [] -> uiState . uiGoal . focus %= focusNext
        _ -> do
          focused <- use $ uiState . uiGoal . focus
          case focusGetCurrent focused of
            Just (GoalWidgets w) -> case w of
              ObjectivesList -> do
                lw <- use $ uiState . uiGoal . listWidget
                newList <- refreshGoalList lw
                uiState . uiGoal . listWidget .= newList
              GoalSummary -> handleInfoPanelEvent modalScroll (VtyEvent ev)
            _ -> handleInfoPanelEvent modalScroll (VtyEvent ev)
      _ -> handleInfoPanelEvent modalScroll (VtyEvent ev)
   where
    refreshGoalList lw = nestEventM' lw $ handleListEventWithSeparators ev shouldSkipSelection
    refreshList z = Brick.zoom z $ BL.handleListEvent ev

getNormalizedCurrentScenarioPath :: (MonadIO m, MonadState AppState m) => m (Maybe FilePath)
getNormalizedCurrentScenarioPath =
  -- the path should be normalized and good to search in scenario collection
  use (gameState . currentScenarioPath) >>= \case
    Nothing -> return Nothing
    Just p' -> do
      gs <- use $ runtimeState . scenarios
      Just <$> liftIO (normalizeScenarioPath gs p')

saveScenarioInfoOnFinish :: (MonadIO m, MonadState AppState m) => FilePath -> m (Maybe ScenarioInfo)
saveScenarioInfoOnFinish p = do
  initialRunCode <- use $ gameState . initiallyRunCode
  t <- liftIO getZonedTime
  wc <- use $ gameState . winCondition
  let won = case wc of
        WinConditions (Won _) _ -> True
        _ -> False
  ts <- use $ gameState . ticks

  -- NOTE: This traversal is apparently not the same one as used by
  -- the scenario selection menu, so the menu needs to be updated separately.
  -- See Note [scenario menu update]
  let currentScenarioInfo :: Traversal' AppState ScenarioInfo
      currentScenarioInfo = runtimeState . scenarios . scenarioItemByPath p . _SISingle . _2

  replHist <- use $ uiState . uiREPL . replHistory
  let determinator = CodeSizeDeterminators initialRunCode $ replHist ^. replHasExecutedManualInput
  currentScenarioInfo
    %= updateScenarioInfoOnFinish determinator t ts won
  status <- preuse currentScenarioInfo
  case status of
    Nothing -> return ()
    Just si -> do
      let segments = splitDirectories p
      case segments of
        firstDir : _ -> do
          when (won && firstDir == tutorialsDirname) $
            attainAchievement' t (Just p) (GlobalAchievement CompletedSingleTutorial)
        _ -> return ()
      liftIO $ saveScenarioInfo p si
  return status

-- | Write the @ScenarioInfo@ out to disk when finishing a game (i.e. on winning or exit).
saveScenarioInfoOnFinishNocheat :: (MonadIO m, MonadState AppState m) => m ()
saveScenarioInfoOnFinishNocheat = do
  -- Don't save progress if we are in cheat mode
  cheat <- use $ uiState . uiCheatMode
  unless cheat $ do
    -- the path should be normalized and good to search in scenario collection
    getNormalizedCurrentScenarioPath >>= \case
      Nothing -> return ()
      Just p -> void $ saveScenarioInfoOnFinish p

-- | Write the @ScenarioInfo@ out to disk when exiting a game.
saveScenarioInfoOnQuit :: (MonadIO m, MonadState AppState m) => m ()
saveScenarioInfoOnQuit = do
  -- Don't save progress if we are in cheat mode
  -- NOTE This check is duplicated in "saveScenarioInfoOnFinishNocheat"
  cheat <- use $ uiState . uiCheatMode
  unless cheat $ do
    getNormalizedCurrentScenarioPath >>= \case
      Nothing -> return ()
      Just p -> do
        maybeSi <- saveScenarioInfoOnFinish p
        -- Note [scenario menu update]
        -- Ensures that the scenario selection menu gets updated
        -- with the high score/completion status
        forM_
          maybeSi
          ( uiState
              . uiMenu
              . _NewGameMenu
              . ix 0
              . BL.listSelectedElementL
              . _SISingle
              . _2
              .=
          )

        -- See what scenario is currently focused in the menu.  Depending on how the
        -- previous scenario ended (via quit vs. via win), it might be the same as
        -- currentScenarioPath or it might be different.
        curPath <- preuse $ uiState . uiMenu . _NewGameMenu . ix 0 . BL.listSelectedElementL . _SISingle . _2 . scenarioPath
        -- Now rebuild the NewGameMenu so it gets the updated ScenarioInfo,
        -- being sure to preserve the same focused scenario.
        sc <- use $ runtimeState . scenarios
        forM_ (mkNewGameMenu cheat sc (fromMaybe p curPath)) (uiState . uiMenu .=)

-- | Quit a game.
--
-- * writes out the updated REPL history to a @.swarm_history@ file
-- * saves current scenario status (InProgress/Completed)
-- * advances the menu to the next scenario IF the current one was won
-- * returns to the previous menu
quitGame :: EventM Name AppState ()
quitGame = do
  -- Write out REPL history.
  history <- use $ uiState . uiREPL . replHistory
  let hist = mapMaybe getREPLEntry $ getLatestREPLHistoryItems maxBound history
  liftIO $ (`T.appendFile` T.unlines hist) =<< getSwarmHistoryPath True

  -- Save scenario status info.
  saveScenarioInfoOnQuit

  -- Automatically advance the menu to the next scenario iff the
  -- player has won the current one.
  wc <- use $ gameState . winCondition
  case wc of
    WinConditions (Won _) _ -> uiState . uiMenu %= advanceMenu
    _ -> return ()

  -- Either quit the entire app (if the scenario was chosen directly
  -- from the command line) or return to the menu (if the scenario was
  -- chosen from the menu).
  menu <- use $ uiState . uiMenu
  case menu of
    NoMenu -> halt
    _ -> uiState . uiPlaying .= False

------------------------------------------------------------
-- Handling Frame events
------------------------------------------------------------

-- | Run the game for a single /frame/ (/i.e./ screen redraw), then
--   update the UI.  Depending on how long it is taking to draw each
--   frame, and how many ticks per second we are trying to achieve,
--   this may involve stepping the game any number of ticks (including
--   zero).
runFrameUI :: EventM Name AppState ()
runFrameUI = do
  runFrame
  redraw <- updateUI
  unless redraw continueWithoutRedraw

-- | Run the game for a single frame, without updating the UI.
runFrame :: EventM Name AppState ()
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
runFrameTicks :: TimeSpec -> EventM Name AppState ()
runFrameTicks dt = do
  a <- use (uiState . accumulatedTime)
  t <- use (uiState . frameTickCount)

  -- Ensure there is still enough time left, and we haven't hit the
  -- tick limit for this frame.
  when (a >= dt && t < ticksPerFrameCap) $ do
    -- If so, do a tick, count it, subtract dt from the accumulated time,
    -- and loop!
    runGameTick
    uiState . tickCount += 1
    uiState . frameTickCount += 1
    uiState . accumulatedTime -= dt
    runFrameTicks dt

-- | Run the game for a single tick, and update the UI.
runGameTickUI :: EventM Name AppState ()
runGameTickUI = runGameTick >> void updateUI

-- | Modifies the game state using a fused-effect state action.
zoomGameState :: (MonadState AppState m, MonadIO m) => Fused.StateC GameState (Fused.LiftC IO) a -> m a
zoomGameState f = do
  gs <- use gameState
  (gs', a) <- liftIO (Fused.runM (Fused.runState gs f))
  gameState .= gs'
  return a

updateAchievements :: EventM Name AppState ()
updateAchievements = do
  -- Merge the in-game achievements with the master list in UIState
  achievementsFromGame <- use $ gameState . gameAchievements
  let wrappedGameAchievements = M.mapKeys GameplayAchievement achievementsFromGame

  oldMasterAchievementsList <- use $ uiState . uiAchievements
  uiState . uiAchievements %= M.unionWith (<>) wrappedGameAchievements

  -- Don't save to disk unless there was a change in the attainment list.
  let incrementalAchievements = wrappedGameAchievements `M.difference` oldMasterAchievementsList
  unless (null incrementalAchievements) $ do
    -- TODO: #916 This is where new achievements would be displayed in a popup
    newAchievements <- use $ uiState . uiAchievements
    liftIO $ saveAchievementsInfo $ M.elems newAchievements

-- | Run the game for a single tick (/without/ updating the UI).
--   Every robot is given a certain amount of maximum computation to
--   perform a single world action (like moving, turning, grabbing,
--   etc.).
runGameTick :: EventM Name AppState ()
runGameTick = do
  ticked <- zoomGameState gameTick
  when ticked updateAchievements

-- | Update the UI.  This function is used after running the
--   game for some number of ticks.
updateUI :: EventM Name AppState Bool
updateUI = do
  loadVisibleRegion

  -- If the game state indicates a redraw is needed, invalidate the
  -- world cache so it will be redrawn.
  g <- use gameState
  when (g ^. needsRedraw) $ invalidateCacheEntry WorldCache

  -- The hash of the robot whose inventory is currently displayed (if any)
  listRobotHash <- fmap fst <$> use (uiState . uiInventory)

  -- The hash of the focused robot (if any)
  fr <- use (gameState . to focusedRobot)
  let focusedRobotHash = view inventoryHash <$> fr

  -- Check if the inventory list needs to be updated.
  shouldUpdate <- use (uiState . uiInventoryShouldUpdate)

  -- Whether the focused robot is too far away to sense, & whether
  -- that has recently changed
  dist <- use (gameState . to focusedRange)
  farOK <- liftA2 (||) (use (gameState . creativeMode)) (use (gameState . worldScrollable))
  let tooFar = not farOK && dist == Just Far
      farChanged = tooFar /= isNothing listRobotHash

  -- If the robot moved in or out of range, or hashes don't match
  -- (either because which robot (or whether any robot) is focused
  -- changed, or the focused robot's inventory changed), or the
  -- inventory was flagged to be updated, regenerate the inventory list.
  inventoryUpdated <-
    if farChanged || (not farChanged && listRobotHash /= focusedRobotHash) || shouldUpdate
      then do
        Brick.zoom uiState $ populateInventoryList (if tooFar then Nothing else fr)
        (uiState . uiInventoryShouldUpdate) .= False
        pure True
      else pure False

  -- Now check if the base finished running a program entered at the REPL.
  replUpdated <- case g ^. replStatus of
    -- It did, and the result was the unit value.  Just reset replStatus.
    REPLWorking (Typed (Just VUnit) typ reqs) -> do
      gameState . replStatus .= REPLDone (Just $ Typed VUnit typ reqs)
      pure True

    -- It did, and returned some other value.  Pretty-print the
    -- result as a REPL output, with its type, and reset the replStatus.
    REPLWorking (Typed (Just v) pty reqs) -> do
      let finalType = stripCmd pty
      let val = Typed (stripVResult v) finalType reqs
      itIx <- use (gameState . replNextValueIndex)
      let itName = fromString $ "it" ++ show itIx
      let out = T.intercalate " " [itName, ":", prettyText finalType, "=", into (prettyValue v)]
      uiState . uiREPL . replHistory %= addREPLItem (REPLOutput out)
      gameState . replStatus .= REPLDone (Just val)
      gameState . baseRobot . robotContext . at itName .= Just val
      gameState . replNextValueIndex %= (+ 1)
      pure True

    -- Otherwise, do nothing.
    _ -> pure False

  -- If the focused robot's log has been updated and the UI focus
  -- isn't currently on the inventory or info panels, attempt to
  -- automatically switch to the logger and scroll all the way down so
  -- the new message can be seen.
  uiState . uiScrollToEnd .= False
  logUpdated <- do
    -- If the inventory or info panels are currently focused, it would
    -- be rude to update them right under the user's nose, so consider
    -- them "sticky".  They will be updated as soon as the player moves
    -- the focus away.
    fring <- use $ uiState . uiFocusRing
    let sticky = focusGetCurrent fring `elem` map (Just . FocusablePanel) [RobotPanel, InfoPanel]

    -- Check if the robot log was updated and we are allowed to change
    -- the inventory+info panels.
    case maybe False (view robotLogUpdated) fr && not sticky of
      False -> pure False
      True -> do
        -- Reset the log updated flag
        zoomGameState clearFocusedRobotLogUpdated

        -- Find and focus an equipped "logger" device in the inventory list.
        let isLogger (EquippedEntry e) = e ^. entityName == "logger"
            isLogger _ = False
            focusLogger = BL.listFindBy isLogger

        uiState . uiInventory . _Just . _2 %= focusLogger

        -- Now inform the UI that it should scroll the info panel to
        -- the very end.
        uiState . uiScrollToEnd .= True
        pure True

  goalOrWinUpdated <- doGoalUpdates

  let redraw =
        g ^. needsRedraw
          || inventoryUpdated
          || replUpdated
          || logUpdated
          || goalOrWinUpdated
  pure redraw

-- | Either pops up the updated Goals modal
-- or pops up the Congratulations (Win) modal, or pops
-- up the Condolences (Lose) modal.
-- The Win modal will take precedence if the player
-- has met the necessary conditions to win the game.
--
-- If the player chooses to "Keep Playing" from the Win modal, the
-- updated Goals will then immediately appear.
-- This is desirable for:
-- * feedback as to the final goal the player accomplished,
-- * as a summary of all of the goals of the game
-- * shows the player more "optional" goals they can continue to pursue
doGoalUpdates :: EventM Name AppState Bool
doGoalUpdates = do
  curGoal <- use (uiState . uiGoal . goalsContent)
  isCheating <- use (uiState . uiCheatMode)
  curWinCondition <- use (gameState . winCondition)
  announcementsSeq <- use (gameState . announcementQueue)
  let announcementsList = toList announcementsSeq

  -- Decide whether we need to update the current goal text and pop
  -- up a modal dialog.
  case curWinCondition of
    NoWinCondition -> return False
    WinConditions (Unwinnable False) x -> do
      -- This clears the "flag" that the Lose dialog needs to pop up
      gameState . winCondition .= WinConditions (Unwinnable True) x
      openModal $ ScenarioEndModal LoseModal
      saveScenarioInfoOnFinishNocheat
      return True
    WinConditions (Won False) x -> do
      -- This clears the "flag" that the Win dialog needs to pop up
      gameState . winCondition .= WinConditions (Won True) x
      openModal $ ScenarioEndModal WinModal
      saveScenarioInfoOnFinishNocheat
      -- We do NOT advance the New Game menu to the next item here (we
      -- used to!), because we do not know if the user is going to
      -- select 'keep playing' or 'next challenge'.  We maintain the
      -- invariant that the current menu item is always the same as
      -- the scenario currently being played.  If the user either (1)
      -- quits to the menu or (2) selects 'next challenge' we will
      -- advance the menu at that point.
      return True
    WinConditions _ oc -> do
      let newGoalTracking = GoalTracking announcementsList $ constructGoalMap isCheating oc
          -- The "uiGoal" field is initialized with empty members, so we know that
          -- this will be the first time showing it if it will be nonempty after previously
          -- being empty.
          isFirstGoalDisplay = hasAnythingToShow newGoalTracking && not (hasAnythingToShow curGoal)
          goalWasUpdated = isFirstGoalDisplay || not (null announcementsList)

      -- Decide whether to show a pop-up modal congratulating the user on
      -- successfully completing the current challenge.
      when goalWasUpdated $ do
        let hasMultiple = hasMultipleGoals newGoalTracking
            defaultFocus =
              if hasMultiple
                then ObjectivesList
                else GoalSummary

            ring =
              focusRing $
                map GoalWidgets $
                  if hasMultiple
                    then listEnums
                    else [GoalSummary]

        -- The "uiGoal" field is necessary at least to "persist" the data that is needed
        -- if the player chooses to later "recall" the goals dialog with CTRL+g.
        uiState
          . uiGoal
          .= GoalDisplay
            newGoalTracking
            (GR.makeListWidget newGoalTracking)
            (focusSetCurrent (GoalWidgets defaultFocus) ring)

        -- This clears the "flag" that indicate that the goals dialog needs to be
        -- automatically popped up.
        gameState . announcementQueue .= mempty

        hideGoals <- use $ uiState . uiHideGoals
        unless hideGoals $
          openModal GoalModal

      return goalWasUpdated

-- | Strips top-level `cmd` from type (in case of REPL evaluation),
--   and returns a boolean to indicate if it happened
stripCmd :: Polytype -> Polytype
stripCmd (Forall xs (TyCmd ty)) = Forall xs ty
stripCmd pty = pty

------------------------------------------------------------
-- REPL events
------------------------------------------------------------

-- | Set the REPLForm to the given value, resetting type error checks to Nothing
--   and removing uiError.
resetREPL :: T.Text -> REPLPrompt -> UIState -> UIState
resetREPL t r ui =
  ui
    & uiREPL . replPromptText .~ t
    & uiREPL . replPromptType .~ r
    & uiError .~ Nothing

-- | Handle a user input event for the REPL.
handleREPLEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleREPLEvent x = do
  s <- get
  let repl = s ^. uiState . uiREPL
      controlMode = repl ^. replControlMode
      uinput = repl ^. replPromptText
  case x of
    -- Handle Ctrl-c here so we can always cancel the currently running
    -- base program no matter what REPL control mode we are in.
    ControlChar 'c' -> do
      gameState . baseRobot . machine %= cancel
      uiState . uiREPL . replPromptType .= CmdPrompt []
      uiState . uiREPL . replPromptText .= ""

    -- Handle M-p and M-k, shortcuts for toggling pilot + key handler modes.
    MetaChar 'p' ->
      onlyCreative $ do
        curMode <- use $ uiState . uiREPL . replControlMode
        case curMode of
          Piloting -> uiState . uiREPL . replControlMode .= Typing
          _ ->
            if T.null uinput
              then uiState . uiREPL . replControlMode .= Piloting
              else uiState . uiError ?= "Please clear the REPL first."
    MetaChar 'k' -> do
      when (isJust (s ^. gameState . inputHandler)) $ do
        curMode <- use $ uiState . uiREPL . replControlMode
        (uiState . uiREPL . replControlMode) .= case curMode of Handling -> Typing; _ -> Handling

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
  mhandler <- use $ gameState . inputHandler
  case mhandler of
    -- Shouldn't be possible to get here if there is no input handler, but
    -- if we do somehow, just do nothing.
    Nothing -> return ()
    Just (_, handler) -> do
      -- Make sure the base is currently idle; if so, apply the
      -- installed input handler function to a `key` value
      -- representing the typed input.
      working <- use $ gameState . replWorking
      unless working $ do
        s <- get
        let topCtx = topContext s
            handlerCESK = Out (VKey kc) (topCtx ^. defStore) [FApp handler, FExec]
        gameState . baseRobot . machine .= handlerCESK
        gameState %= execState (activateRobot 0)

-- | Handle a user "piloting" input event for the REPL.
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
    uiState . uiREPL %= setCmd (cmdText <> ";")
    modify validateREPLForm
    handleREPLEventTyping $ Key V.KEnter

  setCmd nt repl =
    repl
      & replPromptText .~ nt
      & replPromptType .~ CmdPrompt []

runBaseWebCode :: (MonadState AppState m) => T.Text -> m ()
runBaseWebCode uinput = do
  s <- get
  let topCtx = topContext s
  unless (s ^. gameState . replWorking) $
    runBaseCode topCtx uinput

runBaseCode :: (MonadState AppState m) => RobotContext -> T.Text -> m ()
runBaseCode topCtx uinput =
  case processTerm' (topCtx ^. defTypes) (topCtx ^. defReqs) uinput of
    Right mt -> do
      uiState %= resetREPL "" (CmdPrompt [])
      uiState . uiREPL . replHistory %= addREPLItem (REPLEntry uinput)
      uiState . uiREPL . replHistory . replHasExecutedManualInput .= True
      runBaseTerm topCtx mt
    Left err -> do
      uiState . uiError ?= err

runBaseTerm :: (MonadState AppState m) => RobotContext -> Maybe ProcessedTerm -> m ()
runBaseTerm topCtx =
  modify . maybe id startBaseProgram
 where
  -- The player typed something at the REPL and hit Enter; this
  -- function takes the resulting ProcessedTerm (if the REPL
  -- input is valid) and sets up the base robot to run it.
  startBaseProgram t@(ProcessedTerm (Module tm _) reqs reqCtx) =
    -- Set the REPL status to Working
    (gameState . replStatus .~ REPLWorking (Typed Nothing (tm ^. sType) reqs))
      -- The `reqCtx` maps names of variables defined in the
      -- term (by `def` statements) to their requirements.
      -- E.g. if we had `def m = move end`, the reqCtx would
      -- record the fact that `m` needs the `move` capability.
      -- We simply add the entire `reqCtx` to the robot's
      -- context, so we can look up requirements if we later
      -- need to requirements-check an argument to `build` or
      -- `reprogram` at runtime.  See the discussion at
      -- https://github.com/swarm-game/swarm/pull/827 for more
      -- details.
      . (gameState . baseRobot . robotContext . defReqs <>~ reqCtx)
      -- Set up the robot's CESK machine to evaluate/execute the
      -- given term, being sure to initialize the CESK machine
      -- environment and store from the top-level context.
      . (gameState . baseRobot . machine .~ initMachine t (topCtx ^. defVals) (topCtx ^. defStore))
      -- Finally, be sure to activate the base robot.
      . (gameState %~ execState (activateRobot 0))

-- | Handle a user input event for the REPL.
handleREPLEventTyping :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleREPLEventTyping = \case
  Key V.KEnter -> do
    s <- get
    let topCtx = topContext s
        repl = s ^. uiState . uiREPL
        uinput = repl ^. replPromptText

    if not $ s ^. gameState . replWorking
      then case repl ^. replPromptType of
        CmdPrompt _ -> runBaseCode topCtx uinput
        SearchPrompt hist ->
          case lastEntry uinput hist of
            Nothing -> uiState %= resetREPL "" (CmdPrompt [])
            Just found
              | T.null uinput -> uiState %= resetREPL "" (CmdPrompt [])
              | otherwise -> do
                  uiState %= resetREPL found (CmdPrompt [])
                  modify validateREPLForm
      else continueWithoutRedraw
  Key V.KUp -> modify $ adjReplHistIndex Older
  Key V.KDown -> modify $ adjReplHistIndex Newer
  ControlChar 'r' -> do
    s <- get
    let uinput = s ^. uiState . uiREPL . replPromptText
    case s ^. uiState . uiREPL . replPromptType of
      CmdPrompt _ -> uiState . uiREPL . replPromptType .= SearchPrompt (s ^. uiState . uiREPL . replHistory)
      SearchPrompt rh -> case lastEntry uinput rh of
        Nothing -> pure ()
        Just found -> uiState . uiREPL . replPromptType .= SearchPrompt (removeEntry found rh)
  CharKey '\t' -> do
    s <- get
    let names = s ^.. gameState . baseRobot . robotContext . defTypes . to assocs . traverse . _1
    uiState . uiREPL %= tabComplete names (s ^. gameState . entityMap)
    modify validateREPLForm
  EscapeKey -> do
    formSt <- use $ uiState . uiREPL . replPromptType
    case formSt of
      CmdPrompt {} -> continueWithoutRedraw
      SearchPrompt _ ->
        uiState %= resetREPL "" (CmdPrompt [])
  ControlChar 'd' -> do
    text <- use $ uiState . uiREPL . replPromptText
    if text == T.empty
      then toggleModal QuitModal
      else continueWithoutRedraw
  -- finally if none match pass the event to the editor
  ev -> do
    Brick.zoom (uiState . uiREPL . replPromptEditor) (handleEditorEvent ev)
    uiState . uiREPL . replPromptType %= \case
      CmdPrompt _ -> CmdPrompt [] -- reset completions on any event passed to editor
      SearchPrompt a -> SearchPrompt a
    modify validateREPLForm

data CompletionType
  = FunctionName
  | EntityName
  deriving (Eq)

-- | Try to complete the last word in a partially-entered REPL prompt using
--   reserved words and names in scope (in the case of function names) or
--   entity names (in the case of string literals).
tabComplete :: [Var] -> EntityMap -> REPLState -> REPLState
tabComplete names em repl = case repl ^. replPromptType of
  SearchPrompt _ -> repl
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

  possibleWords = reservedWords ++ names

  entityNames = M.keys $ entitiesByName em

  t = repl ^. replPromptText
  setCmd nt ms =
    repl
      & replPromptText .~ nt
      & replPromptType .~ CmdPrompt ms

-- | Validate the REPL input when it changes: see if it parses and
--   typechecks, and set the color accordingly.
validateREPLForm :: AppState -> AppState
validateREPLForm s =
  case replPrompt of
    CmdPrompt _
      | T.null uinput ->
          let theType = s ^. gameState . replStatus . replActiveType
           in s & uiState . uiREPL . replType .~ theType
    CmdPrompt _
      | otherwise ->
          let result = processTerm' (topCtx ^. defTypes) (topCtx ^. defReqs) uinput
              theType = case result of
                Right (Just (ProcessedTerm (Module tm _) _ _)) -> Just (tm ^. sType)
                _ -> Nothing
           in s
                & uiState . uiREPL . replValid .~ isRight result
                & uiState . uiREPL . replType .~ theType
    SearchPrompt _ -> s
 where
  uinput = s ^. uiState . uiREPL . replPromptText
  replPrompt = s ^. uiState . uiREPL . replPromptType
  topCtx = topContext s

-- | Update our current position in the REPL history.
adjReplHistIndex :: TimeDir -> AppState -> AppState
adjReplHistIndex d s =
  s
    & uiState . uiREPL %~ moveREPL
    & validateREPLForm
 where
  moveREPL :: REPLState -> REPLState
  moveREPL repl =
    newREPL
      & (if replIndexIsAtInput (repl ^. replHistory) then saveLastEntry else id)
      & (if oldEntry /= newEntry then showNewEntry else id)
   where
    -- new AppState after moving the repl index
    newREPL :: REPLState
    newREPL = repl & replHistory %~ moveReplHistIndex d oldEntry

    saveLastEntry = replLast .~ (repl ^. replPromptText)
    showNewEntry = (replPromptEditor .~ newREPLEditor newEntry) . (replPromptType .~ CmdPrompt [])
    -- get REPL data
    getCurrEntry = fromMaybe (repl ^. replLast) . getCurrentItemText . view replHistory
    oldEntry = getCurrEntry repl
    newEntry = getCurrEntry newREPL

------------------------------------------------------------
-- World events
------------------------------------------------------------

worldScrollDist :: Int32
worldScrollDist = 8

onlyCreative :: (MonadState AppState m) => m () -> m ()
onlyCreative a = do
  c <- use $ gameState . creativeMode
  when c a

-- | Handle a user input event in the world view panel.
handleWorldEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleWorldEvent = \case
  Key k
    | k `elem` moveKeys -> do
        c <- use $ gameState . creativeMode
        s <- use $ gameState . worldScrollable
        when (c || s) $ scrollView (.+^ (worldScrollDist *^ keyToDir k))
  CharKey 'c' -> do
    invalidateCacheEntry WorldCache
    gameState . viewCenterRule .= VCRobot 0
  -- show fps
  CharKey 'f' -> uiState . uiShowFPS %= not
  -- Fall-through case: don't do anything.
  _ -> continueWithoutRedraw
 where
  moveKeys =
    [ V.KUp
    , V.KDown
    , V.KLeft
    , V.KRight
    , V.KChar 'h'
    , V.KChar 'j'
    , V.KChar 'k'
    , V.KChar 'l'
    ]

-- | Manually scroll the world view.
scrollView :: (Location -> Location) -> EventM Name AppState ()
scrollView update = do
  -- Manually invalidate the 'WorldCache' instead of just setting
  -- 'needsRedraw'.  I don't quite understand why the latter doesn't
  -- always work, but there seems to be some sort of race condition
  -- where 'needsRedraw' gets reset before the UI drawing code runs.
  invalidateCacheEntry WorldCache
  gameState %= modifyViewCenter (fmap update)

-- | Convert a directional key into a direction.
keyToDir :: V.Key -> Heading
keyToDir V.KUp = north
keyToDir V.KDown = south
keyToDir V.KRight = east
keyToDir V.KLeft = west
keyToDir (V.KChar 'h') = west
keyToDir (V.KChar 'j') = south
keyToDir (V.KChar 'k') = north
keyToDir (V.KChar 'l') = east
keyToDir _ = zero

-- | Adjust the ticks per second speed.
adjustTPS :: (Int -> Int -> Int) -> AppState -> AppState
adjustTPS (+/-) = uiState . lgTicksPerSecond %~ (+/- 1)

------------------------------------------------------------
-- Robot panel events
------------------------------------------------------------

-- | Handle user input events in the robot panel.
handleRobotPanelEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleRobotPanelEvent bev = do
  search <- use (uiState . uiInventorySearch)
  case search of
    Just _ -> handleInventorySearchEvent bev
    Nothing -> case bev of
      Key V.KEnter ->
        gets focusedEntity >>= maybe continueWithoutRedraw descriptionModal
      CharKey 'm' ->
        gets focusedEntity >>= maybe continueWithoutRedraw makeEntity
      CharKey '0' -> do
        uiState . uiInventoryShouldUpdate .= True
        uiState . uiShowZero %= not
      CharKey ';' -> do
        uiState . uiInventoryShouldUpdate .= True
        uiState . uiInventorySort %= cycleSortOrder
      CharKey ':' -> do
        uiState . uiInventoryShouldUpdate .= True
        uiState . uiInventorySort %= cycleSortDirection
      CharKey '/' -> do
        uiState . uiInventoryShouldUpdate .= True
        uiState . uiInventorySearch .= Just ""
      VtyEvent ev -> handleInventoryListEvent ev
      _ -> continueWithoutRedraw

-- | Handle an event to navigate through the inventory list.
handleInventoryListEvent :: V.Event -> EventM Name AppState ()
handleInventoryListEvent ev = do
  -- Note, refactoring like this is tempting:
  --
  --   Brick.zoom (uiState . uiInventory . _Just . _2) (handleListEventWithSeparators ev (is _Separator))
  --
  -- However, this does not work since we want to skip redrawing in the no-list case!

  mList <- preuse $ uiState . uiInventory . _Just . _2
  case mList of
    Nothing -> continueWithoutRedraw
    Just l -> do
      l' <- nestEventM' l (handleListEventWithSeparators ev (is _Separator))
      uiState . uiInventory . _Just . _2 .= l'

-- | Handle a user input event in the robot/inventory panel, while in
--   inventory search mode.
handleInventorySearchEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleInventorySearchEvent = \case
  -- Escape: stop filtering and go back to regular inventory mode
  EscapeKey -> do
    uiState . uiInventoryShouldUpdate .= True
    uiState . uiInventorySearch .= Nothing
  -- Enter: return to regular inventory mode, and pop out the selected item
  Key V.KEnter -> do
    uiState . uiInventoryShouldUpdate .= True
    uiState . uiInventorySearch .= Nothing
    gets focusedEntity >>= maybe continueWithoutRedraw descriptionModal
  -- Any old character: append to the current search string
  CharKey c -> do
    uiState . uiInventoryShouldUpdate .= True
    uiState . uiInventorySearch %= fmap (`snoc` c)
  -- Backspace: chop the last character off the end of the current search string
  BackspaceKey -> do
    uiState . uiInventoryShouldUpdate .= True
    uiState . uiInventorySearch %= fmap (T.dropEnd 1)
  -- Handle any other event as list navigation, so we can look through
  -- the filtered inventory using e.g. arrow keys
  VtyEvent ev -> handleInventoryListEvent ev
  _ -> continueWithoutRedraw

-- | Attempt to make an entity selected from the inventory, if the
--   base is not currently busy.
makeEntity :: Entity -> EventM Name AppState ()
makeEntity e = do
  s <- get
  let name = e ^. entityName
      mkPT = [tmQ| make $str:name |]
      topStore =
        fromMaybe emptyStore $
          s ^? gameState . baseRobot . robotContext . defStore

  case isActive <$> (s ^? gameState . baseRobot) of
    Just False -> do
      gameState . replStatus .= REPLWorking (Typed Nothing PolyUnit (R.singletonCap CMake))
      gameState . baseRobot . machine .= initMachine mkPT empty topStore
      gameState %= execState (activateRobot 0)
    _ -> continueWithoutRedraw

-- | Display a modal window with the description of an entity.
descriptionModal :: Entity -> EventM Name AppState ()
descriptionModal e = do
  s <- get
  uiState . uiModal ?= generateModal s (DescriptionModal e)

------------------------------------------------------------
-- Info panel events
------------------------------------------------------------

-- | Handle user events in the info panel (just scrolling).
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
