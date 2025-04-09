{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.StateUpdate (
  initAppState,
  initPersistentState,
  constructAppState,
  initAppStateForScenario,
  classicGame0,
  startGame,
  startGameWithSeed,
  restartGame,
  attainAchievement,
  attainAchievement',
  loadScenarioInfoFromPath,
  scenarioToAppState,
) where

import Brick.AttrMap (applyAttrMappings)
import Brick.Focus
import Brick.Widgets.List qualified as BL
import Control.Applicative ((<|>))
import Control.Carrier.Accum.FixedStrict (runAccum)
import Control.Effect.Accum
import Control.Effect.Lift
import Control.Effect.Throw
import Control.Lens hiding (from, (<.>))
import Control.Monad (guard, unless, void)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState, execStateT)
import Data.Bifunctor (first)
import Data.Foldable qualified as F
import Data.List qualified as List
import Data.List.Extra (enumerate)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getZonedTime)
import Swarm.Failure (SystemFailure (..))
import Swarm.Game.Land
import Swarm.Game.Scenario (
  ScenarioInputs (..),
  gsiScenarioInputs,
  loadScenario,
  scenarioAttrs,
  scenarioLandscape,
  scenarioOperation,
  scenarioSolution,
  scenarioWorlds,
 )
import Swarm.Game.Scenario.Scoring.Best
import Swarm.Game.Scenario.Scoring.ConcreteMetrics
import Swarm.Game.Scenario.Scoring.GenericMetrics
import Swarm.Game.Scenario.Status
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type (originalStructureDefinitions)
import Swarm.Game.ScenarioInfo (
  ScenarioCollection,
  normalizeScenarioPath,
  scenarioItemByPath,
  _SISingle,
 )
import Swarm.Game.State
import Swarm.Game.State.Initialize
import Swarm.Game.State.Landscape
import Swarm.Game.State.Runtime
import Swarm.Game.State.Substate
import Swarm.Game.World.Gen (Seed)
import Swarm.Log (LogSource (SystemLog), Severity (..))
import Swarm.Pretty (prettyText)
import Swarm.ResourceLoading (getSwarmHistoryPath)
import Swarm.TUI.Editor.Model
import Swarm.TUI.Editor.Model qualified as EM
import Swarm.TUI.Editor.Util qualified as EU
import Swarm.TUI.Inventory.Sorting
import Swarm.TUI.Launch.Model (toSerializableParams)
import Swarm.TUI.Model
import Swarm.TUI.Model.Achievements
import Swarm.TUI.Model.DebugOption (DebugOption (LoadTestingScenarios))
import Swarm.TUI.Model.Dialog
import Swarm.TUI.Model.KeyBindings
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.UI
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.View.Attribute.Attr (getWorldAttrName, swarmAttrMap)
import Swarm.TUI.View.Attribute.CustomStyling (toAttrPair)
import Swarm.TUI.View.Robot
import Swarm.TUI.View.Robot.Type
import Swarm.TUI.View.Structure qualified as SR
import Swarm.Util
import Swarm.Util.Effect (asExceptT, withThrow)
import System.Clock

-- | Initialize the 'AppState' from scratch.
initAppState ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  AppOpts ->
  m AppState
initAppState opts = do
  (rs, ui, keyHandling) <- initPersistentState opts
  constructAppState rs ui keyHandling opts

-- | Add some system failures to the list of messages in the
--   'RuntimeState'.
addWarnings :: RuntimeState -> [SystemFailure] -> RuntimeState
addWarnings = List.foldl' logWarning
 where
  logWarning rs' w = rs' & eventLog %~ logEvent SystemLog Error "UI Loading" (prettyText w)

-- | Based on the command line options, should we skip displaying the
--   menu?
skipMenu :: AppOpts -> Bool
skipMenu AppOpts {..} = isJust userScenario || isRunningInitialProgram || isJust userSeed
 where
  isRunningInitialProgram = isJust scriptToRun || autoPlay

-- | Initialize the more persistent parts of the app state, /i.e./ the
--   'RuntimeState' and 'UIState'.  This is split out into a separate
--   function so that in the integration test suite we can call this
--   once and reuse the resulting states for all tests.
initPersistentState ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  AppOpts ->
  m (RuntimeState, UIState, KeyEventHandlingState)
initPersistentState opts@(AppOpts {..}) = do
  (warnings :: Seq SystemFailure, (initRS, initUI, initKs)) <- runAccum mempty $ do
    rs <-
      initRuntimeState
        RuntimeOptions
          { startPaused = pausedAtStart
          , pauseOnObjectiveCompletion = autoShowObjectives
          , loadTestScenarios = Set.member LoadTestingScenarios debugOptions
          }
    let showMainMenu = not (skipMenu opts)
    ui <- initUIState UIInitOptions {..}
    ks <- initKeyHandlingState
    return (rs, ui, ks)
  let initRS' = addWarnings initRS (F.toList warnings)
  return (initRS', initUI, initKs)

loadScenarioInfoFromPath ::
  ScenarioCollection ScenarioInfo ->
  FilePath ->
  ScenarioInfo
loadScenarioInfoFromPath ss path =
  fromMaybe (ScenarioInfo path NotStarted) currentScenarioInfo
 where
  currentScenarioInfo = ss ^? scenarioItemByPath path . _SISingle . _2

-- | Construct an 'AppState' from an already-loaded 'RuntimeState' and
--   'UIState', given the 'AppOpts' the app was started with.
constructAppState ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  RuntimeState ->
  UIState ->
  KeyEventHandlingState ->
  AppOpts ->
  m AppState
constructAppState rs ui key opts@(AppOpts {..}) = do
  historyT <- sendIO $ readFileMayT =<< getSwarmHistoryPath False
  let history = maybe [] (map mkREPLSubmission . T.lines) historyT
  startTime <- sendIO $ getTime Monotonic

  let gs = initGameState (rs ^. stdGameConfigInputs)
      ps = PlayState gs $ initialUiGameplay startTime history
  case skipMenu opts of
    False -> return $ AppState (ps & uiGameplay . uiTiming . lgTicksPerSecond .~ defaultInitLgTicksPerSecond) ui key rs
    True -> do
      let tem = gs ^. landscape . terrainAndEntities
      (scenario, path) <-
        loadScenario
          (fromMaybe "classic" userScenario)
          (ScenarioInputs (initWorldMap . gsiScenarioInputs . initState $ rs ^. stdGameConfigInputs) tem)
      maybeRunScript <- traverse parseCodeFile scriptToRun

      let maybeAutoplay = do
            guard autoPlay
            soln <- scenario ^. scenarioOperation . scenarioSolution
            return $ CodeToRun ScenarioSuggested soln
          codeToRun = maybeAutoplay <|> maybeRunScript

      let si = loadScenarioInfoFromPath (rs ^. scenarios) path

      sendIO $
        execStateT
          (startGameWithSeed (scenario, si) $ LaunchParams (pure userSeed) (pure codeToRun))
          (AppState ps ui key rs)
 where
  initialUiGameplay startTime history =
    UIGameplay
      { _uiFocusRing = initFocusRing
      , _uiWorldCursor = Nothing
      , _uiWorldEditor = initialWorldEditor startTime
      , _uiREPL = initREPLState $ newREPLHistory history
      , _uiInventory =
          UIInventory
            { _uiInventoryList = Nothing
            , _uiInventorySort = defaultSortOptions
            , _uiInventorySearch = Nothing
            , _uiShowZero = True
            , _uiInventoryShouldUpdate = False
            }
      , _uiScrollToEnd = False
      , _uiDialogs =
          UIDialogs
            { _uiModal = Nothing
            , _uiGoal = emptyGoalDisplay
            , _uiStructure = emptyStructureDisplay
            , _uiRobot =
                RobotDisplay
                  { _robotDetailsFocus = focusRing $ map (RobotsListDialog . SingleRobotDetails) enumerate
                  , _isDetailsOpened = False
                  , _robotListContent = emptyRobotDisplay debugOptions
                  }
            }
      , _uiIsAutoPlay = False
      , _uiAutoShowObjectives = autoShowObjectives
      , _uiTiming =
          UITiming
            { _uiShowFPS = False
            , _uiTPF = 0
            , _uiFPS = 0
            , _lgTicksPerSecond = speed
            , _lastFrameTime = startTime
            , _accumulatedTime = 0
            , _lastInfoTime = 0
            , _tickCount = 0
            , _frameCount = 0
            , _frameTickCount = 0
            }
      , _uiShowREPL = True
      , _uiShowDebug = False
      , _uiHideRobotsUntil = startTime - 1
      , _scenarioRef = Nothing
      }

-- | Load a 'Scenario' and start playing the game.
startGame ::
  (MonadIO m, MonadState AppState m) =>
  ScenarioInfoPair ScenarioPath ->
  Maybe CodeToRun ->
  m ()
startGame (s, ScenarioPath p) c = do
  ss <- use $ runtimeState . scenarios
  let si = loadScenarioInfoFromPath ss p
  startGameWithSeed (s, si) . LaunchParams (pure Nothing) $ pure c

-- | Re-initialize the game from the stored reference to the current scenario.
--
-- Note that "restarting" is intended only for "scenarios";
-- with some scenarios, it may be possible to get stuck so that it is
-- either impossible or very annoying to win, so being offered an
-- option to restart is more user-friendly.
--
-- Since scenarios are stored as a Maybe in the UI state, we handle the Nothing
-- case upstream so that the Scenario passed to this function definitely exists.
restartGame ::
  (MonadIO m, MonadState AppState m) =>
  Seed ->
  ScenarioInfoPair ScenarioPath ->
  m ()
restartGame currentSeed (s, ScenarioPath p) = do
  ss <- use $ runtimeState . scenarios
  let si = loadScenarioInfoFromPath ss p
  startGameWithSeed (s, si) $ LaunchParams (pure (Just currentSeed)) (pure Nothing)

-- | Load a 'Scenario' and start playing the game, with the
--   possibility for the user to override the seed.
startGameWithSeed ::
  (MonadIO m, MonadState AppState m) =>
  ScenarioInfoPair ScenarioInfo ->
  ValidatedLaunchParams ->
  m ()
startGameWithSeed siPair@(_scene, si) lp = do
  t <- liftIO getZonedTime
  ss <- use $ runtimeState . scenarios
  p <- liftIO $ normalizeScenarioPath ss $ si ^. scenarioPath

  runtimeState
    . scenarios
    . scenarioItemByPath p
    . _SISingle
    . _2
    . scenarioStatus
    .= Played
      (toSerializableParams lp)
      (Metric Attempted $ ProgressStats t emptyAttemptMetric)
      (prevBest t)

  scenarioToAppState (fmap (ScenarioPath . view scenarioPath) siPair) lp
  -- Beware: currentScenarioPath must be set so that progress/achievements can be saved.
  -- It has just been cleared in scenarioToAppState.
  playState . gameState . currentScenarioPath .= Just p

  -- Warn the user that the use of debugging options means progress
  -- will not be saved.
  debugging <- use $ uiState . uiDebugOptions
  unless (null debugging) $
    uiState . uiPopups %= addPopup DebugWarningPopup
 where
  prevBest t = case si ^. scenarioStatus of
    NotStarted -> emptyBest t
    Played _ _ b -> b

-- | Modify the 'AppState' appropriately when starting a new scenario.
scenarioToAppState ::
  (MonadIO m, MonadState AppState m) =>
  ScenarioInfoPair ScenarioPath ->
  ValidatedLaunchParams ->
  m ()
scenarioToAppState siPair@(scene, _) lp = do
  rs <- use runtimeState
  gs <- liftIO $ scenarioToGameState scene lp $ rs ^. stdGameConfigInputs
  playState . gameState .= gs

  curTime <- liftIO $ getTime Monotonic
  playState . uiGameplay %= setUIGameplay gs curTime isAutoplaying siPair

  void $ withLensIO uiState $ scenarioToUIState siPair
 where
  isAutoplaying = case fmap (view toRunSource) . runIdentity $ initialCode lp of
    Just ScenarioSuggested -> True
    _ -> False

  withLensIO :: (MonadIO m, MonadState AppState m) => Lens' AppState x -> (x -> IO x) -> m x
  withLensIO l a = do
    x <- use l
    x' <- liftIO $ a x
    l .= x'
    return x'

setUIGameplay ::
  GameState ->
  TimeSpec ->
  Bool ->
  ScenarioInfoPair ScenarioPath ->
  UIGameplay ->
  UIGameplay
setUIGameplay gs curTime isAutoplaying siPair@(scenario, _) uig =
  uig
    & uiDialogs . uiGoal .~ emptyGoalDisplay
    & uiIsAutoPlay .~ isAutoplaying
    & uiFocusRing .~ initFocusRing
    & uiInventory . uiInventorySearch .~ Nothing
    & uiInventory . uiInventoryList .~ Nothing
    & uiInventory . uiInventorySort .~ defaultSortOptions
    & uiInventory . uiShowZero .~ True
    & uiTiming . uiShowFPS .~ False
    & uiREPL .~ initREPLState (uig ^. uiREPL . replHistory)
    & uiREPL . replHistory %~ restartREPLHistory
    & scenarioRef ?~ siPair
    & uiTiming . lastFrameTime .~ curTime
    & uiWorldEditor . EM.entityPaintList %~ BL.listReplace entityList Nothing
    & uiWorldEditor . EM.editingBounds . EM.boundsRect %~ setNewBounds
    & uiDialogs . uiStructure
      .~ StructureDisplay
        (SR.makeListWidget . M.elems $ gs ^. landscape . recognizerAutomatons . originalStructureDefinitions)
        (focusSetCurrent (StructureWidgets StructuresList) $ focusRing $ map StructureWidgets enumerate)
 where
  entityList = EU.getEntitiesForList $ gs ^. landscape . terrainAndEntities . entityMap

  (isEmptyArea, newBounds) =
    EU.getEditingBounds $
      NE.head $
        scenario ^. scenarioLandscape . scenarioWorlds

  setNewBounds maybeOldBounds =
    if isEmptyArea
      then maybeOldBounds
      else Just newBounds

-- | Modify the UI state appropriately when starting a new scenario.
scenarioToUIState ::
  ScenarioInfoPair a ->
  UIState ->
  IO UIState
scenarioToUIState siPair u = do
  return $
    u
      & uiPlaying .~ True
      & uiAttrMap
        .~ applyAttrMappings
          ( map (first getWorldAttrName . toAttrPair) $
              fst siPair ^. scenarioLandscape . scenarioAttrs
          )
          swarmAttrMap

-- | Create an initial app state for a specific scenario.  Note that
--   this function is used only for unit tests, integration tests, and
--   benchmarks.
--
--   In normal play, an 'AppState' already exists and we simply need
--   to update it using 'scenarioToAppState'.
initAppStateForScenario :: String -> Maybe Seed -> Maybe FilePath -> ExceptT Text IO AppState
initAppStateForScenario sceneName userSeed toRun =
  asExceptT . withThrow (prettyText @SystemFailure) . initAppState $
    defaultAppOpts
      { userScenario = Just sceneName
      , userSeed = userSeed
      , scriptToRun = toRun
      }

-- | For convenience, the 'AppState' corresponding to the classic game
--   with seed 0.  This is used only for benchmarks and unit tests.
classicGame0 :: ExceptT Text IO AppState
classicGame0 = initAppStateForScenario "classic" (Just 0) Nothing
