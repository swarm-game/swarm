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
  getScenarioInfoFromPath,
  scenarioToAppState,
  animMgrTickDuration,
  PersistentState (..),
) where

import Brick.Animation (startAnimationManager)
import Brick.AttrMap (applyAttrMappings)
import Brick.BChan (BChan, newBChan)
import Brick.Focus
import Brick.Widgets.List qualified as BL
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Carrier.Accum.Strict (runAccum)
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
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getZonedTime)
import Swarm.Failure (SystemFailure (..))
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Persistence
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
  loadScenarios,
  normalizeScenarioPath,
  pathifyCollection,
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

-- | The resolution at which the animation manager checks animations for updates, in miliseconds
animMgrTickDuration :: Int
animMgrTickDuration = 33

-- | Initialize the 'AppState' from scratch.
initAppState ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  AppOpts ->
  Maybe (BChan AppEvent) ->
  m AppState
initAppState opts mChan = do
  persistentState <- initPersistentState opts
  constructAppState persistentState opts mChan

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

mkRuntimeOptions :: AppOpts -> RuntimeOptions
mkRuntimeOptions AppOpts {..} =
  RuntimeOptions
    { startPaused = pausedAtStart
    , pauseOnObjectiveCompletion = autoShowObjectives
    , loadTestScenarios = Set.member LoadTestingScenarios debugOptions
    }

data PersistentState
  = PersistentState
      RuntimeState
      UIState
      KeyEventHandlingState
      ProgressionState

-- | Initialize the more persistent parts of the app state, /i.e./ the
--   'RuntimeState' and 'UIState'.  This is split out into a separate
--   function so that in the integration test suite we can call this
--   once and reuse the resulting states for all tests.
initPersistentState ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  AppOpts ->
  m PersistentState
initPersistentState opts@(AppOpts {..}) = do
  (warnings :: Seq SystemFailure, PersistentState initRS initUI initKs initProg) <- runAccum mempty $ do
    rs <- initRuntimeState $ mkRuntimeOptions opts
    let showMainMenu = not (skipMenu opts)
    ui <- initUIState UIInitOptions {..}
    ks <- initKeyHandlingState

    s <-
      loadScenarios
        (gsiScenarioInputs $ initState $ rs ^. stdGameConfigInputs)
        (loadTestScenarios $ mkRuntimeOptions opts)
    achievements <- loadAchievementsInfo

    let animState = AnimInactive
    let progState =
          ProgressionState
            { _scenarios = s
            , _attainedAchievements = M.fromList $ map (view achievement &&& id) achievements
            , _uiPopups = initPopupState
            , _scenarioSequence = mempty
            , _uiPopupAnimationState = animState
            }
    return $ PersistentState rs ui ks progState
  let initRS' = addWarnings initRS (F.toList warnings)
  return $ PersistentState initRS' initUI initKs initProg

getScenarioInfoFromPath ::
  ScenarioCollection ScenarioInfo ->
  FilePath ->
  ScenarioInfo
getScenarioInfoFromPath ss path =
  fromMaybe (ScenarioInfo path NotStarted) currentScenarioInfo
 where
  currentScenarioInfo = ss ^? scenarioItemByPath path . _SISingle . getScenarioInfo

-- | Construct an 'AppState' from an already-loaded 'RuntimeState' and
--   'UIState', given the 'AppOpts' the app was started with.
constructAppState ::
  ( Has (Throw SystemFailure) sig m
  , Has (Lift IO) sig m
  ) =>
  PersistentState ->
  AppOpts ->
  Maybe (BChan AppEvent) ->
  m AppState
constructAppState (PersistentState rs ui key progState) opts@(AppOpts {..}) mChan = do
  historyT <- sendIO $ readFileMayT =<< getSwarmHistoryPath False
  let history = maybe [] (map mkREPLSubmission . T.lines) historyT
  startTime <- sendIO $ getTime Monotonic
  chan <- sendIO $ maybe initTestChan pure mChan
  animMgr <- sendIO $ startAnimationManager animMgrTickDuration chan PopupEvent

  let gsc = rs ^. stdGameConfigInputs
      gs = initGameState gsc
      ps =
        PlayState
          { _scenarioState = ScenarioState gs $ initialUiGameplay startTime history
          , _progression = progState
          }

  case skipMenu opts of
    False -> return $ AppState (ps & scenarioState . uiGameplay . uiTiming . lgTicksPerSecond .~ defaultInitLgTicksPerSecond) ui key rs animMgr
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

      let si = getScenarioInfoFromPath (progState ^. scenarios) path

      sendIO $
        execStateT
          (startGameWithSeed (ScenarioWith scenario si) $ LaunchParams (pure userSeed) (pure codeToRun))
          (AppState ps ui key rs animMgr)
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
            , _uiRobot = emptyRobotDisplay
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
  NonEmpty (ScenarioWith ScenarioPath) ->
  Maybe CodeToRun ->
  m ()
startGame (ScenarioWith s (ScenarioPath p) :| remaining) c = do
  playState . progression . scenarioSequence .= remaining
  ss <- use $ playState . progression . scenarios
  let si = getScenarioInfoFromPath ss p
  startGameWithSeed (ScenarioWith s si) . LaunchParams (pure Nothing) $ pure c

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
  ScenarioWith ScenarioPath ->
  m ()
restartGame currentSeed (ScenarioWith s (ScenarioPath p)) = do
  ss <- use $ playState . progression . scenarios
  let si = getScenarioInfoFromPath ss p
  startGameWithSeed (ScenarioWith s si) $ LaunchParams (pure (Just currentSeed)) (pure Nothing)

-- | Load a 'Scenario' and start playing the game, with the
--   possibility for the user to override the seed.
startGameWithSeed ::
  (MonadIO m, MonadState AppState m) =>
  ScenarioWith ScenarioInfo ->
  ValidatedLaunchParams ->
  m ()
startGameWithSeed siPair@(ScenarioWith _scene si) lp = do
  t <- liftIO getZonedTime
  ss <- use $ playState . progression . scenarios
  p <- liftIO $ normalizeScenarioPath ss $ si ^. scenarioPath

  playState
    . progression
    . scenarios
    . scenarioItemByPath p
    . _SISingle
    . getScenarioInfo
    . scenarioStatus
    .= Played
      (toSerializableParams lp)
      (Metric Attempted $ ProgressStats t emptyAttemptMetric)
      (prevBest t)

  scenarioToAppState (pathifyCollection siPair) lp

  -- Warn the user that the use of debugging options means progress
  -- will not be saved.
  debugging <- use $ uiState . uiDebugOptions
  unless (null debugging) $
    playState . progression . uiPopups %= addPopup DebugWarningPopup
 where
  prevBest t = case si ^. scenarioStatus of
    NotStarted -> emptyBest t
    Played _ _ b -> b

-- | Modify the 'AppState' appropriately when starting a new scenario.
scenarioToAppState ::
  (MonadIO m, MonadState AppState m) =>
  ScenarioWith ScenarioPath ->
  ValidatedLaunchParams ->
  m ()
scenarioToAppState siPair@(ScenarioWith scene p) lp = do
  rs <- use runtimeState
  gs <- liftIO $ scenarioToGameState (ScenarioWith scene $ Just p) lp $ rs ^. stdGameConfigInputs
  playState . scenarioState . gameState .= gs

  curTime <- liftIO $ getTime Monotonic
  playState . scenarioState . uiGameplay %= setUIGameplay gs curTime isAutoplaying siPair

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
  ScenarioWith ScenarioPath ->
  UIGameplay ->
  UIGameplay
setUIGameplay gs curTime isAutoplaying siPair@(ScenarioWith scenario _) uig =
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
  ScenarioWith a ->
  UIState ->
  IO UIState
scenarioToUIState siPair u = do
  return $
    u
      & uiPlaying .~ True
      & uiAttrMap
        .~ applyAttrMappings
          ( map (first getWorldAttrName . toAttrPair) $
              siPair ^. getScenario . scenarioLandscape . scenarioAttrs
          )
          swarmAttrMap

-- | Create a BChan that holds only one event.
--   This should only be used in unit tests, integration tests, and benchmarks
initTestChan :: IO (BChan AppEvent)
initTestChan = newBChan 1

-- | Create an initial app state for a specific scenario.  Note that
--   this function is used only for unit tests, integration tests, and
--   benchmarks.
--
--   In normal play, an 'AppState' already exists and we simply need
--   to update it using 'scenarioToAppState'.
initAppStateForScenario :: String -> Maybe Seed -> Maybe FilePath -> ExceptT Text IO AppState
initAppStateForScenario sceneName userSeed toRun =
  asExceptT . withThrow (prettyText @SystemFailure) . flip initAppState Nothing $
    defaultAppOpts
      { userScenario = Just sceneName
      , userSeed = userSeed
      , scriptToRun = toRun
      }

-- | For convenience, the 'AppState' corresponding to the classic game
--   with seed 0.  This is used only for benchmarks and unit tests.
classicGame0 :: ExceptT Text IO AppState
classicGame0 = initAppStateForScenario "classic" (Just 0) Nothing
