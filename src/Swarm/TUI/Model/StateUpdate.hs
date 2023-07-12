{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.StateUpdate (
  initAppState,
  initAppStateForScenario,
  classicGame0,
  startGame,
  startGameWithSeed,
  restartGame,
  attainAchievement,
  attainAchievement',
  scenarioToAppState,
) where

import Brick.AttrMap (applyAttrMappings)
import Brick.Widgets.List qualified as BL
import Control.Applicative ((<|>))
import Control.Lens hiding (from, (<.>))
import Control.Monad (guard, void)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState, execStateT)
import Data.List qualified as List
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Time (ZonedTime, getZonedTime)
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Achievement.Persistence
import Swarm.Game.Failure.Render (prettyFailure)
import Swarm.Game.Log (ErrorLevel (..), LogSource (ErrorTrace))
import Swarm.Game.Scenario (loadScenario, scenarioAttrs, scenarioWorld)
import Swarm.Game.Scenario.Scoring.Best
import Swarm.Game.Scenario.Scoring.ConcreteMetrics
import Swarm.Game.Scenario.Scoring.GenericMetrics
import Swarm.Game.Scenario.Status
import Swarm.Game.ScenarioInfo (
  loadScenarioInfo,
  normalizeScenarioPath,
  scenarioItemByPath,
  scenarioSolution,
  _SISingle,
 )
import Swarm.Game.State
import Swarm.TUI.Attr (swarmAttrMap)
import Swarm.TUI.Editor.Model qualified as EM
import Swarm.TUI.Editor.Util qualified as EU
import Swarm.TUI.Inventory.Sorting
import Swarm.TUI.Launch.Model (toSerializableParams)
import Swarm.TUI.Model
import Swarm.TUI.Model.Goal (emptyGoalDisplay)
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.UI
import Swarm.TUI.View.CustomStyling (toAttrPair)
import System.Clock

-- | Initialize the 'AppState'.
initAppState :: AppOpts -> ExceptT Text IO AppState
initAppState AppOpts {..} = do
  let isRunningInitialProgram = isJust scriptToRun || autoPlay
      skipMenu = isJust userScenario || isRunningInitialProgram || isJust userSeed
  (rsWarnings, initRS) <- initRuntimeState
  let gs = initGameState (mkGameStateConfig initRS)
  (uiWarnings, ui) <- initUIState speed (not skipMenu) (cheatMode || autoPlay)
  let logWarning rs' w = rs' & eventLog %~ logEvent (ErrorTrace Error) ("UI Loading", -8) (prettyFailure w)
      addWarnings = List.foldl' logWarning
      rs = addWarnings initRS $ rsWarnings <> uiWarnings
  case skipMenu of
    False -> return $ AppState gs (ui & lgTicksPerSecond .~ defaultInitLgTicksPerSecond) rs
    True -> do
      (scenario, path) <- loadScenario (fromMaybe "classic" userScenario) (gs ^. entityMap)
      maybeRunScript <- getParsedInitialCode scriptToRun

      let maybeAutoplay = do
            guard autoPlay
            soln <- scenario ^. scenarioSolution
            return $ CodeToRun ScenarioSuggested soln
          codeToRun = maybeAutoplay <|> maybeRunScript

      eitherSi <- runExceptT $ loadScenarioInfo path
      let (si, newRs) = case eitherSi of
            Right x -> (x, rs)
            Left e -> (ScenarioInfo path NotStarted, addWarnings rs e)
      execStateT
        (startGameWithSeed (scenario, si) $ LaunchParams (pure userSeed) (pure codeToRun))
        (AppState gs ui newRs)

-- | Load a 'Scenario' and start playing the game.
startGame :: (MonadIO m, MonadState AppState m) => ScenarioInfoPair -> Maybe CodeToRun -> m ()
startGame siPair = startGameWithSeed siPair . LaunchParams (pure Nothing) . pure

-- | Re-initialize the game from the stored reference to the current scenario.
--
-- Note that "restarting" is intended only for "scenarios";
-- with some scenarios, it may be possible to get stuck so that it is
-- either impossible or very annoying to win, so being offered an
-- option to restart is more user-friendly.
--
-- Since scenarios are stored as a Maybe in the UI state, we handle the Nothing
-- case upstream so that the Scenario passed to this function definitely exists.
restartGame :: (MonadIO m, MonadState AppState m) => Seed -> ScenarioInfoPair -> m ()
restartGame currentSeed siPair = startGameWithSeed siPair $ LaunchParams (pure (Just currentSeed)) (pure Nothing)

-- | Load a 'Scenario' and start playing the game, with the
--   possibility for the user to override the seed.
startGameWithSeed ::
  (MonadIO m, MonadState AppState m) =>
  ScenarioInfoPair ->
  ValidatedLaunchParams ->
  m ()
startGameWithSeed siPair@(_scene, si) lp = do
  t <- liftIO getZonedTime
  ss <- use $ runtimeState . scenarios
  p <- liftIO $ normalizeScenarioPath ss (si ^. scenarioPath)
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
  scenarioToAppState siPair lp
  -- Beware: currentScenarioPath must be set so that progress/achievements can be saved.
  -- It has just been cleared in scenarioToAppState.
  gameState . currentScenarioPath .= Just p
 where
  prevBest t = case si ^. scenarioStatus of
    NotStarted -> emptyBest t
    Played _ _ b -> b

-- | Modify the 'AppState' appropriately when starting a new scenario.
scenarioToAppState ::
  (MonadIO m, MonadState AppState m) =>
  ScenarioInfoPair ->
  ValidatedLaunchParams ->
  m ()
scenarioToAppState siPair@(scene, _) lp = do
  rs <- use runtimeState
  gs <- liftIO $ scenarioToGameState scene lp $ mkGameStateConfig rs
  gameState .= gs
  void $ withLensIO uiState $ scenarioToUIState isAutoplaying siPair gs
 where
  isAutoplaying = case runIdentity (initialCode lp) of
    Just (CodeToRun ScenarioSuggested _) -> True
    _ -> False

  withLensIO :: (MonadIO m, MonadState AppState m) => Lens' AppState x -> (x -> IO x) -> m x
  withLensIO l a = do
    x <- use l
    x' <- liftIO $ a x
    l .= x'
    return x'

attainAchievement :: (MonadIO m, MonadState AppState m) => CategorizedAchievement -> m ()
attainAchievement a = do
  currentTime <- liftIO getZonedTime
  attainAchievement' currentTime Nothing a

attainAchievement' ::
  (MonadIO m, MonadState AppState m) =>
  ZonedTime ->
  Maybe FilePath ->
  CategorizedAchievement ->
  m ()
attainAchievement' t p a = do
  (uiState . uiAchievements)
    %= M.insertWith
      (<>)
      a
      (Attainment a p t)
  newAchievements <- use $ uiState . uiAchievements
  liftIO $ saveAchievementsInfo $ M.elems newAchievements

-- | Modify the UI state appropriately when starting a new scenario.
scenarioToUIState ::
  Bool ->
  ScenarioInfoPair ->
  GameState ->
  UIState ->
  IO UIState
scenarioToUIState isAutoplaying siPair@(scenario, _) gs u = do
  curTime <- getTime Monotonic
  return $
    u
      & uiPlaying .~ True
      & uiGoal .~ emptyGoalDisplay
      & uiIsAutoplay .~ isAutoplaying
      & uiFocusRing .~ initFocusRing
      & uiInventory .~ Nothing
      & uiInventorySort .~ defaultSortOptions
      & uiShowFPS .~ False
      & uiShowZero .~ True
      & uiREPL .~ initREPLState (u ^. uiREPL . replHistory)
      & uiREPL . replHistory %~ restartREPLHistory
      & uiAttrMap .~ applyAttrMappings (map toAttrPair $ fst siPair ^. scenarioAttrs) swarmAttrMap
      & scenarioRef ?~ siPair
      & lastFrameTime .~ curTime
      & uiWorldEditor . EM.entityPaintList %~ BL.listReplace entityList Nothing
      & uiWorldEditor . EM.editingBounds . EM.boundsRect %~ setNewBounds
 where
  entityList = EU.getEntitiesForList $ gs ^. entityMap

  myWorld = scenario ^. scenarioWorld
  (isEmptyArea, newBounds) = EU.getEditingBounds myWorld
  setNewBounds maybeOldBounds =
    if isEmptyArea
      then maybeOldBounds
      else Just newBounds

-- | Create an initial app state for a specific scenario.  Note that
--   this function is used only for unit tests, integration tests, and
--   benchmarks.
--
--   In normal play, an 'AppState' already exists and we simply need
--   to update it using 'scenarioToAppState'.
initAppStateForScenario :: String -> Maybe Seed -> Maybe FilePath -> ExceptT Text IO AppState
initAppStateForScenario sceneName userSeed toRun =
  initAppState (defaultAppOpts {userScenario = Just sceneName, userSeed = userSeed, scriptToRun = toRun})

-- | For convenience, the 'AppState' corresponding to the classic game
--   with seed 0.  This is used only for benchmarks and unit tests.
classicGame0 :: ExceptT Text IO AppState
classicGame0 = initAppStateForScenario "classic" (Just 0) Nothing
