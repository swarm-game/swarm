{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Swarm.TUI.Model.StateUpdate (
  initAppState,
  startGame,
  restartGame,
  attainAchievement,
  attainAchievement',
  scenarioToAppState,
) where

import Brick.Widgets.List qualified as BL
import Control.Applicative ((<|>))
import Control.Lens hiding (from, (<.>))
import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Time (ZonedTime, getZonedTime)
import Data.Vector qualified as V
import Swarm.Game.Entity as E
import Swarm.Game.Scenario (area, loadScenario, scenarioWorld, ul)
import Swarm.Game.Scenario.EntityFacade
import Swarm.Game.ScenarioInfo (
  ScenarioInfo (..),
  ScenarioInfoPair,
  ScenarioStatus (..),
  normalizeScenarioPath,
  scenarioItemByPath,
  scenarioPath,
  scenarioSolution,
  scenarioStatus,
  _SISingle,
 )
import Swarm.Game.State
import Swarm.Game.World qualified as W
import Swarm.TUI.Editor.EditorModel
import Swarm.TUI.Inventory.Sorting
import Swarm.TUI.Model
import Swarm.TUI.Model.Achievement.Attainment
import Swarm.TUI.Model.Achievement.Definitions
import Swarm.TUI.Model.Achievement.Persistence
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.UI
import Swarm.Util.Location
import System.Clock

-- | Initialize the 'AppState'.
initAppState :: AppOpts -> ExceptT Text IO AppState
initAppState AppOpts {..} = do
  let isRunningInitialProgram = isJust scriptToRun || autoPlay
      skipMenu = isJust userScenario || isRunningInitialProgram || isJust userSeed
  gs <- initGameState
  ui <- initUIState (not skipMenu) cheatMode
  let rs = initRuntimeState
  case skipMenu of
    False -> return $ AppState gs ui rs
    True -> do
      (scenario, path) <- loadScenario (fromMaybe "classic" userScenario) (gs ^. entityMap)

      let maybeAutoplay = do
            guard autoPlay
            soln <- scenario ^. scenarioSolution
            return $ SuggestedSolution soln
      let realToRun = maybeAutoplay <|> (ScriptPath <$> scriptToRun)

      execStateT
        (startGameWithSeed userSeed (scenario, ScenarioInfo path NotStarted NotStarted NotStarted) realToRun)
        (AppState gs ui rs)

-- | Load a 'Scenario' and start playing the game.
startGame :: (MonadIO m, MonadState AppState m) => ScenarioInfoPair -> Maybe CodeToRun -> m ()
startGame = startGameWithSeed Nothing

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
restartGame currentSeed siPair = startGameWithSeed (Just currentSeed) siPair Nothing

-- | Load a 'Scenario' and start playing the game, with the
--   possibility for the user to override the seed.
startGameWithSeed :: (MonadIO m, MonadState AppState m) => Maybe Seed -> ScenarioInfoPair -> Maybe CodeToRun -> m ()
startGameWithSeed userSeed siPair@(_scene, si) toRun = do
  t <- liftIO getZonedTime
  ss <- use $ gameState . scenarios
  p <- liftIO $ normalizeScenarioPath ss (si ^. scenarioPath)
  gameState . currentScenarioPath .= Just p
  gameState . scenarios . scenarioItemByPath p . _SISingle . _2 . scenarioStatus .= InProgress t 0 0
  scenarioToAppState siPair userSeed toRun

-- XXX do we need to keep an old entity map around???

-- | Modify the 'AppState' appropriately when starting a new scenario.
scenarioToAppState :: (MonadIO m, MonadState AppState m) => ScenarioInfoPair -> Maybe Seed -> Maybe CodeToRun -> m ()
scenarioToAppState siPair@(scene, _) userSeed toRun = do
  newGameState <- withLensIO gameState $ scenarioToGameState scene userSeed toRun
  void $ withLensIO uiState $ scenarioToUIState siPair newGameState
 where
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

attainAchievement' :: (MonadIO m, MonadState AppState m) => ZonedTime -> Maybe Text -> CategorizedAchievement -> m ()
attainAchievement' t p a = do
  (uiState . uiAchievements)
    %= M.insertWith
      (<>)
      a
      (Attainment a p t)
  newAchievements <- use $ uiState . uiAchievements
  liftIO $ saveAchievementsInfo $ M.elems newAchievements

-- | Modify the UI state appropriately when starting a new scenario.
scenarioToUIState :: ScenarioInfoPair -> GameState -> UIState -> IO UIState
scenarioToUIState siPair@(scenario, _) gs u = do
  curTime <- getTime Monotonic
  return $
    u
      & uiPlaying .~ True
      & uiGoal .~ Nothing
      & uiFocusRing .~ initFocusRing
      & uiInventory .~ Nothing
      & uiInventorySort .~ defaultSortOptions
      & uiShowFPS .~ False
      & uiShowZero .~ True
      & lgTicksPerSecond .~ initLgTicksPerSecond
      & uiREPL .~ initREPLState (u ^. uiREPL . replHistory)
      & uiREPL . replHistory %~ restartREPLHistory
      & scenarioRef ?~ siPair
      & lastFrameTime .~ curTime
      & uiWorldEditor . entityPaintList %~ BL.listReplace newList Nothing
      & uiWorldEditor . editingBounds . boundsRect %~ setNewBounds
 where
  myWorld = scenario ^. scenarioWorld
  Location left top = ul myWorld

  setNewBounds maybeOldBounds =
    if mapHeight /= 0 && mapWidth /= 0
      then Just newBounds
      else maybeOldBounds

  -- TODO Note inversion of horizontal and vertical coordinates
  newBounds = (W.Coords (-top, left), W.Coords (-top + mapHeight - 1, left + mapWidth - 1))
  AreaDimensions mapWidth mapHeight = getAreaDimensions $ area myWorld

  entities = M.elems $ entitiesByName $ gs ^. entityMap
  newList = V.fromList $ map mkPaint entities
