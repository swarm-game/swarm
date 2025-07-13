{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Runtime state and utility functions
module Swarm.Game.State.Runtime (
  RuntimeState,
  RuntimeOptions (..),
  initRuntimeState,

  -- ** Lenses
  webPort,
  metricsPort,
  upstreamRelease,
  eventLog,
  appData,
  stdGameConfigInputs,
  metrics,
  logger,

  -- ** Utility
  initScenarioInputs,
  initGameStateConfig,
  waitForLogger,
)
where

import Control.Effect.Accum
import Control.Effect.Lift
import Control.Effect.Throw
import Control.Lens
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Swarm.Failure (SystemFailure)
import Swarm.Game.Land
import Swarm.Game.Recipe (loadRecipes)
import Swarm.Game.Scenario (GameStateInputs (..), ScenarioInputs (..))
import Swarm.Game.State.Substate
import Swarm.Game.World.Load (loadWorlds)
import Swarm.Log
import Swarm.ResourceLoading (initNameGenerator, readAppData, getSwarmLogsPath)
import Swarm.Util.Lens (makeLensesNoSigs)
import System.Metrics qualified as Metrics
import Log
import System.IO (openFile, IOMode(..), BufferMode(..), hSetBuffering)
import System.FilePath

data RuntimeState = RuntimeState
  { _webPort :: Maybe Int
  , _metricsPort :: Maybe Int
  , _upstreamRelease :: Either (Severity, Text) String
  , _eventLog :: Notifications LogEntry
  , _stdGameConfigInputs :: GameStateConfig
  , _appData :: Map Text Text
  , _metrics :: Metrics.Store
  , _logger :: Logger
  }

initScenarioInputs ::
  ( Has (Throw SystemFailure) sig m
  , Has (Accum (Seq SystemFailure)) sig m
  , Has (Lift IO) sig m
  ) =>
  m ScenarioInputs
initScenarioInputs = do
  tem <- loadEntitiesAndTerrain
  worlds <- loadWorlds tem
  return $ ScenarioInputs worlds tem

initGameStateInputs ::
  ( Has (Throw SystemFailure) sig m
  , Has (Accum (Seq SystemFailure)) sig m
  , Has (Lift IO) sig m
  ) =>
  m GameStateInputs
initGameStateInputs = do
  scenarioInputs <- initScenarioInputs
  recipes <- loadRecipes $ initEntityTerrain scenarioInputs ^. entityMap
  return $ GameStateInputs scenarioInputs recipes

initGameStateConfig ::
  ( Has (Throw SystemFailure) sig m
  , Has (Accum (Seq SystemFailure)) sig m
  , Has (Lift IO) sig m
  ) =>
  RuntimeOptions ->
  m GameStateConfig
initGameStateConfig RuntimeOptions {..} = do
  initAppDataMap <- readAppData
  nameParts <- initNameGenerator initAppDataMap
  initState <- initGameStateInputs
  return $ GameStateConfig {..}

-- | Runtime state initialization options.
data RuntimeOptions = RuntimeOptions
  { startPaused :: Bool
  , pauseOnObjectiveCompletion :: Bool
  , loadTestScenarios :: Bool
  , startLogging :: Bool
  }
  deriving (Eq, Show)

initRuntimeState ::
  ( Has (Throw SystemFailure) sig m
  , Has (Accum (Seq SystemFailure)) sig m
  , Has (Lift IO) sig m
  ) =>
  RuntimeOptions ->
  m RuntimeState
initRuntimeState opts = do
  store <- sendIO Metrics.newStore
  sendIO $ Metrics.registerGcMetrics store
  gsc <- initGameStateConfig opts
  fileLogger <- if startLogging opts then sendIO makeFileLogger else pure mempty
  return $
    RuntimeState
      { _webPort = Nothing
      , _metricsPort = Nothing
      , _upstreamRelease = Left (Info, "No upstream release found.")
      , _eventLog = mempty
      , _appData = initAppDataMap gsc
      , _stdGameConfigInputs = gsc
      , _metrics = store
      , _logger = fileLogger
      }

makeFileLogger :: IO Logger
makeFileLogger = do
  logPath <- getSwarmLogsPath
  logFile <- openFile (logPath </> "log.txt") WriteMode
  hSetBuffering logFile LineBuffering
  mkLogger "file log" (T.hPutStrLn logFile . formatMsg)
 where
  formatMsg :: LogMessage -> Text
  formatMsg = showLogMessage Nothing

makeLensesNoSigs ''RuntimeState

-- | The port on which the HTTP debug service is running.
webPort :: Lens' RuntimeState (Maybe Int)

-- | The port on which the HTTP debug service is running.
metricsPort :: Lens' RuntimeState (Maybe Int)

-- | The upstream release version.
upstreamRelease :: Lens' RuntimeState (Either (Severity, Text) String)

-- | A log of runtime events.
--
-- This logging is separate from the logging done during game-play.
-- If some error happens before a game is even selected, this is the
-- place to log it.
eventLog :: Lens' RuntimeState (Notifications LogEntry)

-- | Built-in resources for loading games
stdGameConfigInputs :: Lens' RuntimeState GameStateConfig

-- | Free-form data loaded from the @data@ directory, for things like
--   the logo, about page, tutorial story, etc.
appData :: Lens' RuntimeState (Map Text Text)

-- | The EKG store of metrics for Swarm. Individual components can
-- register counters, gauges and distributions to this store. Then they
-- will be published together with GHC metrics by the Wai server taking
-- a reference to this store.
metrics :: Lens' RuntimeState Metrics.Store

-- | Get the persisted logger - you can think of this is as the IO action,
-- while LoggerEnv should be passed around to enrich logging in current context.
-- For example game engine logs can be enriched with the "game engine" component.
-- While game state can be rebuilt each time time a scenario is started
-- and LoggerEnv discarded, we want to keep around this logging IO action.
logger :: Lens' RuntimeState Logger
