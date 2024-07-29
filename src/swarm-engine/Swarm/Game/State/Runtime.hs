{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Runtime state and utility functions
module Swarm.Game.State.Runtime (
  RuntimeState,

  -- ** Lenses
  webPort,
  upstreamRelease,
  eventLog,
  scenarios,
  appData,
  stdGameConfigInputs,

  -- ** Utility
  initScenarioInputs,
  initRuntimeState,
  initGameStateConfig,
)
where

import Control.Effect.Accum
import Control.Effect.Lift
import Control.Effect.Throw
import Control.Lens
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Network.Wai.Handler.Warp (Port)
import Swarm.Game.Failure (SystemFailure)
import Swarm.Game.Land
import Swarm.Game.Recipe (loadRecipes)
import Swarm.Game.ResourceLoading (initNameGenerator, readAppData)
import Swarm.Game.Scenario (GameStateInputs (..), ScenarioInputs (..))
import Swarm.Game.ScenarioInfo (ScenarioCollection, loadScenarios)
import Swarm.Game.State.Substate
import Swarm.Game.World.Load (loadWorlds)
import Swarm.Log
import Swarm.Util.Lens (makeLensesNoSigs)
import Swarm.Version (NewReleaseFailure (..))

data RuntimeState = RuntimeState
  { _webPort :: Maybe Port
  , _upstreamRelease :: Either NewReleaseFailure String
  , _eventLog :: Notifications LogEntry
  , _scenarios :: ScenarioCollection
  , _stdGameConfigInputs :: GameStateConfig
  , _appData :: Map Text Text
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
  Bool ->
  m GameStateConfig
initGameStateConfig pause = do
  gsi <- initGameStateInputs
  appDataMap <- readAppData
  nameGen <- initNameGenerator appDataMap
  return $ GameStateConfig appDataMap nameGen pause gsi

initRuntimeState ::
  ( Has (Throw SystemFailure) sig m
  , Has (Accum (Seq SystemFailure)) sig m
  , Has (Lift IO) sig m
  ) =>
  Bool ->
  m RuntimeState
initRuntimeState pause = do
  gsc <- initGameStateConfig pause
  scenarios <- loadScenarios $ gsiScenarioInputs $ initState gsc

  return $
    RuntimeState
      { _webPort = Nothing
      , _upstreamRelease = Left (NoMainUpstreamRelease [])
      , _eventLog = mempty
      , _scenarios = scenarios
      , _appData = initAppDataMap gsc
      , _stdGameConfigInputs = gsc
      }

makeLensesNoSigs ''RuntimeState

-- | The port on which the HTTP debug service is running.
webPort :: Lens' RuntimeState (Maybe Port)

-- | The upstream release version.
upstreamRelease :: Lens' RuntimeState (Either NewReleaseFailure String)

-- | A log of runtime events.
--
-- This logging is separate from the logging done during game-play.
-- If some error happens before a game is even selected, this is the
-- place to log it.
eventLog :: Lens' RuntimeState (Notifications LogEntry)

-- | The collection of scenarios that comes with the game.
scenarios :: Lens' RuntimeState ScenarioCollection

-- | Built-in resources for loading games
stdGameConfigInputs :: Lens' RuntimeState GameStateConfig

-- | Free-form data loaded from the @data@ directory, for things like
--   the logo, about page, tutorial story, etc.
appData :: Lens' RuntimeState (Map Text Text)
