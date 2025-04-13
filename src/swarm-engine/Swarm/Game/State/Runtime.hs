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
  ProgressionState,

  -- ** Lenses
  progression,
  webPort,
  upstreamRelease,
  eventLog,
  scenarios,
  appData,
  stdGameConfigInputs,
  attainedAchievements,
  uiPopups,

  -- ** Utility
  initScenarioInputs,
  initGameStateConfig,
)
where

import Control.Arrow ((&&&))
import Control.Effect.Accum
import Control.Effect.Lift
import Control.Effect.Throw
import Control.Lens
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Text (Text)
import Swarm.Failure (SystemFailure)
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Achievement.Persistence
import Swarm.Game.Land
import Swarm.Game.Popup
import Swarm.Game.Recipe (loadRecipes)
import Swarm.Game.Scenario (GameStateInputs (..), ScenarioInputs (..))
import Swarm.Game.ScenarioInfo (ScenarioCollection, ScenarioInfo, loadScenarios)
import Swarm.Game.State.Substate
import Swarm.Game.World.Load (loadWorlds)
import Swarm.Log
import Swarm.ResourceLoading (initNameGenerator, readAppData)
import Swarm.Util.Lens (makeLensesNoSigs)

-- | State that can evolve as the user progresses through scenarios.
-- This includes achievements and completion records.
--
-- Note that these things are also serialized to disk storage, but
-- we also persist in memory since we don't reload data from disk as
-- we progress through scenarios.
data ProgressionState = ProgressionState
  { _scenarios :: ScenarioCollection ScenarioInfo
  , _attainedAchievements :: Map CategorizedAchievement Attainment
  , _uiPopups :: PopupState
  }

makeLensesNoSigs ''ProgressionState

-- | Map of achievements that were attained
attainedAchievements :: Lens' ProgressionState (Map CategorizedAchievement Attainment)

-- | The collection of scenarios that comes with the game.
scenarios :: Lens' ProgressionState (ScenarioCollection ScenarioInfo)

-- | Queue of popups to display
uiPopups :: Lens' ProgressionState PopupState

data RuntimeState = RuntimeState
  { _webPort :: Maybe Int
  , _upstreamRelease :: Either (Severity, Text) String
  , _eventLog :: Notifications LogEntry
  , _stdGameConfigInputs :: GameStateConfig
  , _appData :: Map Text Text
  , _progression :: ProgressionState
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
  gsc <- initGameStateConfig opts
  s <- loadScenarios (gsiScenarioInputs $ initState gsc) (loadTestScenarios opts)

  achievements <- loadAchievementsInfo
  return $
    RuntimeState
      { _webPort = Nothing
      , _upstreamRelease = Left (Info, "No upstream release found.")
      , _eventLog = mempty
      , _appData = initAppDataMap gsc
      , _stdGameConfigInputs = gsc
      , _progression =
          ProgressionState
            { _scenarios = s
            , _attainedAchievements = M.fromList $ map (view achievement &&& id) achievements
            , _uiPopups = initPopupState
            }
      }

makeLensesNoSigs ''RuntimeState

-- | The port on which the HTTP debug service is running.
webPort :: Lens' RuntimeState (Maybe Int)

-- | The upstream release version.
upstreamRelease :: Lens' RuntimeState (Either (Severity, Text) String)

-- | A log of runtime events.
--
-- This logging is separate from the logging done during game-play.
-- If some error happens before a game is even selected, this is the
-- place to log it.
eventLog :: Lens' RuntimeState (Notifications LogEntry)

-- | State that can evolve as the user progresses through scenarios.
progression :: Lens' RuntimeState ProgressionState

-- | Built-in resources for loading games
stdGameConfigInputs :: Lens' RuntimeState GameStateConfig

-- | Free-form data loaded from the @data@ directory, for things like
--   the logo, about page, tutorial story, etc.
appData :: Lens' RuntimeState (Map Text Text)
