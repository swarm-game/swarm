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
  worlds,
  scenarios,
  stdEntityMap,
  stdRecipes,
  appData,
  nameParts,

  -- ** Utility
  initRuntimeState,
  mkGameStateConfig,
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
import Swarm.Game.Entity (Entity, EntityMap, loadEntities)
import Swarm.Game.Failure (SystemFailure)
import Swarm.Game.Recipe (Recipe, loadRecipes)
import Swarm.Game.ResourceLoading (NameGenerator, initNameGenerator, readAppData)
import Swarm.Game.ScenarioInfo (ScenarioCollection, loadScenarios)
import Swarm.Game.State.Substate
import Swarm.Game.World.Load (loadWorlds)
import Swarm.Game.World.Typecheck (WorldMap)
import Swarm.Log
import Swarm.Util.Lens (makeLensesNoSigs)
import Swarm.Version (NewReleaseFailure (..))

data RuntimeState = RuntimeState
  { _webPort :: Maybe Port
  , _upstreamRelease :: Either NewReleaseFailure String
  , _eventLog :: Notifications LogEntry
  , _worlds :: WorldMap
  , _scenarios :: ScenarioCollection
  , _stdEntityMap :: EntityMap
  , _stdRecipes :: [Recipe Entity]
  , _appData :: Map Text Text
  , _nameParts :: NameGenerator
  }

initRuntimeState ::
  ( Has (Throw SystemFailure) sig m
  , Has (Accum (Seq SystemFailure)) sig m
  , Has (Lift IO) sig m
  ) =>
  m RuntimeState
initRuntimeState = do
  entities <- loadEntities
  recipes <- loadRecipes entities
  worlds <- loadWorlds entities
  scenarios <- loadScenarios entities worlds
  appDataMap <- readAppData
  nameGen <- initNameGenerator appDataMap
  return $
    RuntimeState
      { _webPort = Nothing
      , _upstreamRelease = Left (NoMainUpstreamRelease [])
      , _eventLog = mempty
      , _worlds = worlds
      , _scenarios = scenarios
      , _stdEntityMap = entities
      , _stdRecipes = recipes
      , _appData = appDataMap
      , _nameParts = nameGen
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

-- | A collection of typechecked world DSL terms that are available to
--   be used in scenario definitions.
worlds :: Lens' RuntimeState WorldMap

-- | The collection of scenarios that comes with the game.
scenarios :: Lens' RuntimeState ScenarioCollection

-- | The standard entity map loaded from disk.  Individual scenarios
--   may define additional entities which will get added to this map
--   when loading the scenario.
stdEntityMap :: Lens' RuntimeState EntityMap

-- | The standard list of recipes loaded from disk.  Individual scenarios
--   may define additional recipes which will get added to this list
--   when loading the scenario.
stdRecipes :: Lens' RuntimeState [Recipe Entity]

-- | Free-form data loaded from the @data@ directory, for things like
--   the logo, about page, tutorial story, etc.
appData :: Lens' RuntimeState (Map Text Text)

-- | Lists of words/adjectives for use in building random robot names.
nameParts :: Lens' RuntimeState NameGenerator

-- | Create a 'GameStateConfig' record from the 'RuntimeState'.
mkGameStateConfig :: RuntimeState -> GameStateConfig
mkGameStateConfig rs =
  GameStateConfig
    { initNameParts = rs ^. nameParts
    , initEntities = rs ^. stdEntityMap
    , initRecipes = rs ^. stdRecipes
    , initWorldMap = rs ^. worlds
    }
