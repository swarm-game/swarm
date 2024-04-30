-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utility for determining the center of
-- the map, outside the context of a
-- running game
module Swarm.Game.Scenario.Topography.Center where

import Control.Lens (view)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe, listToMaybe)
import Swarm.Game.Location (Location, origin)
import Swarm.Game.Robot (trobotLocation)
import Swarm.Game.Scenario (ScenarioLandscape)
import Swarm.Game.State.Landscape (SubworldDescription, genRobotTemplates)
import Swarm.Game.Universe (Cosmic (..), SubworldName (DefaultRootSubworld))

-- | Determine view center for a static map
-- without reference to a 'GameState'
-- (i.e. outside the context of an active game)
determineStaticViewCenter ::
  ScenarioLandscape ->
  NonEmpty SubworldDescription ->
  Cosmic Location
determineStaticViewCenter sLandscape worldTuples =
  fromMaybe defaultVC baseRobotLoc
 where
  theRobots = genRobotTemplates sLandscape worldTuples
  defaultVC = Cosmic DefaultRootSubworld origin

  -- The first robot is guaranteed to be the base.
  baseRobotLoc :: Maybe (Cosmic Location)
  baseRobotLoc = do
    theBaseRobot <- listToMaybe theRobots
    view trobotLocation theBaseRobot
