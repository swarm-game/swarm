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
import Swarm.Game.Scenario (Scenario)
import Swarm.Game.State (SubworldDescription, genRobotTemplates)
import Swarm.Game.Universe (Cosmic (..), SubworldName (DefaultRootSubworld))

determineViewCenter ::
  Scenario ->
  NonEmpty SubworldDescription ->
  Cosmic Location
determineViewCenter s worldTuples =
  fromMaybe defaultVC baseRobotLoc
 where
  theRobots = genRobotTemplates s worldTuples
  defaultVC = Cosmic DefaultRootSubworld origin

  -- The first robot is guaranteed to be the base.
  baseRobotLoc :: Maybe (Cosmic Location)
  baseRobotLoc = do
    theBaseRobot <- listToMaybe theRobots
    view trobotLocation theBaseRobot
