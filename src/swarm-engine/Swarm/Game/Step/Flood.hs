-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Implementation of the 'Swarm.Language.Syntax.Volume' command for robots.
module Swarm.Game.Step.Flood (
    floodFill
  , globalMaxVolume
  , ) where

import Control.Effect.Lens
import Swarm.Game.Location
import Swarm.Game.Step.RobotStepState
import Swarm.Game.Universe
import Swarm.Game.Step.Util.Inspect (getNeighborLocs)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Control.Monad (filterM)
import Swarm.Game.Step.Util (checkMoveFailureUnprivileged)

globalMaxVolume :: Integer
globalMaxVolume = 64 * 64

data FloodParms = FloodParms {
    theSubworld :: SubworldName
  , maxVisits :: Int
  }

floodRecursive ::
  HasRobotStepState sig m =>
  HashSet Location ->
  [Location] ->
  FloodParms ->
  m (Maybe Int)
floodRecursive visited pending st =
  case pending of
    nextLoc : otherLocs -> if visitedCount > maxVisits st
      then return Nothing
      else do
        let newVisited = HashSet.insert nextLoc visited
        candidateNeighbors <- neighborFunc $ Cosmic (theSubworld st) nextLoc
        let visitableNeighbors = filter (not . (`HashSet.member` visited)) candidateNeighbors
            -- It's cheaper to prepend the "visitableNeighbors" list because
            -- it should in general be a shorter list than the pending queue.
            newPending = visitableNeighbors <> otherLocs
        floodRecursive newVisited newPending st

    [] -> return $ Just visitedCount
  where
    visitedCount = HashSet.size visited

floodFill :: 
  HasRobotStepState sig m =>
  Cosmic Location ->
  Int ->
  m (Maybe Int)
floodFill (Cosmic swn curLoc) =
  floodRecursive mempty [curLoc] . FloodParms swn

neighborFunc ::
  HasRobotStepState sig m =>
  Cosmic Location ->
  m [Location]
neighborFunc loc = do
  locs <- filterM isWalkableLoc neighborLocs
  return $ map (view planar) locs
  where
  neighborLocs = getNeighborLocs loc
  isWalkableLoc someLoc =
    null <$> checkMoveFailureUnprivileged someLoc
