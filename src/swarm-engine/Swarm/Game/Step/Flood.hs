-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Implementation of the 'Swarm.Language.Syntax.Volume' command for robots.
module Swarm.Game.Step.Flood (
  floodFill,
) where

import Control.Effect.Lens
import Control.Monad (filterM)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Swarm.Game.Location
import Swarm.Game.Step.RobotStepState
import Swarm.Game.Step.Util (checkMoveFailureUnprivileged)
import Swarm.Game.Step.Util.Inspect (getNeighborLocs)
import Swarm.Game.Universe

data FloodParms = FloodParms
  { theSubworld :: SubworldName
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
    nextLoc : otherLocs ->
      if visitedCount > maxVisits st
        then return Nothing
        else checkNeighbors nextLoc otherLocs
    [] -> return $ Just visitedCount
 where
  visitedCount = HashSet.size visited
  checkNeighbors nextLoc otherLocs = do
    candidateNeighbors <- listNeighbors $ Cosmic (theSubworld st) nextLoc
    let visitableNeighbors = filter (not . (`HashSet.member` visited)) candidateNeighbors
        -- It's cheaper to prepend the "visitableNeighbors" list because
        -- it should in general be a shorter list than the pending queue.
        newPending = visitableNeighbors <> otherLocs
    floodRecursive newVisited newPending st
   where
    newVisited = HashSet.insert nextLoc visited

listNeighbors ::
  HasRobotStepState sig m =>
  Cosmic Location ->
  m [Location]
listNeighbors loc = do
  locs <- filterM isWalkableLoc $ getNeighborLocs loc
  return $ map (view planar) locs
 where
  isWalkableLoc = fmap null . checkMoveFailureUnprivileged

floodFill ::
  HasRobotStepState sig m =>
  Cosmic Location ->
  Int ->
  m (Maybe Int)
floodFill (Cosmic swn curLoc) =
  floodRecursive mempty [curLoc] . FloodParms swn
