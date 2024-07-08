{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Implementation of the 'Swarm.Language.Syntax.Volume' command for robots.
--
-- Note: If the robot is currently on an unwalkable cell (which may happen in
-- the case of teleportation or if an entity is placed or pushed into its cell),
-- the volume shall be zero.
module Swarm.Game.Step.Flood (
  floodFill,
) where

import Control.Effect.Lens
import Control.Lens (makeLenses, (%~), (&))
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

data Tracking = Tracking
  { visited :: HashSet Location
  , floodPartition :: FloodPartition
  }

-- | We annotate each visited cell as
-- being part of the boundary or the interior.
-- This lets us:
--
-- 1. Use the interior cell count as a termination condition
-- 2. Handle (eventual) cache invalidation differently for boundary
--    members than interior members.
data FloodPartition = FloodPartition
  { _boundary :: HashSet Location
  , _interior :: HashSet Location
  }

makeLenses ''FloodPartition

-- |
-- == Algorithm
--
-- Explore via DFS using a list as a stack.
-- Each iteration examines a single cell.
--
-- 1. Mark the popped cell as visited, regardless of walkability.
-- 2. Check popped cell for walkability
-- 3. Add all neighbors that aren't already visited, regardless of walkability, to the stack.
--    But unwalkable cells shall not produce neighbors and shall be marked with a boundary/interior distinction.
floodRecursive ::
  HasRobotStepState sig m =>
  Tracking ->
  [Location] ->
  FloodParms ->
  m (Maybe Int)
floodRecursive tracking pending params =
  case pending of
    nextLoc : otherLocs ->
      if interiorCount > maxVisits params
        then return Nothing
        else checkNeighbors nextLoc otherLocs
    [] -> return $ Just interiorCount
 where
  interiorCount = HashSet.size $ _interior $ floodPartition tracking
  checkNeighbors nextLoc otherLocs = do
    isWalkable <- null <$> checkMoveFailureUnprivileged cosmicLoc
    let candidateNeighbors =
          if isWalkable
            then map (view planar) $ getNeighborLocs cosmicLoc
            else []
        visitableNeighbors = filter (not . (`HashSet.member` visited tracking)) candidateNeighbors

        -- It's cheaper to prepend the "visitableNeighbors" list because
        -- it should in general be a shorter list than the "pending" list.
        newPending = visitableNeighbors <> otherLocs

        partitionMutator =
          if isWalkable
            then interior
            else boundary
        newPartition = floodPartition tracking & partitionMutator %~ HashSet.insert nextLoc

        newTracking =
          tracking
            { visited = newVisited
            , floodPartition = newPartition
            }
    floodRecursive newTracking newPending params
   where
    newVisited = HashSet.insert nextLoc $ visited tracking
    cosmicLoc = Cosmic (theSubworld params) nextLoc

floodFill ::
  HasRobotStepState sig m =>
  Cosmic Location ->
  Int ->
  m (Maybe Int)
floodFill (Cosmic swn curLoc) =
  floodRecursive emptyTracking [curLoc] . FloodParms swn
 where
  emptyTracking = Tracking mempty $ FloodPartition mempty mempty
