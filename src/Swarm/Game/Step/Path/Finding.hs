-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Implementation of the @path@ command for robots.
--
-- = Design considerations
-- One possible design of the @path@ command entailed storing a computed
-- shortest path and providing a mechanism to retrieve parts of it later
-- without recomputing the whole thing.
-- However, in general the playfield can be dynamic and obstructions may
-- appear that invalidate a given computed shortest path.
-- Therefore, there can be limited value in caching a computed path for use
-- across ticks.
--
-- Instead, in the current implementation a complete path is computed
-- internally upon invoking the @path@ command, and just the direction of the
-- first "move" along that path is returned as a result to the caller.
--
-- == Max distance
--
-- We allow the caller to supply a max distance, but also impose an internal maximum
-- distance to prevent programming errors from irrecoverably freezing the game.
module Swarm.Game.Step.Path.Finding where

import Control.Carrier.State.Lazy
import Control.Effect.Lens
import Control.Lens ((^.))
import Control.Monad (filterM, guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Graph.AStar (aStarM)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.Step.Path.Cache
import Swarm.Game.Step.Path.Cache.DistanceLimit (withinDistance)
import Swarm.Game.Step.Path.Type
import Swarm.Game.Step.RobotStepState
import Swarm.Game.Step.Util
import Swarm.Game.Step.Util.Inspect
import Swarm.Game.Universe
import Swarm.Language.Syntax
import Swarm.Util (hoistMaybe)

-- | Swarm command arguments are converted to idiomatic Haskell
-- types before invoking this function, and conversely the callsite
-- is also responsible for translating the output type to a swarm value.
--
-- The cost function is uniformly @1@ between adjacent cells.
--
-- Viable paths are determined by walkability.
-- If the goal type is an 'Entity', then it is permissible for that
-- entity to be 'Unwalkable'.
--
-- See "Swarm.Game.Step.Path.Cache" for caching details.
pathCommand ::
  (HasRobotStepState sig m, Has (State GameState) sig m) =>
  PathfindingParameters (Cosmic Location) ->
  m (Maybe Direction)
pathCommand parms = do
  currentWalkabilityContext <- use walkabilityContext

  -- First, check if the pathfinding target has a cached path.
  eitherCachedPath <- retrieveCachedPath currentWalkabilityContext parms

  case eitherCachedPath of
    Right cachedPath -> return $ Just $ nextDir cachedPath
    Left _ -> do
      -- This is a short-circuiting optimization; if the goal location itself
      -- is not a walkable cell, then no amount of searching will reach it.
      isGoalLocWalkable <- case target of
        LocationTarget loc -> null <$> checkMoveFailure (Cosmic currentSubworld loc)
        EntityTarget _ -> return True

      runMaybeT $ do
        guard isGoalLocWalkable
        maybeFoundPath <- lift computePath
        foundPath <- hoistMaybe maybeFoundPath
        -- NOTE: This will not cache the fact that a path was not found.
        lift $ recordCache (fmap (^. subworld) parms) currentWalkabilityContext $ robotLoc :| foundPath
        return $ nextDir foundPath
 where
  PathfindingParameters maybeDistanceLimit (Cosmic currentSubworld robotLoc) target = parms

  computePath =
    aStarM
      (neighborFunc withinDistanceLimit . Cosmic currentSubworld)
      (const $ const $ return 1)
      (return . distHeuristic)
      goalReachedFunc
      (return robotLoc)

  withinDistanceLimit :: Location -> Bool
  withinDistanceLimit = withinDistance distLimit robotLoc

  directionTo :: Location -> Direction
  directionTo nextLoc = DAbsolute $ nearestDirection $ nextLoc .-. robotLoc

  -- Extracts the head of the found path to determine
  -- the next direction for the robot to proceed along
  nextDir :: [Location] -> Direction
  nextDir pathLocs = case pathLocs of
    [] -> DRelative DDown
    (nextLoc : _) -> directionTo nextLoc

  neighborFunc ::
    HasRobotStepState sig m =>
    (Location -> Bool) ->
    Cosmic Location ->
    m (HashSet Location)
  neighborFunc isWithinRange loc = do
    locs <- filterM isWalkableLoc neighborLocs
    return $ HashSet.fromList $ map (view planar) locs
   where
    neighborLocs = getNeighborLocs loc
    isWalkableLoc someLoc =
      if not $ isWithinRange $ view planar someLoc
        then return False
        else do
          isGoal <- goalReachedFunc $ view planar someLoc
          if isGoal
            then return True
            else null <$> checkMoveFailureUnprivileged someLoc

  -- This is an optimization for when a specific location
  -- is given as the target.
  -- However, it is not strictly necessary, and in fact
  -- cannot be used when the target is a certain type of
  -- entity.
  distHeuristic :: Location -> Int32
  distHeuristic = case target of
    LocationTarget gLoc -> manhattan gLoc
    EntityTarget _eName -> const 0

  goalReachedFunc :: Has (State GameState) sig m => Location -> m Bool
  goalReachedFunc loc = case target of
    LocationTarget gLoc -> return $ loc == gLoc
    EntityTarget eName -> do
      me <- entityAt $ Cosmic currentSubworld loc
      return $ (view entityName <$> me) == Just eName

  -- A failsafe limit is hardcoded to prevent the game from freezing
  --  if an error exists in some .sw code.
  distLimit = maybe maxPathRange (min maxPathRange) maybeDistanceLimit
