-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Implementation of the 'Swarm.Language.Syntax.Path' command for robots.
--
-- = Design considerations
-- In general the playfield can be dynamic, and obstructions may
-- appear that invalidate a given computed shortest path.
-- Therefore, there would be limited value in a command that returns
-- an entirely static computed path that is somehow stored on the client side
-- (i.e. inside a swarm-lang program).
--
-- In the current implementation, a complete path is computed
-- internally upon invoking the @path@ command
-- and doled out incrementally across ticks.
-- Each @path@ invocation returns the direction of the
-- next "move" along the computed shortest path.
--
-- This internally stored path is re-used across invocations until some
-- event invalidates its cache (see "Swarm.Game.Step.Path.Cache").
--
-- == Max distance
--
-- We allow the caller to supply a max distance, but also impose an internal maximum
-- distance to prevent programming errors from irrecoverably freezing the game.
module Swarm.Game.Step.Path.Finding where

import Control.Effect.Lens as Fused
import Control.Lens ((^.))
import Control.Monad (filterM, guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.Graph.AStar (aStarM)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.State.Landscape (multiWorld)
import Swarm.Game.Step.Path.Cache
import Swarm.Game.Step.Path.Cache.DistanceLimit (withinDistance)
import Swarm.Game.Step.Path.Type
import Swarm.Game.Step.RobotStepState
import Swarm.Game.Step.Util
import Swarm.Game.Step.Util.Inspect
import Swarm.Game.Universe
import Swarm.Game.World (locToCoords, lookupCosmicEntity)
import Swarm.Language.Syntax
import Swarm.Language.Syntax.Direction

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
  forall sig m.
  HasRobotStepState sig m =>
  PathfindingParameters (Cosmic Location) ->
  m (Maybe (Direction, Int))
pathCommand parms = do
  currentWalkabilityContext <- use (walkabilityContext @Instantiated)

  -- First, check if the pathfinding target has a cached path.
  eitherCachedPath <- retrieveCachedPath currentWalkabilityContext parms

  case eitherCachedPath of
    Right foundCachedPath -> return $ Just $ mkResult foundCachedPath
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
        return $ mkResult foundPath
 where
  mkResult p = (nextDir p, length p)
  PathfindingParameters maybeDistanceLimit (Cosmic currentSubworld robotLoc) target = parms

  computePath :: m (Maybe [Location])
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

  goalReachedFunc :: Location -> m Bool
  goalReachedFunc loc = case target of
    LocationTarget gLoc -> return $ loc == gLoc
    EntityTarget eName -> do
      worlds <- Fused.use $ landscape . multiWorld
      let me = lookupCosmicEntity (Cosmic currentSubworld $ locToCoords loc) worlds
      return $ (view entityName <$> me) == Just eName

  -- A failsafe limit is hardcoded to prevent the game from freezing
  --  if an error exists in some .sw code.
  distLimit = maybe maxPathRange (min maxPathRange) maybeDistanceLimit
