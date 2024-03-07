-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Pathfinding cache invalidation logic
--
-- == Overview
-- Each time the 'Path' command is invoked, the computed
-- shortest-path is placed in in a cache specific to the invoking robot.
-- If the 'Path' command is invoked again by that robot
-- with identical arguments and from the same position, or a position lying
-- on the previously computed path, then the shortest path shall
-- be retrieved from the cache instead of being recomputed.
--
-- If the 'Path' command is re-invoked with different arguments
-- or from a novel position, then the shortest-path shall be
-- recomputed and the cache overwritten with this new result.
--
-- Asynchronous to the event of invoking the 'Path' command,
-- there are a variety of events that may invalidate
-- a previously-computed shortest path between some
-- location and a destination, including adding or removing
-- particular entities at certain locations.
--
-- Certain events allow for partial re-use of the previously
-- computed path.
module Swarm.Game.Step.Path.Cache (
  retrieveCachedPath,
  revalidatePathCache,
  recordCache,
) where

import Control.Arrow (left, (&&&))
import Control.Carrier.State.Lazy
import Control.Effect.Lens
import Control.Monad (unless)
import Data.Either.Extra (maybeToEither)
import Data.IntMap qualified as IM
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.Robot.Walk
import Swarm.Game.State
import Swarm.Game.Step.Path.Cache.DistanceLimit
import Swarm.Game.Step.Path.Type
import Swarm.Game.Step.Path.Walkability (checkUnwalkable)
import Swarm.Game.Step.RobotStepState
import Swarm.Game.Step.Util.Inspect (robotWithID)
import Swarm.Game.Universe (Cosmic (..), SubworldName)
import Swarm.Game.World.Modify
import Swarm.Util (prependList, tails1)
import Swarm.Util.RingBuffer qualified as RB

-- | Fetch the previously computed shortest path from the cache.
-- Log success or the reason it failed.
retrieveCachedPath ::
  HasRobotStepState sig m =>
  WalkabilityContext ->
  PathfindingParameters (Cosmic Location) ->
  m (Either CacheRetreivalInapplicability [Location])
retrieveCachedPath currentWalkabilityContext newParms = do
  pcr <- use $ pathCaching . pathCachingRobots
  rid <- use robotID
  let eitherCachedPath = guardFailures rid pcr
      myEntry :: CacheRetrievalAttempt
      myEntry = either RecomputationRequired (const Success) eitherCachedPath

  pathCaching . pathCachingLog
    %= RB.insert (CacheLogEntry rid $ RetrievalAttempt myEntry)

  return eitherCachedPath
 where
  PathfindingParameters currentDistanceLimit (Cosmic currentSubworld currentRobotLoc) target = newParms

  guardFailures rid pcr = do
    -- Checks whether this robot has a cached path
    cached <- maybeToEither NotCached $ IM.lookup rid pcr

    let PathfindingCache prevParms prevWalkabilityContext _targetLoc (CachedPath pathCells (TailMap ps)) = cached
        PathfindingParameters prevDistLimit previousSubworldName t = prevParms

    -- Subworlds must match
    unless (previousSubworldName == currentSubworld) $
      Left $
        DifferentArg NewSubworld

    -- Pathfinding target type must match
    unless (t == target) $
      Left $
        DifferentArg NewTargetType

    -- Walkability context must match
    unless (currentWalkabilityContext == prevWalkabilityContext) $
      Left $
        DifferentArg NewWalkabilityContext

    left (DifferentArg . NewDistanceLimit) $
      getDistanceLimitInvalidation currentRobotLoc pathCells currentDistanceLimit prevDistLimit

    -- Checks whether invoked from the same position or a position lying
    -- on the previously computed path
    maybeToEither PositionOutsidePath $ M.lookup currentRobotLoc ps

-- | Store a newly computed shortest path in the cache.
recordCache ::
  HasRobotStepState sig m =>
  PathfindingParameters SubworldName ->
  WalkabilityContext ->
  -- | includes robot starting position
  NonEmpty Location ->
  m ()
recordCache parms wc pathLocs = do
  rid <- use robotID
  pathCaching . pathCachingRobots %= IM.insert rid newCache
 where
  newCache = PathfindingCache parms wc (NE.last pathLocs) $ CachedPath pathLocs $ mkTailMap pathLocs

-- | For every non-empty suffix of the path, place its tail in a map keyed
-- by its head.
mkTailMap :: NonEmpty Location -> TailMap
mkTailMap pathLocs = TailMap locsMap
 where
  locsMap = M.fromList . NE.toList . NE.map (NE.head &&& NE.tail) $ tails1 pathLocs

-- |
-- Returns either a 'Left' which mandates cache invalidation (with a reason),
-- or a 'Right' containing a 'Maybe'; 'Nothing' indicates the cache should
-- remain unchanged, while 'Just' supplies a modified cache entry.
--
-- Cache is affected by modification of:
--
-- * "unwalkable" entities (an entity is placed or removed
--   that is "unwalkable" with respect to the invoking robot)
-- * "target" entities (if the `path` command had been invoked
--   with the modified entity as a target)
--
-- === Removed entity
--
-- * If an __unwalkable__ entity is removed from the map, the computed path shall be invalidated.
-- * If a __target__ entity is removed...
--
--     * ...that is the destination of the computed path, invalidate the cache
--     * ...that is /not/ the destination of the computed path, the cache is unaffected
--
-- === Added entity
--
-- * If an __unwalkable__ entity is added to the map, the computed path shall
--   only be invalidated /if the new entity lies on the path/.
-- * If a __target__ entity is added...
--
--     * ...that lies on the computed path, the computed path is truncated to that entity's location
--     * ...that does /not/ lie on the computed path, invalidate the cache
perhapsInvalidateForRobot ::
  WalkabilityContext ->
  -- | location of modified cell
  Cosmic Location ->
  -- | nature of entity modification
  CellModification Entity ->
  PathfindingCache ->
  Either InvalidationReason (Maybe PathfindingCache)
perhapsInvalidateForRobot
  walkInfo
  (Cosmic swn entityLoc)
  entityModification
  oldCache@(PathfindingCache parms _previousWalkabilityInfo destLoc p)
    | swn /= pathSubworld = Right Nothing
    | otherwise = case entityModification of
        Swap oldEntity newEntity ->
          handleRemovedEntity oldEntity >> handleNewEntity newEntity
        Remove oldEntity -> handleRemovedEntity oldEntity
        Add newEntity -> handleNewEntity newEntity
   where
    PathfindingParameters _distLimit pathSubworld tgt = parms
    CachedPath origPath (TailMap locmap) = p

    -- TODO: If using an entity Whitelist,
    -- cache invalidation logic is backwards
    isUnwalkable = not . null . checkUnwalkable walkInfo . Just
    isOnPath = entityLoc `M.member` locmap

    handleRemovedEntity oldEntity
      | destLoc == entityLoc = Left TargetEntityRemoved
      | isUnwalkable oldEntity = Left UnwalkableRemoved
      | otherwise = Right Nothing

    handleNewEntity newEntity
      | isUnwalkable newEntity && isOnPath = Left UnwalkableOntoPath
      | otherwise = case tgt of
          LocationTarget _locTarget -> Right Nothing
          EntityTarget targetEntityName -> handleNewEntityWithEntityTarget newEntity targetEntityName

    -- If the pathfinding target is an Entity rather than a specific location
    handleNewEntityWithEntityTarget newEntity targetEntityName
      | view entityName newEntity /= targetEntityName = Right Nothing
      | isOnPath = Right $ Just $ truncatePath origPath entityLoc oldCache
      | otherwise = Left TargetEntityAddedOutsidePath

-- | If the newly-added target entity lies on the existing path,
-- truncate the path to set it as the goal.
truncatePath ::
  NonEmpty Location ->
  Location ->
  PathfindingCache ->
  PathfindingCache
truncatePath origPath entityLoc oldCache =
  oldCache {cachedPath = CachedPath truncPath $ mkTailMap truncPath}
 where
  truncPath = prependList truncPathExcludingEntityLoc $ pure entityLoc
  truncPathExcludingEntityLoc = takeWhile (== entityLoc) $ NE.toList origPath

-- | Given an event that entails the modification of some cell,
-- check whether a shortest-path previously computed for a
-- given robot is still valid or can be updated.
revalidatePathCache ::
  (Has (State GameState) sig m) =>
  Cosmic Location ->
  CellModification Entity ->
  (RID, PathfindingCache) ->
  m ()
revalidatePathCache entityLoc entityModification (rid, pc) = do
  maybeRobot <- robotWithID rid
  let (logEntry, updateFunc) = getCacheUpdate $ checkPath maybeRobot
  pathCaching . pathCachingRobots %= updateFunc
  pathCaching . pathCachingLog %= RB.insert (CacheLogEntry rid logEntry)
 where
  checkPath = \case
    Nothing -> Left NonexistentRobot
    Just bot ->
      perhapsInvalidateForRobot
        (view walkabilityContext bot)
        entityLoc
        entityModification
        pc

  getCacheUpdate = \case
    Left reason -> (Invalidate reason, IM.delete rid)
    Right maybeReplacement -> case maybeReplacement of
      Nothing -> (Preserve Unmodified, id)
      Just newCache -> (Preserve PathTruncated, IM.insert rid newCache)
