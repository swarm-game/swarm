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
import Control.Lens ((^.))
import Control.Monad (unless)
import Data.Either.Extra (maybeToEither)
import Data.IntMap qualified as IM
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Tuple.Extra (both)
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.Robot.Walk
import Swarm.Game.Scenario.Topography.Terraform
import Swarm.Game.State
import Swarm.Game.Step.Path.Cache.DistanceLimit
import Swarm.Game.Step.Path.Type
import Swarm.Game.Step.Path.Walkability (checkUnwalkable)
import Swarm.Game.Step.RobotStepState
import Swarm.Game.Step.Util.Inspect (robotWithID)
import Swarm.Game.Universe (Cosmic (..), SubworldName)
import Swarm.Language.Syntax (Phase (Instantiated))
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
-- * cell walkability (i.e., an entity is placed or removed
--   that is "unwalkable" (blacklist) or "exclusively walkable" (whitelist)
--   with respect to the invoking robot
-- * "target" entities (if the `path` command had been invoked
--   with the modified entity as a target). Note that it is impossible
--   to find a path to an "unwalkable" target, so this nonsensical case
--   is ignored for the purpose of cache invalidation.
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
        Swap oldEntity newEntity -> deriveBarrierModification $ both Just (oldEntity, newEntity)
        Remove oldEntity -> deriveBarrierModification (Just oldEntity, Nothing)
        Add newEntity -> deriveBarrierModification (Nothing, Just newEntity)
   where
    PathfindingParameters _distLimit pathSubworld tgt = parms
    CachedPath origPath (TailMap locmap) = p

    isWalkable = null . checkUnwalkable walkInfo
    isOnPath = entityLoc `M.member` locmap

    -- NOTE: oldContent and newContent are guaranteed to be different,
    -- because the 'Swap' constructor enforces such.
    deriveBarrierModification change@(_oldContent, newContent) =
      case tgt of
        LocationTarget _locTarget -> barrierChange
        -- If the location of the changed entity was the terminus
        -- of the path, and the path search is "by entity", then
        -- we know that the path must be invalidated due to removal
        -- of the goal.
        -- Also, we know that a "target entity" on the path will
        -- only ever exist the path's terminus; otherwise the
        -- terminus would have been earlier!
        EntityTarget targetEntityName -> handleEntityTarget targetEntityName
     where
      handleEntityTarget targetEntityName
        | destLoc == entityLoc = Left TargetEntityRemoved
        | maybe True ((/= targetEntityName) . (^. entityName)) newContent = barrierChange
        | isOnPath = Right $ Just $ truncatePath origPath entityLoc oldCache
        | otherwise = Left TargetEntityAddedOutsidePath

      walkabilityPair = both isWalkable change
      barrierChange
        | uncurry (==) walkabilityPair = Right Nothing
        | snd walkabilityPair = Left UnwalkableRemoved
        | isOnPath = Left UnwalkableOntoPath
        -- addition of a barrier outside of the path is irrelevant.
        | otherwise = Right Nothing

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
