-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Pathfinding cache invalidation logic
module Swarm.Game.Step.Path.Cache where

import Control.Arrow ((&&&))
import Control.Carrier.State.Lazy
import Control.Effect.Lens
import Data.IntMap qualified as IM
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.Step.Path.Type
import Swarm.Game.Step.Sig
import Swarm.Game.Universe (Cosmic (..), SubworldName)
import Swarm.Game.World.Modify
import Swarm.Util (tails1)
import Swarm.Util.RingBuffer qualified as RB

recordCache ::
  (HasRobotStepState sig m, Has (State GameState) sig m) =>
  PathfindingTarget ->
  SubworldName ->
  -- | includes robot starting position
  NonEmpty Location ->
  m ()
recordCache t swn pathLocs = do
  rid <- use robotID
  pathCaching . pathCachingRobots
    %= IM.insert rid (PathfindingCache t swn (NE.last pathLocs) pathLocs $ mkTailMap pathLocs)

mkTailMap :: NonEmpty Location -> TailMap
mkTailMap pathLocs = TailMap locsMap
 where
  locsMap = M.fromList . NE.toList . NE.map (NE.head &&& NE.tail) $ tails1 pathLocs

-- | TODO: Implement addition/removal of unwalkable entities
-- for both modes of pathfinding invocation
perhapsInvalidateForRobot ::
  Cosmic Location ->
  CellModification Entity ->
  PathfindingCache ->
  Either InvalidationReason (Maybe PathfindingCache)
perhapsInvalidateForRobot
  (Cosmic swn loc)
  entityModification
  (PathfindingCache tgt pathSubworld destLoc origPath (TailMap locmap))
    | swn /= pathSubworld = Right Nothing
    | otherwise = case tgt of
        LocationTarget _locTarget -> Right Nothing
        EntityTarget targetEntityName -> case entityModification of
          Swap oldEntity newEntity ->
            -- TODO: Is this correct?
            handleRemovedEntity oldEntity >> handleNewEntity newEntity targetEntityName
          Remove oldEntity -> handleRemovedEntity oldEntity
          Add newEntity -> handleNewEntity newEntity targetEntityName
   where
    handleRemovedEntity _oldEntity =
      if destLoc == loc
        then Left TargetEntityRemoved
        else Right Nothing -- No change to cache
    handleNewEntity newEntity targetEntityName
      | view entityName newEntity /= targetEntityName = Right Nothing -- No change to cache
      -- If the newly-added target entity lies on the existing path,
      -- truncate the path to set it as the goal.
      | loc `M.member` locmap =
          let truncatedCache = PathfindingCache tgt pathSubworld loc truncPath $ mkTailMap truncPath
              truncPath = NE.prependList truncPathExcluding $ pure loc
              (truncPathExcluding, _) = NE.break (/= loc) origPath
           in Right $ Just truncatedCache
      | otherwise = Left TargetEntityAddedOutsidePath

revalidatePathCache ::
  (Has (State GameState) sig m) =>
  Cosmic Location ->
  CellModification Entity ->
  (RID, PathfindingCache) ->
  m ()
revalidatePathCache cLoc entityModification (rid, pc) = do
  pathCaching . pathCachingRobots %= updateFunc
  pathCaching . pathCachingLog %= RB.insert logEntry
 where
  eitherNewPathfinding = perhapsInvalidateForRobot cLoc entityModification pc
  (logEntry, updateFunc) = case eitherNewPathfinding of
    Left reason -> (Invalidate reason, IM.delete rid)
    Right maybeReplacement -> case maybeReplacement of
      Nothing -> (Preserve Unmodified, id)
      Just x -> (Preserve PathTruncated, IM.insert rid x)
