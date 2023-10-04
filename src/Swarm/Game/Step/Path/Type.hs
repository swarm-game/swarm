{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for shortest-path-finding and logging
-- path-cache invalidation events.
--
-- By convention, a @[Location]@ /does not/ include the
-- starting location, whereas a @NonEmpty Location@ does.
--
-- Consequentially, an empty @[Location]@ implies that
-- the robot's current location is already at the goal location.
module Swarm.Game.Step.Path.Type where

import Control.Lens
import Data.Aeson (Options (..), SumEncoding (ObjectWithSingleField), ToJSON (..), defaultOptions, genericToJSON)
import Data.IntMap.Strict (IntMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as M
import GHC.Generics (Generic)
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Robot (RID, WalkabilityContext)
import Swarm.Game.Universe (SubworldName)
import Swarm.Util.Lens (makeLensesNoSigs)
import Swarm.Util.RingBuffer

maxLogEntries :: Int
maxLogEntries = 32

-- | This is parameterized on the starting location,
-- as we may either want to:
--
-- 1. provide the planar start location (when first /computing/ the path), or
-- 2. suppress it and only propagate the subworld name
--    (when /retrieving/ from cache), which precludes the possibility of
--    downstream accidentally mixing up the planar location of the /target/
--    with the current /robot location/.
data PathfindingParameters a = PathfindingParameters
  { distanceLimit :: Maybe Integer
  -- ^ Manhattan distance limit on cells to explore
  -- (NOTE: this is not a "path length" limit)
  , startingLoc :: a
  -- ^ Starting location
  , searchGoal :: PathfindingTarget
  -- ^ Search goal
  }
  deriving (Generic, Eq, Show, ToJSON, Functor)

-- | It is possible for the cache to be unaffected
-- by certain events, or the cache may modified without
-- fully recomputing the shortest path.
data CachePreservationMode
  = Unmodified
  | PathTruncated
  deriving (Show, Eq, Generic, ToJSON)

data CacheLogEntry = CacheLogEntry {
    robot :: RID
  , event :: CacheEvent
  } deriving (Show, Eq, Generic, ToJSON)

objectSingleFieldEncoding :: Options
objectSingleFieldEncoding =
  defaultOptions
    { sumEncoding = ObjectWithSingleField
    }

data CacheRetrievalAttempt
  = Success
  | RecomputationRequired CacheRetreivalInapplicability
  deriving (Show, Eq, Generic)

instance ToJSON CacheRetrievalAttempt where
  toJSON = genericToJSON objectSingleFieldEncoding

-- | Certain events can obligate the cache to be
-- completely invalidated, or partially or fully preserved.
data CacheEvent
  = Invalidate InvalidationReason
  | Preserve CachePreservationMode
  | RetrievalAttempt CacheRetrievalAttempt
  deriving (Show, Eq, Generic)

instance ToJSON CacheEvent where
  toJSON = genericToJSON objectSingleFieldEncoding

data DistanceLimitChange
  = LimitIncreased
  | PathExceededLimit
  deriving (Show, Eq, Generic, ToJSON)

data DifferentArgument
  = NewSubworld
  | NewTargetType
  | NewWalkabilityContext
  | NewDistanceLimit DistanceLimitChange
  deriving (Show, Eq, Generic, ToJSON)

-- | Reasons why we cannot re-use a precomputed path
-- from the cache upon re-invoking the 'Path' command
data CacheRetreivalInapplicability
  = NotCached
  | DifferentArg DifferentArgument
  | PositionOutsidePath
  deriving (Show, Eq, Generic)

instance ToJSON CacheRetreivalInapplicability where
  toJSON = genericToJSON objectSingleFieldEncoding

-- | Reasons for cache being invalidated
data InvalidationReason
  = TargetEntityAddedOutsidePath
  | TargetEntityRemoved
  | UnwalkableRemoved
  | UnwalkableOntoPath
  | NonexistentRobot
  deriving (Show, Eq, Generic, ToJSON)

emptyPathCache :: PathCaching
emptyPathCache = PathCaching mempty $ mkRingBuffer $ Finite maxLogEntries

-- | Shortest paths can either be computed to the nearest entity of
-- a given type or to a specific location.
data PathfindingTarget
  = LocationTarget Location
  | -- | Note: navigation to entities does not benefit from the
    -- distance heuristic optimization of the A* algorithm
    -- (but see #1568)
    EntityTarget EntityName
  deriving (Generic, Eq, Show, ToJSON)

-- | Facilitates lookup of any shortest path to a particular
-- goal cell, given a location that already lies on a
-- shortest path.
newtype TailMap = TailMap (Map Location [Location])
  deriving (Generic, Eq, Show)

instance ToJSON TailMap where
  toJSON (TailMap x) = toJSON $ M.toList x

-- | A per-robot cache for the @path@ command.
data PathfindingCache = PathfindingCache
  { invocationParms :: PathfindingParameters SubworldName
  , walkabilityInfo :: WalkabilityContext
  , targetLoc :: Location
  , originalPath :: NonEmpty Location
  , locations :: TailMap
  -- ^ Fast lookup map of path suffix by
  -- current location
  }
  deriving (Generic, Eq, Show, ToJSON)

data PathCaching = PathCaching
  { _pathCachingRobots :: IntMap PathfindingCache
  -- ^ Keyed by RID
  , _pathCachingLog :: RingBuffer CacheLogEntry
  -- ^ For diagnostics/testing/debugging
  }
makeLensesNoSigs ''PathCaching

-- | All the RIDs of robots that are storing a cached path that
-- may require invalidation.
pathCachingRobots :: Lens' PathCaching (IntMap PathfindingCache)

-- | Event log for cache invalidation
pathCachingLog :: Lens' PathCaching (RingBuffer CacheLogEntry)
