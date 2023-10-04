{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Pathfinding types
module Swarm.Game.Step.Path.Type where

import Control.Lens
import Data.Aeson (ToJSON (..))
import Data.IntMap.Strict (IntMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as M
import GHC.Generics (Generic)
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Universe (SubworldName)
import Swarm.Util.Lens (makeLensesNoSigs)
import Swarm.Util.RingBuffer

maxLogEntries :: Int
maxLogEntries = 32

data CachePreservationMode
  = Unmodified
  | PathTruncated
  deriving (Generic, ToJSON)

data CacheUpdate
  = Invalidate InvalidationReason
  | Preserve CachePreservationMode
  deriving (Generic, ToJSON)

-- | Reasons for cache being invalidated
data InvalidationReason
  = ArgumentMismatch
  | TargetEntityAddedOutsidePath
  | TargetEntityRemoved
  deriving (Generic, ToJSON)

emptyPathCache :: PathCaching
emptyPathCache = PathCaching mempty $ mkRingBuffer $ Finite maxLogEntries

-- | Shortest paths can either be computed to the nearest entity of
-- a given type or to a specific location.
data PathfindingTarget
  = LocationTarget Location
  | -- | Note: navigation to entities does not benefit from the
    -- distance heuristic optimization of the A* algorithm.
    EntityTarget EntityName
  deriving (Generic, Eq, Show, ToJSON)

newtype TailMap = TailMap (Map Location [Location])
  deriving (Generic, Eq, Show)

instance ToJSON TailMap where
  toJSON (TailMap x) = toJSON $ M.toList x

-- | A per-robot cache for the @path@ command.
data PathfindingCache = PathfindingCache
  { pathTarget :: PathfindingTarget
  , targetWorld :: SubworldName
  , targetLoc :: Location
  , originalPath :: NonEmpty Location
  -- ^ O(log n) lookup of path suffix by
  -- current location
  , locations :: TailMap
  }
  deriving (Generic, Eq, Show, ToJSON)

data PathCaching = PathCaching
  { _pathCachingRobots :: IntMap PathfindingCache
  -- ^ Keyed by RID
  , _pathCachingLog :: RingBuffer CacheUpdate
  -- ^ For diagnostics/debugging
  }
makeLensesNoSigs ''PathCaching

-- | All the RIDs of robots that are storing a cached path that
-- may require invalidation.
pathCachingRobots :: Lens' PathCaching (IntMap PathfindingCache)
pathCachingLog :: Lens' PathCaching (RingBuffer CacheUpdate)
