-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Handles cache invalidation if the distance
-- limit is modified between invocations of
-- the 'Path' command.
module Swarm.Game.Step.Path.Cache.DistanceLimit (
  getDistanceLimitInvalidation,
  withinDistance,
) where

import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Swarm.Game.Location
import Swarm.Game.Step.Path.Type

-- |
-- A greater distance limit might yield a shorter path
-- if there was an better route that just needed to venture outside
-- of the allowed radius.
--
-- On the other hand, a smaller distance limit /will not/ invalidate
-- the cache so long as all cells on the path are within the new limit.
getDistanceLimitInvalidation ::
  -- | current robot location
  Location ->
  -- | original path
  NonEmpty Location ->
  -- | current limit
  Maybe Integer ->
  -- | previous limit
  Maybe Integer ->
  Either DistanceLimitChange ()
-- Limit unchanged:
getDistanceLimitInvalidation _ _ Nothing Nothing = return ()
-- Limit was increased to infinity:
getDistanceLimitInvalidation _ _ Nothing (Just _) = Left LimitIncreased
-- Limit was decreased from infinity:
getDistanceLimitInvalidation robotLoc pathCells (Just currLimit) Nothing =
  handleLimitDecreased robotLoc pathCells currLimit
getDistanceLimitInvalidation robotLoc pathCells (Just currLimit) (Just prevLimit)
  | currLimit < prevLimit = handleLimitDecreased robotLoc pathCells currLimit
  | currLimit > prevLimit = Left LimitIncreased
  | otherwise = return () -- Limit unchanged

handleLimitDecreased ::
  Location ->
  NonEmpty Location ->
  Integer ->
  Either DistanceLimitChange ()
handleLimitDecreased robotLoc pathCells currLimit =
  unless (all (withinDistance currLimit robotLoc) $ NE.tail pathCells) $
    Left PathExceededLimit

-- * Utility functions

-- | This function is shared between path computation logic
-- and patch cache invalidation logic to ensure that
-- the inequality operator is consistent (e.g. @<@ vs. @<=@).
withinDistance ::
  -- | distance limit
  Integer ->
  -- | current robot location
  Location ->
  -- | target location
  Location ->
  Bool
withinDistance distLimit robotLoc = (<= distLimit) . fromIntegral . manhattan robotLoc
