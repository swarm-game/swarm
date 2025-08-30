{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Game-related state and utilities
--
-- Stateful versions of world functions from 'Swarm.Game.World.Pure'.
--
-- The lookup functions cache loaded tiles and should almost always be used instead.
module Swarm.Game.World.Stateful (
  -- ** Lookup
  lookupTerrainM,
  lookupEntityM,
  lookupContentM,

  -- ** Update
  updateM,

  -- ** Loading
  loadCellM,
  loadRegionM,
) where

import Control.Algebra (Has)
import Control.Effect.State (State, get, modify, state)
import Data.Array.IArray
import Data.Array.Unboxed qualified as U
import Swarm.Game.Entity (Entity)
import Swarm.Game.Scenario.Topography.Modify
import Swarm.Game.World.Coords
import Swarm.Game.World.Pure

type HasWorldStateEffect t e sig m =
  ( IArray U.UArray t
  , Has (State (World t e)) sig m
  , Has Effect.Metric sig m
  , Has Effect.Time sig m
  )

-- | A stateful variant of 'lookupTerrain', which first loads the tile
--   containing the given coordinates if it is not already loaded,
--   then looks up the terrain value.
lookupTerrainM ::
  forall t e sig m.
  (Has (State (World t e)) sig m, IArray U.UArray t) =>
  Coords ->
  m t
lookupTerrainM c = do
  modify @(World t e) $ loadCell c
  lookupTerrain c <$> get @(World t e)

lookupContentM ::
  forall t e sig m.
  (Has (State (World t e)) sig m, IArray U.UArray t) =>
  Coords ->
  m (t, Maybe e)
lookupContentM c = do
  modify @(World t e) $ loadCell c
  w <- get @(World t e)
  return (lookupTerrain c w, lookupEntity c w)

-- | A stateful variant of 'lookupEntity', which first loads the tile
--   containing the given coordinates if it is not already loaded,
--   then looks up the terrain value.
lookupEntityM ::
  forall t e sig m.
  (Has (State (World t e)) sig m, IArray U.UArray t) =>
  Coords ->
  m (Maybe e)
lookupEntityM c = do
  modify @(World t e) $ loadCell c
  lookupEntity c <$> get @(World t e)

-- | A stateful variant of 'update', which also ensures the tile
--   containing the given coordinates is loaded.
updateM ::
  forall t sig m.
  (Has (State (World t Entity)) sig m, IArray U.UArray t) =>
  Coords ->
  (Maybe Entity -> Maybe Entity) ->
  m (CellUpdate Entity)
updateM c g = do
  state @(World t Entity) $ update c g . loadCell c

loadCellM ::
  forall t e sig m.
  HasWorldStateEffect t e sig m =>
  Maybe WorldMetrics ->
  Coords ->
  m ()
loadCellM wm c = loadRegionM @t @e wm (c, c)

loadRegionM ::
  forall t e sig m.
  HasWorldStateEffect t e sig m =>
  Maybe WorldMetrics ->
  (Coords, Coords) ->
  m ()
loadRegionM wm = updateMetric . state @(World t e) . loadRegion'
 where
  loadRegion' :: (Coords, Coords) -> World t e -> (World t e, [TileCoords])
  loadRegion' cc ow = let (nw, ts) = loadRegion cc ow in nw.tileCache `seq` (nw, ts)
  updateMetric :: m [TileCoords] -> m ()
  updateMetric m = case wm of
    Nothing -> void m
    Just wMetrics -> do
      (loadTime, loadedTiles) <- Effect.measureCpuTimeInSec m
      inMemoryTiles <- M.size . (.tileCache) <$> get @(World t e)
      Effect.gaugeSet wMetrics.inMemoryTiles inMemoryTiles
      unless (null loadedTiles) $ do
        let loadedCount = length loadedTiles
        let avgTime = loadTime / fromIntegral loadedCount
        Effect.gaugeAdd wMetrics.loadedTiles loadedCount
        Effect.distributionAdd wMetrics.tilesBatchLoadTime loadTime
        Effect.distributionAdd wMetrics.tileAverageLoadTime avgTime
