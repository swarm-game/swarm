{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
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

  -- TODO: #2555
  -- Loading
  -- loadCellM,
  -- loadRegionM,
) where

import Control.Algebra (Has)
import Control.Effect.State (State, get, state)
import Control.Monad (unless, void)
import Data.Array.IArray
import Data.Array.Unboxed qualified as U
import Data.Map.Strict qualified as M
import Swarm.Effect as Effect
import Swarm.Game.Entity (Entity)
import Swarm.Game.Scenario.Topography.Modify
import Swarm.Game.World.Coords
import Swarm.Game.World.Metrics (WorldMetrics (..))
import Swarm.Game.World.Pure
import Swarm.Game.World.Tile

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

