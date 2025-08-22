{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Game-related state and utilities
--
-- Definition of metrics tracked for world.
module Swarm.Game.World.WorldMetrics (
  WorldMetrics (..),
  initWorldMetrics,
) where

import System.Metrics
import System.Metrics.Distribution (Distribution)
import System.Metrics.Gauge (Gauge)

-- | Metrics tracked in Swarm game engine.
data WorldMetrics = WorldMetrics
  { loadedTiles :: Gauge
  , tileLoadTime :: Distribution
  }

-- | Create and register the metrics to metric store.
--
-- This function can be only called **once** on the store.
initWorldMetrics :: Store -> IO WorldMetrics
initWorldMetrics s = do
  loadedTiles <- createGauge "game.tile_loaded" s
  tileLoadTime <- createDistribution "game.tile_load_time" s
  pure WorldMetrics {..}
