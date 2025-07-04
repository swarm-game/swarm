{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Game-related state and utilities
--
-- Definition of metrics tracked for game.
module Swarm.Game.State.GameMetrics (
  GameMetrics (..),
  initGameMetrics,
) where

import System.Metrics
import System.Metrics.Counter (Counter)
import System.Metrics.Distribution (Distribution)
import System.Metrics.Gauge (Gauge)

-- | Metrics tracked in Swarm game engine.
data GameMetrics = GameMetrics
  { tickCounter :: Counter
  , tickDistribution :: Distribution
  , robotsGauge :: Gauge
  , activeRobotsGauge :: Gauge
  }

-- | Create and register the metrics to metric store.
--
-- This function can be only called **once** on the store.
initGameMetrics :: Store -> IO GameMetrics
initGameMetrics s = do
  tickCounter <- createCounter "game.tick_count" s
  tickDistribution <- createDistribution "game.tick_time" s
  robotsGauge <- createGauge "game.robots_total" s
  activeRobotsGauge <- createGauge "game.robots_active" s
  pure GameMetrics {..}
