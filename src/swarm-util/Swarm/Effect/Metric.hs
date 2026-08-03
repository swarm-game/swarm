{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Metrics effects, to avoid unrestricted IO
module Swarm.Effect.Metric (
  Metric (..),
  counterInc,
  gaugeAdd,
  gaugeSet,
  distributionAdd,

  -- * Metric Handler
  runMetricIO,

  -- ** Test Fake Handler
  runFakeMetric,
) where

import Effectful
import Effectful.Dispatch.Dynamic
import System.Metrics.Counter qualified as Counter
import System.Metrics.Distribution qualified as Distribution
import System.Metrics.Gauge qualified as Gauge

data Metric :: Effect where
  CounterInc :: Counter.Counter -> Metric m ()
  GaugeAdd :: Gauge.Gauge -> Int -> Metric m ()
  GaugeSet :: Gauge.Gauge -> Int -> Metric m ()
  DistributionAdd :: Distribution.Distribution -> Double -> Metric m ()

type instance DispatchOf Metric = Dynamic

counterInc :: (HasCallStack, Metric :> es) => Counter.Counter -> Eff es ()
counterInc = send . CounterInc
{-# INLINE counterInc #-}

gaugeAdd :: (HasCallStack, Metric :> es) => Gauge.Gauge -> Int -> Eff es ()
gaugeAdd g = send . GaugeAdd g
{-# INLINE gaugeAdd #-}

gaugeSet :: (HasCallStack, Metric :> es) => Gauge.Gauge -> Int -> Eff es ()
gaugeSet g = send . GaugeSet g
{-# INLINE gaugeSet #-}

distributionAdd :: (HasCallStack, Metric :> es) => Distribution.Distribution -> Double -> Eff es ()
distributionAdd d = send . DistributionAdd d
{-# INLINE distributionAdd #-}

runMetricIO :: (IOE :> es) => Eff (Metric : es) a -> Eff es a
runMetricIO = interpret $ \_ -> \case
  CounterInc c -> liftIO (Counter.inc c)
  DistributionAdd d v -> liftIO (Distribution.add d v)
  GaugeAdd g v -> liftIO (Gauge.add g $ fromIntegral v)
  GaugeSet g v -> liftIO (Gauge.set g $ fromIntegral v)

runFakeMetric :: Eff (Metric : es) a -> Eff es a
runFakeMetric = interpret $ \_ -> \case
  CounterInc _ -> pure ()
  GaugeAdd _ __ -> pure ()
  GaugeSet _ _ -> pure ()
  DistributionAdd _ _ -> pure ()
{-# INLINE runFakeMetric #-}
