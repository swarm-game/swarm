{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
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

  -- * Metric Carrier
  MetricIOC (..),

  -- ** Test Fake Carrier
  FakeMetric (..),
) where

import Control.Algebra
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Kind (Type)
import System.Metrics.Counter qualified as Counter
import System.Metrics.Distribution qualified as Distribution
import System.Metrics.Gauge qualified as Gauge

data Metric (m :: Type -> Type) k where
  CounterInc :: Counter.Counter -> Metric m ()
  GaugeAdd :: Gauge.Gauge -> Int -> Metric m ()
  GaugeSet :: Gauge.Gauge -> Int -> Metric m ()
  DistributionAdd :: Distribution.Distribution -> Double -> Metric m ()

counterInc :: Has Metric sig m => Counter.Counter -> m ()
counterInc = send . CounterInc

gaugeAdd :: Has Metric sig m => Gauge.Gauge -> Int -> m ()
gaugeAdd g = send . GaugeAdd g

gaugeSet :: Has Metric sig m => Gauge.Gauge -> Int -> m ()
gaugeSet g = send . GaugeSet g

distributionAdd :: Has Metric sig m => Distribution.Distribution -> Double -> m ()
distributionAdd d = send . DistributionAdd d

newtype MetricIOC m a = MetricIOC {runMetricIO :: m a}
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Metric :+: sig) (MetricIOC m) where
  alg hdl sig ctx = case sig of
    L (CounterInc c) -> (<$ ctx) <$> liftIO (Counter.inc c)
    L (DistributionAdd d v) -> (<$ ctx) <$> liftIO (Distribution.add d v)
    L (GaugeAdd g v) -> (<$ ctx) <$> liftIO (Gauge.add g $ fromIntegral v)
    L (GaugeSet g v) -> (<$ ctx) <$> liftIO (Gauge.set g $ fromIntegral v)
    R other -> MetricIOC (alg (runMetricIO . hdl) other ctx)

newtype FakeMetric m a = FakeMetric {runFakeMetric :: m a}
  deriving newtype (Applicative, Functor, Monad)

instance (Algebra sig m) => Algebra (Metric :+: sig) (FakeMetric m) where
  alg hdl sig ctx = case sig of
    L (CounterInc {}) -> pure ctx
    L (DistributionAdd {}) -> pure ctx
    L (GaugeAdd {}) -> pure ctx
    L (GaugeSet {}) -> pure ctx
    R other -> FakeMetric (alg (runFakeMetric . hdl) other ctx)
