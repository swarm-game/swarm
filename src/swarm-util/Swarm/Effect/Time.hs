{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Time effects
module Swarm.Effect.Time (
  Time,
  getNow,
  measureCpuTimeInSec,

  -- ** Time Carrier
  TimeIOC (..),

  -- ** Test Reader Carrier
  FakeTime,
  runFakeTime,
) where

import Control.Algebra
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Functor (($>))
import Data.Kind (Type)
import System.CPUTime
import System.Clock (Clock (Monotonic), TimeSpec, getTime, toNanoSecs)

-- | Effect for things related to time
data Time (m :: Type -> Type) k where
  GetNow :: Time m TimeSpec
  GetCpuTime :: Time m Integer

getNow :: Has Time sig m => m TimeSpec
getNow = send GetNow

measureCpuTimeInSec :: Has Time sig m => m a -> m (Double, a)
measureCpuTimeInSec f = do
  s <- send GetCpuTime
  res <- f
  e <- send GetCpuTime
  let elapsedSec = fromIntegral (e - s) * 1e-12
  pure (elapsedSec, res)

newtype TimeIOC m a = TimeIOC {runTimeIO :: m a}
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Time :+: sig) (TimeIOC m) where
  alg hdl sig ctx = case sig of
    L GetNow -> (<$ ctx) <$> liftIO (System.Clock.getTime System.Clock.Monotonic)
    L GetCpuTime -> (<$ ctx) <$> liftIO System.CPUTime.getCPUTime
    R other -> TimeIOC (alg (runTimeIO . hdl) other ctx)

newtype FakeTime m a = FakeTime (TimeSpec -> m a)
  deriving (Functor)

runFakeTime :: TimeSpec -> FakeTime m a -> m a
runFakeTime t (FakeTime act) = act t

instance Applicative m => Applicative (FakeTime m) where
  pure = FakeTime . const . pure
  FakeTime f <*> FakeTime a = FakeTime (liftA2 (<*>) f a)

instance Monad m => Monad (FakeTime m) where
  FakeTime a >>= f = FakeTime (\r -> a r >>= runFakeTime r . f)

instance (Algebra sig m) => Algebra (Time :+: sig) (FakeTime m) where
  alg hdl sig ctx = FakeTime $ \fakeTime -> case sig of
    L GetNow -> pure (ctx $> fakeTime)
    L GetCpuTime -> pure (ctx $> 1000 * toNanoSecs fakeTime)
    R other -> alg (runFakeTime fakeTime . hdl) other ctx
