{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Time effects
module Swarm.Effect.Time (
  Time,
  getNow,
  measureCpuTimeInSec,
  getZonedTime,

  -- ** Time Carrier
  TimeIOC,
  runTimeIO,

  -- ** Test Reader Carrier
  FakeTime,
  runFakeTime,
) where

import Control.Algebra
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Functor (($>))
import Data.Kind (Type)
import Data.Time.LocalTime qualified as LT
import System.CPUTime (getCPUTime)
import System.Clock (Clock (Monotonic), TimeSpec, getTime, toNanoSecs)

-- | Effect for things related to time
data Time (m :: Type -> Type) k where
  GetNow :: Time m TimeSpec
  GetCpuTime :: Time m Integer
  GetZonedTime :: Time m LT.ZonedTime

getNow :: Has Time sig m => m TimeSpec
getNow = send GetNow
{-# INLINE getNow #-}

measureCpuTimeInSec :: Has Time sig m => m a -> m (Double, a)
measureCpuTimeInSec f = do
  s <- send GetCpuTime
  res <- f
  e <- send GetCpuTime
  let elapsedSec = fromIntegral (e - s) * 1e-12
  pure (elapsedSec, res)
{-# INLINE measureCpuTimeInSec #-}

getZonedTime :: Has Time sig m => m LT.ZonedTime
getZonedTime = send GetZonedTime

newtype TimeIOC m a = TimeIOC (m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO)

runTimeIO :: TimeIOC m a -> m a
runTimeIO (TimeIOC m) = m
{-# INLINE runTimeIO #-}

instance (MonadIO m, Algebra sig m) => Algebra (Time :+: sig) (TimeIOC m) where
  alg hdl sig ctx = case sig of
    L cmd -> case cmd of
      GetNow -> (<$ ctx) <$> liftIO (System.Clock.getTime System.Clock.Monotonic)
      GetCpuTime -> (<$ ctx) <$> liftIO System.CPUTime.getCPUTime
      GetZonedTime -> (<$ ctx) <$> liftIO LT.getZonedTime
    R other -> TimeIOC (alg (runTimeIO . hdl) other ctx)
  {-# INLINE alg #-}

newtype FakeTime m a = FakeTime (TimeSpec -> LT.ZonedTime -> m a)
  deriving (Functor)

runFakeTime :: TimeSpec -> LT.ZonedTime -> FakeTime m a -> m a
runFakeTime t zt (FakeTime act) = act t zt
{-# INLINE runFakeTime #-}

instance Applicative m => Applicative (FakeTime m) where
  pure = FakeTime . const . const . pure
  {-# INLINE pure #-}
  FakeTime f <*> FakeTime a = FakeTime ((liftA2 . liftA2) (<*>) f a)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (FakeTime m) where
  FakeTime a >>= f = FakeTime (\t z -> a t z >>= runFakeTime t z . f)
  {-# INLINE (>>=) #-}

instance (Algebra sig m) => Algebra (Time :+: sig) (FakeTime m) where
  alg hdl sig ctx = FakeTime $ \t z -> case sig of
    L GetNow -> pure (ctx $> t)
    L GetCpuTime -> pure (ctx $> 1000 * toNanoSecs t)
    L GetZonedTime -> pure (ctx $> z)
    R other -> alg (runFakeTime t z . hdl) other ctx
  {-# INLINE alg #-}
