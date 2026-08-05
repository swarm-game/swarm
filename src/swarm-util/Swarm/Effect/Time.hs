{-# LANGUAGE TypeFamilies #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Time effects
module Swarm.Effect.Time (
  Time,
  getNow,
  measureCpuTimeInSec,
  getZonedTime,

  -- ** Time Handler
  runTimeIO,

  -- ** Test Reader Handler
  runFakeTime,
) where

import Data.Time.LocalTime qualified as LT
import Effectful
import Effectful.Dispatch.Dynamic
import System.CPUTime (getCPUTime)
import System.Clock (Clock (Monotonic), TimeSpec, getTime, toNanoSecs)

-- | Effect for things related to time
data Time :: Effect where
  GetNow :: Time m TimeSpec
  GetCpuTime :: Time m Integer
  GetZonedTime :: Time m LT.ZonedTime

type instance DispatchOf Time = Dynamic

getNow :: (HasCallStack, Time :> es) => Eff es TimeSpec
getNow = send GetNow
{-# INLINE getNow #-}

measureCpuTimeInSec :: (Time :> es) => Eff es a -> Eff es (Double, a)
measureCpuTimeInSec f = do
  s <- getNow
  res <- f
  e <- getNow
  let elapsedSec = fromIntegral (e - s) * 1e-12
  pure (elapsedSec, res)
{-# INLINE measureCpuTimeInSec #-}

getZonedTime :: (HasCallStack, Time :> es) => Eff es LT.ZonedTime
getZonedTime = send GetZonedTime
{-# INLINE getZonedTime #-}

runTimeIO :: (IOE :> es) => Eff (Time : es) a -> Eff es a
runTimeIO = interpret $ \_ -> \case
  GetNow -> liftIO (System.Clock.getTime System.Clock.Monotonic)
  GetCpuTime -> liftIO System.CPUTime.getCPUTime
  GetZonedTime -> liftIO LT.getZonedTime
{-# INLINE runTimeIO #-}

runFakeTime :: TimeSpec -> LT.ZonedTime -> Eff (Time : es) a -> Eff es a
runFakeTime t zt = interpret $ \_ -> \case
  GetNow -> pure t
  GetCpuTime -> pure (1000 * toNanoSecs t)
  GetZonedTime -> pure zt
