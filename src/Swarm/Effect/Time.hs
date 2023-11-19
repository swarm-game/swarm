{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Swarm.Effect.Time where

import Control.Algebra
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Kind (Type)
import System.Clock (Clock (Monotonic), TimeSpec, getTime)

-- | Effect for things related to time
data Time (m :: Type -> Type) k where
  GetNow :: Time m TimeSpec

getNow :: Has Time sig m => m TimeSpec
getNow = send GetNow

newtype TimeIOC m a = TimeIOC {runTimeIO :: m a}
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Time :+: sig) (TimeIOC m) where
  alg hdl sig ctx = case sig of
    L GetNow -> (<$ ctx) <$> liftIO (System.Clock.getTime System.Clock.Monotonic)
    R other -> TimeIOC (alg (runTimeIO . hdl) other ctx)
