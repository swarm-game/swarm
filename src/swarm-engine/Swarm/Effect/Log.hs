{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Log effects
module Swarm.Effect.Log (
    Log (..),

    -- ** Log Carrier
    LogIOC (..),
    runLogIOC,

    -- * Re-exports
    module Log,
) where
import Log
import Control.Algebra
import Control.Carrier.Reader
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Kind (Type)
import Data.Time.Clock
import Data.Text (Text)
import Data.Aeson.Types

-- | Effect for logging
data Log (m :: Type -> Type) k where
  LogMessageOp :: LogLevel -> Text -> Value -> Log m ()
  LocalData :: [Pair] -> m a -> Log m a
  LocalDomain :: Text -> m a -> Log m a
  LocalMaxLogLevel :: LogLevel -> m a -> Log m a
  GetLoggerEnv :: Log m LoggerEnv

newtype LogIOC m a = LogIOC (ReaderC LoggerEnv m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO)

{-
runReader :: r -> ReaderC r m a -> m a
runReader r (ReaderC runReaderC) = runReaderC r
-}

runLogIOC
    :: Text
    -> Logger
    -> LogLevel
    -> LogIOC m a
    -> m a
runLogIOC component logger maxLogLevel = internalRunLogIOC $ LoggerEnv
    { leLogger = logger
    , leComponent = component
    , leDomain = []
    , leData = []
    , leMaxLogLevel = maxLogLevel
    }

internalRunLogIOC :: LoggerEnv -> LogIOC m a -> m a
internalRunLogIOC env (LogIOC (ReaderC runLogIO)) = runLogIO env

{-
instance Algebra sig m => Algebra (Reader r :+: sig) (ReaderC r m) where
  alg Handler ctx n (ReaderC r m)
hdl sig ctx ()
ctx = ReaderC $ \ r -> case sig of
    L Ask         -> pure (r <$ ctx)
    L (Local f m) -> runReader (f r) (hdl (m <$ ctx))
    R other       -> alg (runReader r . hdl) other ctx
-}

instance (MonadIO m, Algebra sig m) => Algebra (Log :+: sig) (LogIOC m) where
  alg hdl sig ctx = LogIOC . ReaderC $ \env -> case sig of
    L (LogMessageOp lvl msg data_) -> (<$ ctx) <$> liftIO (
        getCurrentTime >>= \time -> logMessageIO env time lvl msg data_)
    L (LocalData data_ m) -> internalRunLogIOC (env {leData = leData env <> data_}) (hdl (m <$ ctx))
    L (LocalDomain domain m) -> internalRunLogIOC (env {leDomain = leDomain env <> [domain]}) (hdl (m <$ ctx))
    L (LocalMaxLogLevel lvl m) -> internalRunLogIOC (env {leMaxLogLevel = lvl}) (hdl (m <$ ctx))
    L GetLoggerEnv -> pure (env <$ ctx)
    R other -> alg (internalRunLogIOC env . hdl) other ctx

-- | Orphan, canonical instance.
instance forall sig m. (Has Log sig m, Monad m) => MonadLog m where
  logMessage level message data_ = send $ LogMessageOp level message data_
  localData data_ action = send $ LocalData data_ action
  localDomain domain action = send $ LocalDomain domain action
  localMaxLogLevel level action = send $ LocalMaxLogLevel level action
  getLoggerEnv = send GetLoggerEnv
