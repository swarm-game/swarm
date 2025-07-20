{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Log effects
module Swarm.Effect.Log (
    Log (..),

    -- ** Log functions
    logMessage,
    localData,
    localDomain,
    localMaxLogLevel,
    getLoggerEnv,
    logAttention,
    logInfo,
    logTrace,
    logAttention_,
    logInfo_,
    logTrace_,

    -- ** Log Carrier
    LogIOC (..),
    runLogIOC,
    runLogEnvIOC,

    -- * Re-exports
    module Log,
) where
import Log.Data as Log
import Log.Logger as Log
import Log as Log ((.=), object) 
import Log.Monad
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
runLogIOC component logger maxLogLevel = runLogEnvIOC $ LoggerEnv
    { leLogger = logger
    , leComponent = component
    , leDomain = []
    , leData = []
    , leMaxLogLevel = maxLogLevel
    }

runLogEnvIOC :: LoggerEnv -> LogIOC m a -> m a
runLogEnvIOC env (LogIOC (ReaderC runLogIO)) = runLogIO env

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
    L (LocalData data_ m) -> runLogEnvIOC (env {leData = leData env <> data_}) (hdl (m <$ ctx))
    L (LocalDomain domain m) -> runLogEnvIOC (env {leDomain = leDomain env <> [domain]}) (hdl (m <$ ctx))
    L (LocalMaxLogLevel lvl m) -> runLogEnvIOC (env {leMaxLogLevel = lvl}) (hdl (m <$ ctx))
    L GetLoggerEnv -> pure (env <$ ctx)
    R other -> alg (runLogEnvIOC env . hdl) other ctx

-- Redefine MonadLog with concrete type, to avoid type errors and incoherrent instances

logMessage :: (Has Log sig m, Monad m) => LogLevel -> Text -> Value -> m ()
logMessage level message data_ = send $ LogMessageOp level message data_

localData :: (Has Log sig m, Monad m) => [Pair] -> m a -> m a
localData data_ action = send $ LocalData data_ action

localDomain :: (Has Log sig m, Monad m) => Text -> m a -> m a
localDomain domain action = send $ LocalDomain domain action

localMaxLogLevel :: (Has Log sig m, Monad m) => LogLevel -> m a -> m a
localMaxLogLevel level action = send $ LocalMaxLogLevel level action

getLoggerEnv :: (Has Log sig m, Monad m) => m LoggerEnv
getLoggerEnv = send GetLoggerEnv

-- Log message helpers

logAttention :: (Has Log sig m, Monad m, ToJSON a) => Text -> a -> m ()
logAttention msg a = logMessage LogAttention msg (toJSON a)

logInfo :: (Has Log sig m, Monad m, ToJSON a) => Text -> a -> m ()
logInfo msg a = logMessage LogInfo msg (toJSON a)

logTrace :: (Has Log sig m, Monad m, ToJSON a) => Text -> a -> m ()
logTrace msg a = logMessage LogTrace msg (toJSON a)

-- Log message helpers - without value

logAttention_ :: (Has Log sig m, Monad m) => Text -> m ()
logAttention_ = (`logAttention` emptyObject)

logInfo_ :: (Has Log sig m, Monad m) => Text -> m ()
logInfo_ = (`logInfo` emptyObject)

logTrace_ :: (Has Log sig m, Monad m) => Text -> m ()
logTrace_ = (`logTrace` emptyObject)
