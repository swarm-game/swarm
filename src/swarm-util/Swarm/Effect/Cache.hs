{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Description: Global cache effect wrapper around
--   "Swarm.Util.GlobalCache".  It uses IO under the hood, but with an
--   effect we can be specific about the fact that we are using the
--   cache and not doing arbitrary I/O.
module Swarm.Effect.Cache (
  Cache,
  lookup,

  -- ** Time Carrier
  CacheIOC,
  runCacheIO,
) where

import Control.Algebra (Algebra (..), Has, send, type (:+:) (..))
import Control.Carrier.Reader (ReaderC (..), ask, runReader)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Kind (Type)
import Swarm.Util.GlobalCache (GlobalCache (lookupCached))
import Prelude hiding (lookup)

-- | Global cache effect
data Cache key val (m :: Type -> Type) k where
  Lookup :: key -> Cache key val m (Maybe val)

-- | Look up a key in a global cache
lookup :: Has (Cache key val) sig m => key -> m (Maybe val)
lookup = send . Lookup
{-# INLINE lookup #-}

newtype CacheIOC key val m a = CacheIOC {runCacheIOC :: ReaderC (GlobalCache key val) m a}
  deriving newtype (Applicative, Functor, Monad, MonadIO)

runCacheIO :: GlobalCache key val -> CacheIOC key val m a -> m a
runCacheIO cache = runReader cache . runCacheIOC
{-# INLINE runCacheIO #-}

instance (MonadIO m, Algebra sig m) => Algebra (Cache key val :+: sig) (CacheIOC key val m) where
  alg hdl sig ctx = case sig of
    L (Lookup k) -> do
      cache <- CacheIOC (ask @(GlobalCache key val))
      (<$ ctx) <$> liftIO (lookupCached cache k)
    R other -> CacheIOC (alg (runCacheIOC . hdl) (R other) ctx)
  {-# INLINE alg #-}
