{-# LANGUAGE TypeFamilies #-}
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

  -- ** Time Handler
  runCacheIO,
) where

import Effectful
import Effectful.Dispatch.Dynamic
import Swarm.Util.GlobalCache (GlobalCache (lookupCached))
import Prelude hiding (lookup)

-- | Global cache effect
data Cache key val :: Effect where
  Lookup :: key -> Cache key val m (Maybe val)

type instance DispatchOf (Cache key val) = Dynamic

-- | Look up a key in a global cache
lookup :: (HasCallStack, Cache key val :> es) => key -> Eff es (Maybe val)
lookup = send . Lookup
{-# INLINE lookup #-}

runCacheIO :: (IOE :> es) => GlobalCache key val -> Eff (Cache key val : es) a -> Eff es a
runCacheIO cache = interpret $ \_ -> \case
  Lookup k -> liftIO (lookupCached cache k)
