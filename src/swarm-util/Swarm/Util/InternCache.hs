-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Based on code by Chris Penner, https://chrispenner.ca/posts/intern-cache
-- Used by permission under a BSD 3-clause license.
module Swarm.Util.InternCache (
  InternCache,
  newInternCache,
  lookupCached,
  freezeCache,
  insertCached,
  deleteCached,
)
where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import UnliftIO.STM

-- | An 'InternCache' is a key-value map, parameterized by the monad
--   in which it operates, the key type, and the value type.
data InternCache k v = InternCache
  { lookupCached :: k -> IO (Maybe v)
  , freezeCache :: IO (k -> Maybe v)
  , insertCached :: k -> v -> IO ()
  , deleteCached :: k -> IO ()
  }

-- | Create a new (empty) InternCache.
newInternCache ::
  forall k v.
  (Hashable k, Show k) =>
  IO (InternCache k v)
newInternCache = do
  var <- newTVarIO mempty
  pure $
    InternCache
      { lookupCached = lookupCachedImpl var
      , freezeCache = freezeCacheImpl var
      , insertCached = insertCachedImpl var
      , deleteCached = deleteCachedImpl var
      }
 where
  -- XXX switch to stm-containers?
  -- https://www.parsonsmatt.org/2025/12/17/the_subtle_footgun_of_tvar_(map____).html
  lookupCachedImpl :: TVar (HashMap k v) -> k -> IO (Maybe v)
  lookupCachedImpl var k = HashMap.lookup k <$> readTVarIO var

  freezeCacheImpl :: TVar (HashMap k v) -> IO (k -> Maybe v)
  freezeCacheImpl var = flip HashMap.lookup <$> readTVarIO var

  insertCachedImpl :: TVar (HashMap k v) -> k -> v -> IO ()
  insertCachedImpl var k v = atomically $ modifyTVar' var (HashMap.insert k v)

  deleteCachedImpl :: TVar (HashMap k v) -> k -> IO ()
  deleteCachedImpl var k = atomically $ modifyTVar' var (HashMap.delete k)
