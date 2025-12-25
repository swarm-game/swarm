-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Initially based on code by Chris Penner, https://chrispenner.ca/posts/intern-cache
-- Used by permission under a BSD 3-clause license.
module Swarm.Util.GlobalCache (
  GlobalCache,
  newGlobalCache,
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

-- | An 'GlobalCache' is a key-value map, parameterized by the monad
--   in which it operates, the key type, and the value type.
data GlobalCache k v = GlobalCache
  { lookupCached :: k -> IO (Maybe v)
  , freezeCache :: IO (k -> Maybe v)
  , insertCached :: k -> v -> IO ()
  , deleteCached :: k -> IO ()
  }

-- | Create a new (empty) GlobalCache.
newGlobalCache ::
  forall k v.
  (Hashable k, Show k) =>
  IO (GlobalCache k v)
newGlobalCache = do
  -- TODO (#2669): switch to stm-containers?
  var <- newTVarIO mempty
  pure $
    GlobalCache
      { lookupCached = lookupCachedImpl var
      , freezeCache = freezeCacheImpl var
      , insertCached = insertCachedImpl var
      , deleteCached = deleteCachedImpl var
      }
 where
  lookupCachedImpl :: TVar (HashMap k v) -> k -> IO (Maybe v)
  lookupCachedImpl var k = HashMap.lookup k <$> readTVarIO var

  freezeCacheImpl :: TVar (HashMap k v) -> IO (k -> Maybe v)
  freezeCacheImpl var = flip HashMap.lookup <$> readTVarIO var

  insertCachedImpl :: TVar (HashMap k v) -> k -> v -> IO ()
  insertCachedImpl var k v = atomically $ modifyTVar' var (HashMap.insert k v)

  deleteCachedImpl :: TVar (HashMap k v) -> k -> IO ()
  deleteCachedImpl var k = atomically $ modifyTVar' var (HashMap.delete k)
