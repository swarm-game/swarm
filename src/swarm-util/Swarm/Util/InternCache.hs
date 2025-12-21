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
  cachedKeysSet,
)
where

import Debug.Trace

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import UnliftIO.STM

-- | An 'InternCache' is a key-value map, parameterized by the monad
--   in which it operates, the key type, and the value type.
data InternCache k v = InternCache
  { lookupCached :: k -> IO (Maybe v)
  , freezeCache :: IO (HashMap k v)
  , insertCached :: k -> v -> IO ()
  , cachedKeysSet :: IO (HashSet k)
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
      , cachedKeysSet = cachedKeysSetImpl var
      }
 where
  cachedKeysSetImpl :: TVar (HashMap k v) -> IO (HashSet k)
  cachedKeysSetImpl var = do
    cache <- readTVarIO var
    pure $ HashMap.keysSet cache

  lookupCachedImpl :: TVar (HashMap k v) -> k -> IO (Maybe v)
  lookupCachedImpl var ch = do
    cache <- readTVarIO var
    traceM $ "looking up " ++ show ch
    case HashMap.lookup ch cache of
      Nothing -> pure Nothing
      Just v -> traceM "hit!" >> pure (Just v)

  freezeCacheImpl :: TVar (HashMap k v) -> IO (HashMap k v)
  freezeCacheImpl = readTVarIO

  insertCachedImpl :: TVar (HashMap k v) -> k -> v -> IO ()
  insertCachedImpl var k v = do
    traceM $ "inserting " ++ show k
    atomically $ modifyTVar' var (HashMap.insert k v)
