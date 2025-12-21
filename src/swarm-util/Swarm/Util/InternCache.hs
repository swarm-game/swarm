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
  intern,
  hoist,
)
where

import Debug.Trace

import Control.Algebra (Has)
import Control.Effect.Lift (Lift, sendIO)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import System.Mem.Weak
import UnliftIO.STM

-- | An 'InternCache' is a key-value map, but uses weak references to
--   only keep values in the cache for as long as they're reachable by
--   something else.  In particular, this means you don't need to
--   worry about a value not being GC'd just because it's in the
--   cache.
--
--   An 'InternCache' is parameterized by the monad in which it
--   operates, the key type, and the value type.
data InternCache m k v = InternCache
  { lookupCached :: k -> m (Maybe v)
  , freezeCache :: m (HashMap k v)
  , insertCached :: k -> v -> m ()
  , cachedKeysSet :: m (HashSet k)
  }

-- | Create a new (empty) InternCache.
newInternCache ::
  forall sig m k v.
  (Has (Lift IO) sig m, Hashable k, Show k) =>
  m (InternCache m k v)
newInternCache = do
  var <- sendIO $ newTVarIO mempty
  pure $
    InternCache
      { lookupCached = lookupCachedImpl var
      , freezeCache = freezeCacheImpl var
      , insertCached = insertCachedImpl var
      , cachedKeysSet = cachedKeysSetImpl var
      }
 where
  cachedKeysSetImpl :: TVar (HashMap k (Weak v)) -> m (HashSet k)
  cachedKeysSetImpl var = sendIO $ do
    cache <- readTVarIO var
    pure $ HashMap.keysSet cache

  lookupCachedImpl :: TVar (HashMap k (Weak v)) -> k -> m (Maybe v)
  lookupCachedImpl var ch = sendIO $ do
    cache <- readTVarIO var
    traceM $ "looking up " ++ show ch
    case HashMap.lookup ch cache of
      Nothing -> pure Nothing
      Just weakRef -> do
        traceM "hit!"
        deRefWeak weakRef

  freezeCacheImpl :: TVar (HashMap k (Weak v)) -> m (HashMap k v)
  freezeCacheImpl var = sendIO $ do
    cache <- readTVarIO var
    traverseMaybe deRefWeak cache

  insertCachedImpl :: TVar (HashMap k (Weak v)) -> k -> v -> m ()
  insertCachedImpl var k v = sendIO $ do
    traceM $ "inserting " ++ show k
    wk <- mkWeakPtr v (Just $ removeDeadVal var k)
    atomically $ modifyTVar' var (HashMap.insert k wk)

  -- Use this as a finalizer to remove the key from the map
  -- when its value gets GC'd
  removeDeadVal :: TVar (HashMap k (Weak v)) -> k -> IO ()
  removeDeadVal var k = sendIO $ do
    traceM $ "purging " ++ show k
    atomically $ modifyTVar' var (HashMap.delete k)

-- | Changing the monad in which the cache operates with a natural transformation.
hoist :: (forall x. m x -> n x) -> InternCache m k v -> InternCache n k v
hoist f (InternCache lookup' freeze' insert' keys') =
  InternCache
    { lookupCached = f . lookup'
    , freezeCache = f freeze'
    , insertCached = \k v -> f $ insert' k v
    , cachedKeysSet = f keys'
    }

-- | When a value is its own key, this ensures that the given value is
--   in the cache and always returns the single canonical in-memory
--   instance of that value, garbage collecting any others.
intern :: (Hashable k, Monad m) => InternCache m k k -> k -> m k
intern cache k = do
  mVal <- lookupCached cache k
  case mVal of
    Just v -> pure v
    Nothing -> do
      insertCached cache k k
      pure k

traverseMaybe :: Applicative f => (a -> f (Maybe b)) -> HashMap k a -> f (HashMap k b)
traverseMaybe g m = HashMap.mapMaybe id <$> traverse g m
