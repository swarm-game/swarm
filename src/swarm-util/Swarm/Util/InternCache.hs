-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Based on code by Chris Penner, https://chrispenner.ca/posts/intern-cache
-- Used by permission under a BSD 3-clause license.
module Swarm.Util.InternCache (
  InternCache,
  newInternCache,
  lookupCached,
  insertCached,
  intern,
  hoist,
  fetchCached,
)
where

import Debug.Trace

import Control.Algebra (Has)
import Control.Effect.Lift (Lift, sendIO)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
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
  , insertCached :: k -> v -> m ()
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
      , insertCached = insertCachedImpl var
      }
 where
  lookupCachedImpl :: TVar (HashMap k (Weak v)) -> k -> m (Maybe v)
  lookupCachedImpl var ch = sendIO $ do
    cache <- readTVarIO var
    case HashMap.lookup ch cache of
      Nothing -> pure Nothing
      Just weakRef -> do
        deRefWeak weakRef

  insertCachedImpl :: TVar (HashMap k (Weak v)) -> k -> v -> m ()
  insertCachedImpl var k v = sendIO $ do
    wk <- mkWeakPtr v (Just $ removeDeadVal var k)
    atomically $ modifyTVar' var (HashMap.insert k wk)

  -- Use this as a finalizer to remove the key from the map
  -- when its value gets GC'd
  removeDeadVal :: TVar (HashMap k (Weak v)) -> k -> IO ()
  removeDeadVal var k = sendIO $ do
    atomically $ modifyTVar' var (HashMap.delete k)

-- | Changing the monad in which the cache operates with a natural transformation.
hoist :: (forall x. m x -> n x) -> InternCache m k v -> InternCache n k v
hoist f (InternCache lookup' insert') =
  InternCache
    { lookupCached = f . lookup'
    , insertCached = \k v -> f $ insert' k v
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

-- | Given a key, return the corresponding value from the cache, if
--   any; or, if the key is not yet in the cache, run the given action
--   to compute the corresponding value, cache it, and return it.
fetchCached :: (Hashable k, Monad m) => InternCache m k v -> (k -> m v) -> k -> m v
fetchCached cache fetch k = do
  mVal <- lookupCached cache k
  case mVal of
    Just v -> do
      traceM "cache hit!"
      pure v
    Nothing -> do
      traceM "fetching!"
      v <- fetch k
      insertCached cache k v
      pure v

-- -- | Like 'fetchCached', but with an extra function that determines
-- --   whether a given key/value pair is outdated. Re-run the fetch
-- --   action whenever the value stored in the cache for the given key
-- --   is outdated.
-- updateCached :: (Hashable k, Monad m) => InternCache m k v -> (k -> m v) -> (k -> v -> m Bool) -> k -> m v
-- updateCached cache fetch outdated k = do
--   v <- fetchCached cache fetch k
--   out <- outdated k v
--   if out
--     then do
--       v' <- fetch k
--       insertCached cache k v'
--       pure v'
--     else pure v
