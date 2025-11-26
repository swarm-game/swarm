{-# LANGUAGE BlockArguments #-}

-- |
-- SPDX-License-Identifier: ???
--
-- InternCache code by Chris Penner, https://chrispenner.ca/posts/intern-cache
module Swarm.Util.InternCache (
  InternCache,
  newInternCache,
  lookupCached,
  insertCached,
  intern,
  hoist,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import System.Mem.Weak
import UnliftIO.STM

-- | An 'InternCache' is parameterized by the monad in which it
--   operates, the key type, and the value type.
data InternCache m k v = InternCache
  { lookupCached :: k -> m (Maybe v)
  , insertCached :: k -> v -> m ()
  }

-- | Creates an 'InternCache' which uses weak references to only keep
--   values in the cache for as long as they're reachable by something
--   else.
--
--   This means you don't need to worry about a value not being GC'd
--   because it's in the cache.
newInternCache ::
  forall m k v.
  (MonadIO m, Hashable k) =>
  m (InternCache m k v)
newInternCache = do
  var <- newTVarIO mempty
  pure $
    InternCache
      { lookupCached = lookupCachedImpl var
      , insertCached = insertCachedImpl var
      }
 where
  lookupCachedImpl :: TVar (HashMap k (Weak v)) -> k -> m (Maybe v)
  lookupCachedImpl var ch = liftIO $ do
    cache <- readTVarIO var
    case HashMap.lookup ch cache of
      Nothing -> pure Nothing
      Just weakRef -> do
        deRefWeak weakRef

  insertCachedImpl :: TVar (HashMap k (Weak v)) -> k -> v -> m ()
  insertCachedImpl var k v = liftIO $ do
    wk <- mkWeakPtr v (Just $ removeDeadVal var k)
    atomically $ modifyTVar' var (HashMap.insert k wk)

  -- Use this as a finalizer to remove the key from the map
  -- when its value gets GC'd
  removeDeadVal :: TVar (HashMap k (Weak v)) -> k -> IO ()
  removeDeadVal var k = liftIO do
    atomically $ modifyTVar' var (HashMap.delete k)

-- | Changing the monad in which the cache operates with a natural transformation.
hoist :: (forall x. m x -> n x) -> InternCache m k v -> InternCache n k v
hoist f (InternCache lookup' insert') =
  InternCache
    { lookupCached = f . lookup'
    , insertCached = \k v -> f $ insert' k v
    }

-- | When a value is its own key, this ensures that the given value
-- is in the cache and always returns the single canonical in-memory
-- instance of that value, garbage collecting any others.
intern :: (Hashable k, Monad m) => InternCache m k k -> k -> m k
intern cache k = do
  mVal <- lookupCached cache k
  case mVal of
    Just v -> pure v
    Nothing -> do
      insertCached cache k k
      pure k
