-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Compress representation of traversable
--
-- For structures with many repeating elements,
-- will store the expanded element representation
-- in an array so that the structure can be transmitted
-- with array indices as elements.
module Swarm.Util.OccurrenceEncoder (
  Encoder,
  encodeOccurrence,
  getIndices,
  emptyEncoder,
) where

import Control.Monad.Trans.State
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as M

type OccurrenceEncoder a = State (Encoder a)

newtype Encoder a = Encoder (Map a Int)

emptyEncoder :: Ord a => Encoder a
emptyEncoder = Encoder mempty

-- | Map indices are guaranteed to be contiguous
-- from @[0..N]@, so we may convert to a list
-- with no loss of information.
getIndices :: Encoder a -> [a]
getIndices (Encoder m) = map fst $ sortOn snd $ M.toList m

-- | Translate each the first occurrence in the structure
-- to a new integer as it is encountered.
-- Subsequent encounters re-use the allocated integer.
encodeOccurrence :: Ord a => a -> OccurrenceEncoder a Int
encodeOccurrence c = do
  Encoder currentMap <- get
  maybe (cacheNewIndex currentMap) return $
    M.lookup c currentMap
 where
  cacheNewIndex currentMap = do
    put $ Encoder $ M.insert c newIdx currentMap
    return newIdx
   where
    newIdx = M.size currentMap
