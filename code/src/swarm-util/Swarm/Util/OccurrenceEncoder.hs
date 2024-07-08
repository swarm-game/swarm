-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Compress representation of traversable
--
-- Useful for compressing the representation of a
-- structure that has many repeating elements
-- for transmission (e.g. over the network).
module Swarm.Util.OccurrenceEncoder (
  runEncoder,
) where

import Control.Monad.Trans.State
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as M

type OccurrenceEncoder a = State (Encoder a)

newtype Encoder a = Encoder (Map a Int)

-- |
-- Given a data structure that may have many repeating "complex" elements,
-- will store the "complex" element representation
-- in an array so that the structure's elements can be replaced
-- with simple indices into that array.
--
-- The first encountered element is assigned index 0, and the next
-- novel element encountered gets index 1, and so on.
runEncoder ::
  (Traversable t, Ord b) =>
  t b ->
  (t Int, [b])
runEncoder structure =
  getIndices <$> runState (mapM encodeOccurrence structure) emptyEncoder

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
