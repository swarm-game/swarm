{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Ordered map utilities
module Swarm.Util.OrderedMap where

import Control.Lens (Index, IxValue, Ixed (..), (<&>))
import Data.Either (partitionEithers)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OM
import Swarm.Util (lookupEither)

-- | Given an ordered list of keys and a map, return a partition consisting of:
-- * Left: the keys that were not present
-- * Right: the retrievable key-value pairs in corresponding order to the provided keys
lookupInOrder :: Ord k => Map k v -> [k] -> ([k], [(k, v)])
lookupInOrder m = partitionEithers . map produceKeyValuePair
 where
  produceKeyValuePair k = sequenceA (k, lookupEither k m)

type instance Index (OMap k a) = k
type instance IxValue (OMap k a) = a

-- | Adapted from:
-- <https://hackage.haskell.org/package/lens-5.3.4/docs/src/Control.Lens.At.html#line-319>
instance Ord k => Ixed (OMap k a) where
  ix k f m = case OM.lookup k m of
    Just v -> f v <&> \v' -> OM.alter (const $ Just v') k m
    Nothing -> pure m

-- | Strangely, an 'elems' function is missing from the 'OMap' API.
elems :: OMap k a -> [a]
elems = map snd . OM.assocs

fromMap :: Ord k => Map k a -> OMap k a
fromMap = OM.fromList . M.toList
