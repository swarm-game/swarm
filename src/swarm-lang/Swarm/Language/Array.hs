-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Arrays XXX
module Swarm.Language.Array (
  SwarmArray (..),
  fromList,
  toList,
  (!?),
) where

import Data.Aeson (ToJSON (..))
import Data.Array (Array)
import Data.Array qualified as A
import Data.Hashable
import GHC.Generics (Generic)

newtype SwarmArray a = SwarmArray {getSwarmArray :: Array Int a}
  deriving (Eq, Show, Generic)

fromList :: [a] -> SwarmArray a
fromList es = SwarmArray $ A.listArray (0, length es - 1) es

toList :: SwarmArray a -> [a]
toList = A.elems . getSwarmArray

instance (Eq a, Hashable a) => Hashable (SwarmArray a) where
  hashWithSalt s = hashWithSalt s . toList

instance ToJSON a => ToJSON (SwarmArray a) where
  toJSON = toJSON . toList

-- | Safe indexing.
(!?) :: SwarmArray a -> Int -> Maybe a
SwarmArray arr !? i
  | A.inRange (A.bounds arr) i = Just (arr A.! i)
  | otherwise = Nothing
