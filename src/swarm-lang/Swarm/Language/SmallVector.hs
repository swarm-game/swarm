-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Strict small vector type to save on allocations, specifically for type parameters.
-- Most swarm functions will not have more than two parameters.
module Swarm.Language.SmallVector (
  SmallVector,
  toList,
  fromList,
) where

import Data.Data (Data)
import Data.Functor.Classes (Eq1 (..), Ord1 (liftCompare), Show1 (..))
import Data.Hashable (Hashable (..))
import Data.Hashable.Lifted (Hashable1 (..))
import Data.Vector.Instances ()
import Data.Vector.Strict (Vector)
import Data.Vector.Strict qualified as Vec
import GHC.Generics (Generic, Generic1)

data SmallVector a = Nil | One a | Two a a | Three a a a | Many (Vector a)
  deriving (Functor, Foldable, Traversable, Generic, Generic1, Data, Hashable, Hashable1)

toList :: SmallVector a -> [a]
toList = \case
  Nil -> []
  One a -> [a]
  Two a b -> [a, b]
  Three a b c -> [a, b, c]
  Many v -> Vec.toList v

fromList :: [a] -> SmallVector a
fromList = \case
  [] -> Nil
  [a] -> One a
  [a, b] -> Two a b
  [a, b, c] -> Three a b c
  l -> Many $ Vec.fromList l

instance Eq a => Eq (SmallVector a) where
  v1 == v2 = toList v1 == toList v2

instance Ord a => Ord (SmallVector a) where
  compare v1 v2 = compare (toList v1) (toList v2)

instance Show a => Show (SmallVector a) where
  show = show . toList

instance Eq1 SmallVector where
  liftEq p v1 v2 = liftEq p (toList v1) (toList v2)

instance Ord1 SmallVector where
  liftCompare p v1 v2 = liftCompare p (toList v1) (toList v2)

instance Show1 SmallVector where
  liftShowsPrec a b c v = liftShowsPrec a b c (toList v)
  liftShowList a b vs = liftShowList a b (map toList vs)
