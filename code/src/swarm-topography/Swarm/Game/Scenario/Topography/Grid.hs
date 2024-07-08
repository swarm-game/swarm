-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Grid (
  Grid (..),
  gridToVec,
  mapIndexedMembers,
  allMembers,
  mapRows,
  getRows,
  mkGrid,
)
where

import Data.Aeson (ToJSON (..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.Vector qualified as V
import Swarm.Game.World.Coords
import Prelude hiding (zipWith)

data Grid c
  = EmptyGrid
  | Grid (NonEmpty (NonEmpty c))
  deriving (Show, Eq, Functor, Foldable, Traversable)

mkGrid :: [[a]] -> Grid a
mkGrid rows = fromMaybe EmptyGrid $ do
  rowsNE <- NE.nonEmpty =<< mapM NE.nonEmpty rows
  return $ Grid rowsNE

getRows :: Grid a -> [[a]]
getRows EmptyGrid = []
getRows (Grid g) = NE.toList . NE.map NE.toList $ g

-- | Since the derived 'Functor' instance applies to the
-- type parameter that is nested within lists, we define
-- an explicit function for mapping over the enclosing lists.
mapRows :: (NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty b)) -> Grid a -> Grid b
mapRows _ EmptyGrid = EmptyGrid
mapRows f (Grid rows) = Grid $ f rows

allMembers :: Grid a -> [a]
allMembers EmptyGrid = []
allMembers g = concat . getRows $ g

mapIndexedMembers :: (Coords -> a -> b) -> Grid a -> [b]
mapIndexedMembers _ EmptyGrid = []
mapIndexedMembers f (Grid g) =
  NE.toList $
    sconcat $
      NE.zipWith (\i -> NE.zipWith (\j -> f (Coords (i, j))) nonemptyCount) nonemptyCount g
 where
  nonemptyCount = NE.iterate succ 0

-- | Converts linked lists to vectors to facilitate
-- random access when assembling the image
gridToVec :: Grid a -> V.Vector (V.Vector a)
gridToVec EmptyGrid = V.empty
gridToVec (Grid g) = V.fromList . map (V.fromList . NE.toList) $ NE.toList g

instance (ToJSON a) => ToJSON (Grid a) where
  toJSON EmptyGrid = toJSON ([] :: [a])
  toJSON (Grid g) = toJSON g
