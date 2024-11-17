-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Grid (
  Grid (..),
  NonEmptyGrid (..),
  gridToVec,
  zipNumberedNE,
  mapWithCoordsNE,
  mapWithCoords,
  allMembers,
  mapRowsNE,
  getRows,
  mkGrid,
)
where

import Data.Aeson (ToJSON (..))
import Data.Foldable qualified as F
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Swarm.Game.World.Coords
import Prelude hiding (zipWith)

newtype NonEmptyGrid c = NonEmptyGrid (NonEmpty (NonEmpty c))
  deriving (Generic, Show, Eq, Functor, Foldable, Traversable, ToJSON)

data Grid c
  = EmptyGrid
  | Grid (NonEmptyGrid c)
  deriving (Show, Eq, Functor, Foldable, Traversable)

mkGrid :: [[a]] -> Grid a
mkGrid rows = fromMaybe EmptyGrid $ do
  rowsNE <- NE.nonEmpty =<< mapM NE.nonEmpty rows
  return $ Grid $ NonEmptyGrid rowsNE

getRows :: Grid a -> [[a]]
getRows EmptyGrid = []
getRows (Grid (NonEmptyGrid g)) = NE.toList . NE.map NE.toList $ g

-- | Since the derived 'Functor' instance applies to the
-- type parameter that is nested within lists, we define
-- an explicit function for mapping over the enclosing lists.
mapRowsNE ::
  (NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty b)) ->
  NonEmptyGrid a ->
  NonEmptyGrid b
mapRowsNE f (NonEmptyGrid rows) = NonEmptyGrid $ f rows

allMembers :: Grid a -> [a]
allMembers EmptyGrid = []
allMembers g = F.toList g

nonemptyCount :: (Integral i) => NonEmpty i
nonemptyCount = NE.iterate succ 0

zipNumberedNE ::
  Integral i =>
  (i -> a -> b) ->
  NonEmpty a ->
  NonEmpty b
zipNumberedNE f = NE.zipWith f nonemptyCount

mapWithCoordsNE :: (Coords -> a -> b) -> NonEmptyGrid a -> NonEmpty b
mapWithCoordsNE f (NonEmptyGrid g) =
  sconcat $ NE.zipWith outer nonemptyCount g
 where
  outer i = zipNumberedNE $ \j -> f (Coords (i, j))

mapWithCoords :: (Coords -> a -> b) -> Grid a -> [b]
mapWithCoords _ EmptyGrid = []
mapWithCoords f (Grid g) = NE.toList $ mapWithCoordsNE f g

-- | Converts linked lists to vectors to facilitate
-- random access when assembling the image
gridToVec :: Grid a -> V.Vector (V.Vector a)
gridToVec EmptyGrid = V.empty
gridToVec (Grid (NonEmptyGrid g)) = V.fromList . map (V.fromList . NE.toList) $ NE.toList g

instance (ToJSON a) => ToJSON (Grid a) where
  toJSON EmptyGrid = toJSON ([] :: [a])
  toJSON (Grid g) = toJSON g
