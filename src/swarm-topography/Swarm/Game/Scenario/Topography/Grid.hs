-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Grid (
  Grid (..),
  NonEmptyGrid (..),
  gridToVec,
  mapWithCoords,
  mapWithCoordsNE,
  allMembers,
  allMembersNE,
  mapRowsNE,
  getRows,
  mkGrid,
  getNonEmptyGrid,
)
where

import Control.Lens.Indexed (FunctorWithIndex, imap)
import Data.Aeson (ToJSON (..))
import Data.Foldable qualified as F
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Swarm.Game.World.Coords
import Prelude hiding (zipWith)

newtype NonEmptyGrid c = NonEmptyGrid (NonEmpty (NonEmpty c))
  deriving (Generic, Show, Eq, Functor, Foldable, Traversable, ToJSON)

instance FunctorWithIndex Coords NonEmptyGrid where
  imap f (NonEmptyGrid g) =
    NonEmptyGrid $
      imap (\i -> imap (\j -> f (Coords $ both fromIntegral (i, j)))) g

data Grid c
  = EmptyGrid
  | Grid (NonEmptyGrid c)
  deriving (Show, Eq, Functor, Foldable, Traversable)

mkGrid :: [[a]] -> Grid a
mkGrid rows = fromMaybe EmptyGrid $ do
  rowsNE <- NE.nonEmpty =<< mapM NE.nonEmpty rows
  return $ Grid $ NonEmptyGrid rowsNE

getNonEmptyGrid :: Grid a -> Maybe (NonEmptyGrid a)
getNonEmptyGrid = \case
  EmptyGrid -> Nothing
  Grid x -> Just x

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

allMembersNE :: NonEmptyGrid c -> NonEmpty c
allMembersNE (NonEmptyGrid g) = sconcat g

allMembers :: Grid a -> [a]
allMembers EmptyGrid = []
allMembers g = F.toList g

mapWithCoordsNE :: (Coords -> a -> b) -> NonEmptyGrid a -> NonEmpty b
mapWithCoordsNE f = allMembersNE . imap f

mapWithCoords :: (Coords -> a -> b) -> Grid a -> [b]
mapWithCoords _ EmptyGrid = []
mapWithCoords f (Grid g) = NE.toList $ mapWithCoordsNE f g

-- | Converts linked lists to vectors to facilitate
-- random access when assembling the image
gridToVec :: Grid a -> V.Vector (V.Vector a)
gridToVec g = V.fromList . map V.fromList $ getRows g

instance (ToJSON a) => ToJSON (Grid a) where
  toJSON EmptyGrid = toJSON ([] :: [a])
  toJSON (Grid g) = toJSON g
