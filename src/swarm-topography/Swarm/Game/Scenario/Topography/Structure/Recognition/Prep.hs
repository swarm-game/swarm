-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Structure.Recognition.Prep (mkEntityLookup) where

import Control.Arrow ((&&&))
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Semigroup (sconcat)
import Data.Tuple (swap)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Text.AhoCorasick

allStructureRows :: [StructureWithGrid b a] -> [StructureRow b a]
allStructureRows =
  concatMap transformRows
 where
  transformRows :: StructureWithGrid b a -> [StructureRow b a]
  transformRows g = zipWith (StructureRow g) [0 ..] $ entityGrid g

mkOffsets :: Foldable f => Int32 -> f a -> InspectionOffsets
mkOffsets pos xs =
  InspectionOffsets (pure (negate pos)) $
    pure $
      fromIntegral (length xs) - 1 - pos

-- | Given each possible row of entities observed in the world,
-- yield a searcher that can determine whether adjacent
-- rows constitute a complete structure.
mkRowLookup ::
  (Hashable a, Eq a) =>
  NonEmpty (StructureRow b a) ->
  AutomatonInfo a (SymbolSequence a) (StructureWithGrid b a)
mkRowLookup neList =
  AutomatonInfo participatingEnts bounds sm tuples
 where
  mkSmTuple = entityGrid &&& id
  tuples = NE.map (mkSmTuple . wholeStructure) neList

  -- All of the unique entities across all of the full candidate structures
  participatingEnts =
    HS.fromList $
      concatMap (concatMap catMaybes . fst) tuples

  deriveRowOffsets :: StructureRow b a -> InspectionOffsets
  deriveRowOffsets (StructureRow (StructureWithGrid _ _ g) rwIdx _) =
    mkOffsets rwIdx g

  bounds = sconcat $ NE.map deriveRowOffsets neList
  sm = makeStateMachine $ NE.toList tuples

-- | Make the first-phase lookup map, keyed by 'Entity',
-- along with automatons whose key symbols are "Maybe Entity".
--
-- Each automaton in this first layer will attempt to match the
-- underlying world row against all rows within all structures
-- (so long as they contain the keyed entity).
mkEntityLookup ::
  (Hashable a, Eq a) =>
  [StructureWithGrid b a] ->
  HM.HashMap a (NonEmpty (AutomatonInfo a (AtomicKeySymbol a) (StructureSearcher b a)))
mkEntityLookup grids =
  HM.map mkValues rowsByEntityParticipation
 where
  rowsAcrossAllStructures = allStructureRows grids

  -- The input here are all rows across all structures
  -- that share the same entity sequence.
  mkSmValue ksms singleRows =
    StructureSearcher sm2D ksms singleRows
   where
    structureRowsNE = NE.map myRow singleRows
    sm2D = mkRowLookup structureRowsNE

  mkValues neList =
    NE.map (\(mask, tups) -> AutomatonInfo mask bounds sm tups) tuplesByEntMask
   where
    -- If there are no transparent cells,
    -- we don't need a mask.
    getMaskSet row =
      if Nothing `elem` row
        then HS.fromList $ catMaybes row
        else mempty

    tuplesByEntMask = binTuplesHMasListNE $ NE.map (getMaskSet . fst &&& id) tuplesNE

    tuplesNE = NE.map (\(a, b) -> (a, mkSmValue a b)) groupedByUniqueRow

    groupedByUniqueRow =
      binTuplesHMasListNE $
        NE.map (rowContent . myRow &&& id) neList

    bounds = sconcat $ NE.map expandedOffsets neList
    sm = makeStateMachine $ NE.toList tuplesNE

  -- The values of this map are guaranteed to contain only one
  -- entry per row of a given structure.
  rowsByEntityParticipation =
    binTuplesHM $
      map (myEntity &&& id) $
        concatMap explodeRowEntities rowsAcrossAllStructures

  deriveEntityOffsets :: PositionWithinRow b a -> InspectionOffsets
  deriveEntityOffsets (PositionWithinRow pos r) =
    mkOffsets pos $ rowContent r

  -- The members of "rowMembers" are of 'Maybe' type; the 'Nothing's
  -- are dropped but accounted for when indexing the columns.
  explodeRowEntities ::
    (Hashable a, Eq a) =>
    StructureRow b a ->
    [SingleRowEntityOccurrences b a]
  explodeRowEntities r@(StructureRow _ _ rowMembers) =
    map f $ HM.toList $ binTuplesHM unconsolidated
   where
    f (e, occurrences) =
      SingleRowEntityOccurrences r e occurrences $
        sconcat $
          NE.map deriveEntityOffsets occurrences

    unconsolidated =
      map swap $
        catMaybes $
          zipWith (\idx -> fmap (PositionWithinRow idx r,)) [0 ..] rowMembers

-- * Util

-- | Place the second element of the tuples into bins by
-- the value of the first element.
binTuplesHM ::
  (Foldable t, Hashable a, Eq a) =>
  t (a, b) ->
  HM.HashMap a (NonEmpty b)
binTuplesHM = foldr f mempty
 where
  f = uncurry (HM.insertWith (<>)) . fmap pure

-- | We know that if the input to the binning function
-- is a nonempty list, the output map must also have
-- at least one element.
-- Ideally we would use a NonEmptyMap to prove this,
-- but unfortunately such a variant does not exist for 'HashMap'.
-- So we just "force" the proof by using 'NE.fromList'.
binTuplesHMasListNE ::
  (Hashable a, Eq a) =>
  NonEmpty (a, b) ->
  NonEmpty (a, NonEmpty b)
binTuplesHMasListNE = NE.fromList . HM.toList . binTuplesHM
