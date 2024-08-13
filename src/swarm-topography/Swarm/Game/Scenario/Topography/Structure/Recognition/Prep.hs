-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Structure.Recognition.Prep (mkEntityLookup) where

import Control.Arrow ((&&&))
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Swarm.Game.Scenario.Topography.Structure (NamedGrid)
import Data.Int (Int32)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Semigroup (sconcat)
import Data.Tuple (swap)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Text.AhoCorasick

allStructureRows :: [StructureWithGrid (NamedGrid b) a] -> [StructureRow b a]
allStructureRows =
  concatMap transformRows
 where
  transformRows :: StructureWithGrid (NamedGrid b) a -> [StructureRow b a]
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
  NE.NonEmpty (StructureRow b a) ->
  AutomatonInfo a (SymbolSequence a) (StructureWithGrid (NamedGrid b) a)
mkRowLookup neList =
  AutomatonInfo participatingEnts bounds sm
 where
  mkSmTuple = entityGrid &&& id
  tuples = NE.toList $ NE.map (mkSmTuple . wholeStructure) neList

  -- All of the unique entities across all of the full candidate structures
  participatingEnts =
    HS.fromList $
      concatMap (concatMap catMaybes . fst) tuples

  deriveRowOffsets :: StructureRow b a -> InspectionOffsets
  deriveRowOffsets (StructureRow (StructureWithGrid _ _ g) rwIdx _) =
    mkOffsets rwIdx g

  bounds = sconcat $ NE.map deriveRowOffsets neList
  sm = makeStateMachine tuples

-- | Make the first-phase lookup map, keyed by 'Entity',
-- along with automatons whose key symbols are "Maybe Entity".
--
-- Each automaton in this first layer will attempt to match the
-- underlying world row against all rows within all structures
-- (so long as they contain the keyed entity).
mkEntityLookup ::
  (Hashable a, Eq a) =>
  [StructureWithGrid (NamedGrid b) a] ->
  HM.HashMap a (AutomatonInfo a (AtomicKeySymbol a) (StructureSearcher b a))
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

  mkValues neList = AutomatonInfo participatingEnts bounds sm
   where
    participatingEnts =
      HS.fromList
        (concatMap (catMaybes . fst) tuples)

    tuples = HM.toList $ HM.mapWithKey mkSmValue groupedByUniqueRow

    groupedByUniqueRow = binTuplesHM $ NE.toList $ NE.map (rowContent . myRow &&& id) neList
    bounds = sconcat $ NE.map expandedOffsets neList
    sm = makeStateMachine tuples

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
  HM.HashMap a (NE.NonEmpty b)
binTuplesHM = foldr f mempty
 where
  f = uncurry (HM.insertWith (<>)) . fmap pure
