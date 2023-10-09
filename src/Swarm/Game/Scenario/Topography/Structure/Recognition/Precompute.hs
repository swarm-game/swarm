-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Precomputation for structure recognizer.
--
-- = Search process overview
--
-- 2D structures may be defined at the
-- <https://github.com/swarm-game/swarm/blob/main/data/scenarios/doc-fragments/SCHEMA.md#top-level toplevel of a scenario file>.
-- Upon scenario load, all of the predefined structures that are marked
-- as @"recognize"@ are compiled into searcher state machines.
--
-- When an entity is placed on any cell in the world, the
-- 'Swarm.Game.Scenario.Topography.Structure.Recognition.Tracking.entityModified'
-- function is called, which looks up a customized searcher based
-- on the type of placed entity.
--
-- The first searching stage looks for any member row of all participating
-- structure definitions that contains the placed entity.
-- The value returned by the searcher is a second-stage searcher state machine,
-- which this time searches for complete structures of which the found row may
-- be a member.
--
-- Both the first stage and second stage searcher know to start the search
-- at a certain offset horizontally or vertically from the placed entity,
-- based on where within a structure that entity (or row) may occur.
--
-- Upon locating a complete structure, it is added to a registry, which
-- supports lookups by either name or by location (using two different
-- maps maintained in parallel). The map by location is used to remove
-- a structure from the registry if a member entity is changed.
module Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute (
  -- * Main external interface
  mkAutomatons,

  -- * Helper functions
  populateStaticFoundStructures,
  placedToFound,
  getEntityGrid,
) where

import Control.Arrow ((&&&))
import Data.Int (Int32)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Semigroup (sconcat)
import Data.Tuple (swap)
import Swarm.Game.Entity (Entity)
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Universe (Cosmic (..), SubworldName)
import Swarm.Util (binTuples, histogram)
import Swarm.Util.Erasable (erasableToMaybe)
import Text.AhoCorasick

getEntityGrid :: NamedStructure (Maybe Cell) -> [SymbolSequence]
getEntityGrid = map (map ((erasableToMaybe . cellEntity) =<<)) . area . structure

allStructureRows :: [StructureWithGrid] -> [StructureRow]
allStructureRows =
  concatMap getRows
 where
  getRows :: StructureWithGrid -> [StructureRow]
  getRows g = zipWith (StructureRow g) [0 ..] $ entityGrid g

mkOffsets :: Foldable f => Int32 -> f a -> InspectionOffsets
mkOffsets pos xs =
  InspectionOffsets (pure (negate pos)) $
    pure $
      fromIntegral (length xs) - 1 - pos

-- | Given a row of entities observed in the world,
-- yield a searcher that can determine of adjacent
-- rows constitute a complete structure.
mkRowLookup ::
  NE.NonEmpty StructureRow ->
  AutomatonInfo SymbolSequence StructureRow
mkRowLookup neList =
  AutomatonInfo bounds sm
 where
  mkSmTuple = entityGrid . wholeStructure &&& id

  deriveRowOffsets :: StructureRow -> InspectionOffsets
  deriveRowOffsets (StructureRow (StructureWithGrid _ g) rwIdx _) =
    mkOffsets rwIdx g

  bounds = sconcat $ NE.map deriveRowOffsets neList
  sm = makeStateMachine $ NE.toList $ NE.map mkSmTuple neList

-- | Make the first-phase lookup map, keyed by 'Entity',
-- along with automatons whose key symbols are "Maybe Entity".
--
-- Each automaton in this first layer will attempt to match the
-- underlying world row against all rows within all structures
-- (so long as they contain the keyed entity).
mkEntityLookup ::
  [StructureWithGrid] ->
  M.Map Entity (AutomatonInfo AtomicKeySymbol StructureSearcher)
mkEntityLookup grids =
  M.map mkValues rowsByEntityParticipation
 where
  rowsAcrossAllStructures = allStructureRows grids

  -- The input here are all rows across all structures
  -- that share the same entity sequence.
  mkSmValue :: SymbolSequence -> NE.NonEmpty SingleRowEntityRecurrences -> StructureSearcher
  mkSmValue ksms singleRows =
    StructureSearcher sm2D ksms singleRows
   where
    structureRowsNE = NE.map myRow singleRows
    sm2D = mkRowLookup structureRowsNE

  mkValues :: NE.NonEmpty SingleRowEntityRecurrences -> AutomatonInfo AtomicKeySymbol StructureSearcher
  mkValues neList = AutomatonInfo bounds sm
   where
    groupedByUniqueRow = binTuples $ NE.toList $ NE.map (rowContent . myRow &&& id) neList
    bounds = sconcat $ NE.map expandedOffsets neList
    sm = makeStateMachine $ M.toList $ M.mapWithKey mkSmValue groupedByUniqueRow

  -- The values of this map are guaranteed to contain only one
  -- entry per row of a given structure.
  rowsByEntityParticipation :: M.Map Entity (NE.NonEmpty SingleRowEntityRecurrences)
  rowsByEntityParticipation =
    binTuples $
      map (myEntity &&& id) $
        concatMap explodeRowEntities rowsAcrossAllStructures

  deriveEntityOffsets :: PositionWithinRow -> InspectionOffsets
  deriveEntityOffsets (PositionWithinRow pos r) =
    mkOffsets pos $ rowContent r

  -- The members of "rowMembers" are of 'Maybe' type; the 'Nothing's
  -- are dropped but accounted for when indexing the columns.
  explodeRowEntities :: StructureRow -> [SingleRowEntityRecurrences]
  explodeRowEntities r@(StructureRow _ _ rowMembers) =
    map f $ M.toList $ binTuples unconsolidated
   where
    f (e, occurrences) =
      SingleRowEntityRecurrences r e occurrences $
        sconcat $
          NE.map deriveEntityOffsets occurrences
    unconsolidated =
      map swap $
        catMaybes $
          zipWith (\idx -> fmap (PositionWithinRow idx r,)) [0 ..] rowMembers

mkAutomatons :: InheritedStructureDefs -> RecognizerAutomatons
mkAutomatons xs =
  RecognizerAutomatons
    infos
    (mkEntityLookup grids)
 where
  grids = map extractGrid xs

  process g = StructureInfo g . histogram . concatMap catMaybes $ entityGrid g
  infos = map process grids

extractGrid :: NamedStructure (Maybe Cell) -> StructureWithGrid
extractGrid x = StructureWithGrid x $ getEntityGrid x

placedToFound :: SubworldName -> LocatedStructure (Maybe Cell) -> FoundStructure
placedToFound swName (LocatedStructure (Placed _ ns) loc) =
  FoundStructure (extractGrid ns) $ Cosmic swName loc
