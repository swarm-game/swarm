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
-- Upon locating a complete structure, it is added to a registry
-- (see 'Swarm.Game.Scenario.Topography.Structure.Recognition.Registry.FoundRegistry'), which
-- supports lookups by either name or by location (using two different
-- maps maintained in parallel). The map by location is used to remove
-- a structure from the registry if a member entity is changed.
module Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute (
  -- * Main external interface
  mkAutomatons,

  -- * Helper functions
  populateStaticFoundStructures,
  getEntityGrid,
  extractGrids,
  lookupStaticPlacements,
) where

import Control.Arrow ((&&&))
import Control.Lens (view)
import Data.Int (Int32)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Semigroup (sconcat)
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Tuple (swap)
import Swarm.Game.Entity (Entity, entityName)
import Swarm.Game.Scenario (StaticStructureInfo (..))
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Placement (Orientation (..), applyOrientationTransform)
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Universe (Cosmic (..))
import Swarm.Language.Direction (AbsoluteDir)
import Swarm.Util (binTuples, histogram)
import Swarm.Util.Erasable (erasableToMaybe)
import Text.AhoCorasick

getEntityGrid :: Grid (Maybe Cell) -> [SymbolSequence]
getEntityGrid (Grid cells) = map (map ((erasableToMaybe . cellEntity) =<<)) cells

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

-- | Given each possible row of entities observed in the world,
-- yield a searcher that can determine whether adjacent
-- rows constitute a complete structure.
mkRowLookup ::
  NE.NonEmpty StructureRow ->
  AutomatonInfo SymbolSequence StructureWithGrid
mkRowLookup neList =
  AutomatonInfo participatingEnts bounds sm
 where
  mkSmTuple = entityGrid &&& id
  tuples = NE.toList $ NE.map (mkSmTuple . wholeStructure) neList

  -- All of the unique entities across all of the full candidate structures
  participatingEnts =
    S.fromList $
      map (view entityName) $
        concatMap (concatMap catMaybes . fst) tuples

  deriveRowOffsets :: StructureRow -> InspectionOffsets
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
  [StructureWithGrid] ->
  M.Map Entity (AutomatonInfo AtomicKeySymbol StructureSearcher)
mkEntityLookup grids =
  M.map mkValues rowsByEntityParticipation
 where
  rowsAcrossAllStructures = allStructureRows grids

  -- The input here are all rows across all structures
  -- that share the same entity sequence.
  mkSmValue :: SymbolSequence -> NE.NonEmpty SingleRowEntityOccurrences -> StructureSearcher
  mkSmValue ksms singleRows =
    StructureSearcher sm2D ksms singleRows
   where
    structureRowsNE = NE.map myRow singleRows
    sm2D = mkRowLookup structureRowsNE

  mkValues :: NE.NonEmpty SingleRowEntityOccurrences -> AutomatonInfo AtomicKeySymbol StructureSearcher
  mkValues neList = AutomatonInfo participatingEnts bounds sm
   where
    participatingEnts =
      S.fromList
        . map (view entityName)
        . catMaybes
        $ concatMap fst tuples

    tuples = M.toList $ M.mapWithKey mkSmValue groupedByUniqueRow

    groupedByUniqueRow = binTuples $ NE.toList $ NE.map (rowContent . myRow &&& id) neList
    bounds = sconcat $ NE.map expandedOffsets neList
    sm = makeStateMachine tuples

  -- The values of this map are guaranteed to contain only one
  -- entry per row of a given structure.
  rowsByEntityParticipation :: M.Map Entity (NE.NonEmpty SingleRowEntityOccurrences)
  rowsByEntityParticipation =
    binTuples $
      map (myEntity &&& id) $
        concatMap explodeRowEntities rowsAcrossAllStructures

  deriveEntityOffsets :: PositionWithinRow -> InspectionOffsets
  deriveEntityOffsets (PositionWithinRow pos r) =
    mkOffsets pos $ rowContent r

  -- The members of "rowMembers" are of 'Maybe' type; the 'Nothing's
  -- are dropped but accounted for when indexing the columns.
  explodeRowEntities :: StructureRow -> [SingleRowEntityOccurrences]
  explodeRowEntities r@(StructureRow _ _ rowMembers) =
    map f $ M.toList $ binTuples unconsolidated
   where
    f (e, occurrences) =
      SingleRowEntityOccurrences r e occurrences $
        sconcat $
          NE.map deriveEntityOffsets occurrences
    unconsolidated =
      map swap $
        catMaybes $
          zipWith (\idx -> fmap (PositionWithinRow idx r,)) [0 ..] rowMembers

-- | Create Aho-Corasick matchers that will recognize all of the
-- provided structure definitions
mkAutomatons :: [SymmetryAnnotatedGrid (Maybe Cell)] -> RecognizerAutomatons
mkAutomatons xs =
  RecognizerAutomatons
    infos
    (mkEntityLookup rotatedGrids)
 where
  rotatedGrids = concatMap (extractGrids . namedGrid) xs

  process g = StructureInfo g (getEntityGrid $ structure $ namedGrid g) . histogram . concatMap catMaybes . getEntityGrid . structure $ namedGrid g
  infos = M.fromList $ map (name . namedGrid &&& process) xs

extractOrientedGrid :: NamedGrid (Maybe Cell) -> AbsoluteDir -> StructureWithGrid
extractOrientedGrid x d = StructureWithGrid x d $ getEntityGrid g'
 where
  Grid rows = structure x
  g' = Grid $ applyOrientationTransform (Orientation d False) rows

-- | At this point, we have already ensured that orientations
-- redundant by rotational symmetry have been excluded
-- (i.e. at Scenario validation time).
extractGrids :: NamedGrid (Maybe Cell) -> [StructureWithGrid]
extractGrids x = map (extractOrientedGrid x) $ Set.toList $ recognize x

-- | The output list of 'FoundStructure' records is not yet
-- vetted; the 'ensureStructureIntact' function will subsequently
-- filter this list.
lookupStaticPlacements :: StaticStructureInfo -> [FoundStructure]
lookupStaticPlacements (StaticStructureInfo structDefs thePlacements) =
  concatMap f $ M.toList thePlacements
 where
  definitionMap = M.fromList $ map ((name &&& id) . namedGrid) structDefs

  f (subworldName, locatedList) = mapMaybe g locatedList
   where
    g (LocatedStructure theName d loc) = do
      sGrid <- M.lookup theName definitionMap
      return $ FoundStructure (extractOrientedGrid sGrid d) $ Cosmic subworldName loc
