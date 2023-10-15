-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Structure recognizer: precomputation
module Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute (
  mkRecognizer,
  getEntityGrid,
) where

import Control.Arrow ((&&&))
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Semigroup (sconcat)
import Data.Tuple (swap)
import Swarm.Game.Entity (Entity)
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Universe (SubworldName)
import Swarm.Util (binTuples, histogram)
import Swarm.Util.Erasable (erasableToMaybe)
import Text.AhoCorasick

data RowPosition = RowPosition
  { _position :: Int32
  , structureRow :: StructureRow
  }

getEntityGrid :: NamedStructure (Maybe Cell) -> [[AtomicKeySymbol]]
getEntityGrid = map (map ((erasableToMaybe . cellEntity) =<<)) . area . structure

allStructureRows :: [StructureWithGrid] -> [StructureRow]
allStructureRows =
  concatMap getRows
 where
  getRows :: StructureWithGrid -> [StructureRow]
  getRows g = zipWith (StructureRow g) [0 ..] $ entityGrid g

mkOffsets :: Foldable f => Int32 -> f a -> InspectionOffsets
mkOffsets pos xs =
  InspectionOffsets (negate pos) $
    fromIntegral (length xs) - 1 - pos

-- TODO: This is an unnecessary abstraction/indirection
mkGenericLookup ::
  (Foldable t, Ord k, Hashable keySymb) =>
  p ->
  (a -> InspectionOffsets) ->
  (a -> ([keySymb], val)) ->
  (p -> t (k, a)) ->
  Map k (AutomatonInfo keySymb val)
mkGenericLookup gs extractBounds mkSmTuple preprocessAllRows =
  M.map mkValues structuresByRow
 where
  mkValues neList = AutomatonInfo bounds sm
   where
    bounds = sconcat $ NE.map extractBounds neList
    sm = makeStateMachine $ NE.toList $ NE.map mkSmTuple neList

  structuresByRow = binTuples $ preprocessAllRows gs

mkRowLookup ::
  [StructureRow] ->
  Map [AtomicKeySymbol] (AutomatonInfo [AtomicKeySymbol] StructureRow)
mkRowLookup gs =
  M.map mkValues structuresByRow
 where
  mkSmTuple = entityGrid . wholeStructure &&& id
  preprocessAllRows = map (\r@(StructureRow _ _ content) -> (content, r))

  deriveRowOffsets :: StructureRow -> InspectionOffsets
  deriveRowOffsets (StructureRow (StructureWithGrid _ g) rwIdx _) =
    mkOffsets rwIdx g

  mkValues neList = AutomatonInfo bounds sm
   where
    bounds = sconcat $ NE.map deriveRowOffsets neList
    sm = makeStateMachine $ NE.toList $ NE.map mkSmTuple neList

  structuresByRow = binTuples $ preprocessAllRows gs

-- | Make the first-phase lookup map, keyed by 'Entity',
-- along with automatons whose key symbols are "Maybe Entity".
mkEntityLookup ::
  Map [AtomicKeySymbol] (AutomatonInfo [AtomicKeySymbol] StructureRow) ->
  [StructureRow] ->
  Map Entity (AutomatonInfo AtomicKeySymbol StructureSearcher)
mkEntityLookup rowLookup gs =
  mkGenericLookup gs deriveEntityOffsets mkSmTuple preprocessAllRows
 where
  mkSmTuple :: RowPosition -> ([AtomicKeySymbol], StructureSearcher)
  mkSmTuple rp = (rowContent &&& StructureSearcher sm2D) . structureRow $ rp
   where
    -- TODO: Shouldn't have to do a map lookup here; create
    -- the association earlier and pass it through the input records.
    sm2D = rowLookup ! rowContent (structureRow rp)

  deriveEntityOffsets :: RowPosition -> InspectionOffsets
  deriveEntityOffsets (RowPosition pos r) =
    mkOffsets pos $ rowContent r

  preprocessAllRows = concatMap f
   where
    f :: StructureRow -> [(Entity, RowPosition)]
    f r@(StructureRow _ _ content) =
      map swap $
        catMaybes $
          zipWith (\idx -> fmap (RowPosition idx r,)) [0 :: Int32 ..] content

mkAutomatons :: InheritedStructureDefs -> RecognizerAutomatons
mkAutomatons xs =
  RecognizerAutomatons
    infos
    (mkEntityLookup rowLookup rowsAcrossAllStructures)
 where
  rowLookup = mkRowLookup rowsAcrossAllStructures

  infos = map process grids
  grids = map (\x -> StructureWithGrid x $ getEntityGrid x) xs
  rowsAcrossAllStructures = allStructureRows grids

  process g = StructureInfo g . histogram . concatMap catMaybes $ entityGrid g


-- TODO: Pre-populate toplevel placed structures
-- Each of these are re-checked in case a subsequent placement occludes them.
populateStaticFoundStructures = FoundStructures mempty mempty

mkRecognizer ::
  InheritedStructureDefs ->
  M.Map SubworldName [LocatedStructure (Maybe Cell)] ->
  StructureRecognizer
mkRecognizer xs placements =
  StructureRecognizer (mkAutomatons xs) fs []
 where
  -- TODO: Match definitions against the placements
  fs = populateStaticFoundStructures
