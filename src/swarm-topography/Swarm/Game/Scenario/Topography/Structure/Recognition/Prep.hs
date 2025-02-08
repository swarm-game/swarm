-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Structure.Recognition.Prep (
  mkEntityLookup,
  binTuplesHM,
) where

import Control.Arrow ((&&&))
import Control.Lens.Indexed (imap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.List.Split (wordsBy)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Semigroup (sconcat)
import Data.Tuple (swap)
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Text.AhoCorasick (makeStateMachine)

-- | Given all candidate structures, explode them into annotated rows.
-- These annotations entail both the row index with the original structure
-- and a reference to the original structure definition.
--
-- This operation may result in multiple entries that contain the same contents
-- (but different annotations), either because the same contents appear
-- in multiple rows within the same structure, or occur across structures.
allStructureRows :: [StructureWithGrid b a] -> [StructureRow b a]
allStructureRows =
  concatMap $ NE.toList . transformRows
 where
  transformRows g = imap (StructureRow g . fromIntegral) rows
   where
    NonEmptyGrid rows = extractedGrid $ entityGrid g

-- | If this entity is encountered in the world,
-- how far left of it and how far right of it do we need to
-- scan the world row to ensure we can recognize every possible
-- structure that features this entity?
mkOffsets :: Int32 -> RowWidth -> InspectionOffsets
mkOffsets pos (RowWidth w) =
  InspectionOffsets
    (subtractPosFrom 0)
    (subtractPosFrom rightMostShapeRowIndex)
 where
  subtractPosFrom minuend = pure $ minuend - pos
  rightMostShapeRowIndex = w - 1

-- | Make the first-phase lookup map, keyed by 'Entity',
-- along with automatons whose key symbols are "Maybe Entity".
--
-- Each automaton in this first layer will attempt to match the
-- underlying world row against all rows within all structures
-- (so long as they contain the keyed entity).
--
-- = Preparation steps
--
-- 1. Consolidate all identical rows across all structures into a map
-- 2. Consolidate all entities across these rows into an entity-keyed lookup map
-- 3. Extract the contiguous chunks from each unique row
-- 4. Put the expected indices of these chunks into a lookup structure
-- 5. Prepare Aho-Corasick state machines for recognizing these chunks
mkEntityLookup ::
  (Hashable a, Eq a) =>
  [StructureWithGrid b a] ->
  HM.HashMap a (AutomatonInfo b a)
mkEntityLookup grids =
  HM.map mkRowAutomatons rowsByEntityParticipation
 where
  -- Produces an automaton to evaluate whenever a given entity
  -- is encountered.
  mkRowAutomatons neList =
    AutomatonInfo bounds $
      PiecewiseRecognition chunksStateMachine extractedChunksForLookup
   where
    bounds = sconcat $ NE.map expandedOffsets neList

    -- Prepare lookup structure for use with results of the
    -- Aho-Corasick matcher.
    extractedChunksForLookup = NE.map f neList
     where
      f x = RowChunkMatchingReference (myRow x) (mkRightMap x)
      mkRightMap = binTuplesHM . map (chunkContents &&& chunkStartPos) . contiguousChunks

    extractedChunksForStateMachine =
      HS.fromList . concat . NE.toList $
        NE.map (map chunkContents . contiguousChunks) neList

    -- We wrap the entities with 'Just' since the Aho-Corasick
    -- matcher needs to compare against world cells, which are of 'Maybe' type.
    chunksStateMachine =
      makeStateMachine $
        map (NE.toList . fmap Just &&& id) $
          HS.toList extractedChunksForStateMachine

  -- The values of this map are guaranteed to contain only one
  -- entry per row of each structure, even if some of those
  -- rows contain repetition of the same entity.
  rowsByEntityParticipation =
    binTuplesHM
      . map (myEntity &&& id)
      . concatMap explodeRowEntities
      $ structureRowsByContent

  -- Consolidate all identical rows, whether those rows appear in
  -- same structure or a different structures.
  structureRowsByContent =
    map (\(x, y) -> ConsolidatedRowReferences x y . gridWidth . wholeStructure $ NE.head y)
      . HM.toList
      . binTuplesHM
      . map (rowContent &&& id)
      $ allStructureRows grids

-- | Utilizes the convenient 'wordsBy' function
-- from the "split" package.
getContiguousChunks :: SymbolSequence a -> [PositionedChunk a]
getContiguousChunks rowMembers =
  map mkChunk
    . mapMaybe (NE.nonEmpty . mapMaybe sequenceA)
    . wordsBy (null . snd)
    $ zip [0 :: Int ..] rowMembers
 where
  mkChunk xs = PositionedChunk (fst $ NE.head xs) (NE.map snd xs)

-- | All of the occurrences of each unique entity within a row
-- are consolidated into one record, in which the repetitions are noted.
--
-- The members of "rowMembers" are of 'Maybe' type; the 'Nothing's
-- are dropped but accounted for positionally when indexing the columns.
explodeRowEntities ::
  (Hashable a, Eq a) =>
  ConsolidatedRowReferences b a ->
  [SingleRowEntityOccurrences b a]
explodeRowEntities annotatedRow@(ConsolidatedRowReferences rowMembers _ width) =
  map f $ HM.toList $ binTuplesHM unconsolidatedEntityOccurrences
 where
  chunks = getContiguousChunks $ NE.toList rowMembers

  f (e, occurrences) =
    SingleRowEntityOccurrences annotatedRow e chunks $
      sconcat $
        NE.map deriveEntityOffsets occurrences

  -- Tuples of (entity, rowOccurrenceOfEntity).
  -- Only row members for which an entity exists (is not Nothing)
  -- are retained here.
  unconsolidatedEntityOccurrences =
    map swap
      . catMaybes
      . NE.toList
      $ imap (\idx -> fmap (PositionWithinRow (fromIntegral idx) annotatedRow,)) rowMembers

  deriveEntityOffsets :: PositionWithinRow b a -> InspectionOffsets
  deriveEntityOffsets (PositionWithinRow pos _) = mkOffsets pos width

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
