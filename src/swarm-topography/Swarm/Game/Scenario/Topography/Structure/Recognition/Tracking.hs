-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Online operations for structure recognizer.
--
-- See "Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute" for
-- details of the structure recognition process.
module Swarm.Game.Scenario.Topography.Structure.Recognition.Tracking (
  entityModified,
) where

import Control.Arrow (left, (&&&))
import Control.Lens ((%~), (&), (.~), (^.))
import Control.Monad (foldM, guard, unless)
import Control.Monad.Extra (findM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Writer.Strict
import Data.Either (partitionEithers)
import Data.Either.Extra (maybeToEither)
import Data.Function (on)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.IntSet qualified as IS
import Data.IntSet.NonEmpty (NEIntSet)
import Data.IntSet.NonEmpty qualified as NEIS
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Ord (Down (..))
import Data.Semigroup (Max (..), Min (..))
import Data.Tuple (swap)
import Linear (V2 (..))
import Swarm.Game.Location (Location)
import Swarm.Game.Scenario.Topography.Structure.Recognition
import Swarm.Game.Scenario.Topography.Structure.Recognition.Log
import Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute (GenericEntLocator, ensureStructureIntact)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Prep (binTuplesHM)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Scenario.Topography.Terraform
import Swarm.Game.Universe
import Text.AhoCorasick

-- | A hook called from the centralized entity update function,
-- 'Swarm.Game.Step.Util.updateEntityAt'.
--
-- This handles structure detection upon addition of an entity,
-- and structure de-registration upon removal of an entity.
-- Also handles atomic entity swaps.
entityModified ::
  (Monad s, Hashable a, Eq b) =>
  GenericEntLocator s a ->
  CellModification a ->
  Cosmic Location ->
  StructureRecognizer b a ->
  s (StructureRecognizer b a)
entityModified entLoader modification cLoc recognizer = do
  (val, accumulatedLogs) <- runWriterT $ case modification of
    Add newEntity -> doAddition newEntity recognizer
    Remove _ -> doRemoval
    Swap _ newEntity -> doRemoval >>= doAddition newEntity
  return $
    val
      & recognitionState . recognitionLog %~ (reverse accumulatedLogs <>)
 where
  entLookup = recognizer ^. automatons . automatonsByEntity

  doAddition newEntity r = do
    stateRevision <- case HM.lookup newEntity entLookup of
      Nothing -> return oldRecognitionState
      Just finder -> do
        let logFinder f = EntityKeyedFinder (f ^. inspectionOffsets)
        tell . pure . FoundParticipatingEntity . ParticipatingEntity newEntity $
          logFinder finder
        registerRowMatches entLoader cLoc finder oldRecognitionState

    return $ r & recognitionState .~ stateRevision
   where
    oldRecognitionState = r ^. recognitionState

  doRemoval = do
    -- Entity was removed; may need to remove registered structure.
    stateRevision <- case M.lookup cLoc $ foundByLocation structureRegistry of
      Nothing -> return oldRecognitionState
      Just fs -> do
        tell $ pure $ StructureRemoved structureName
        return $
          oldRecognitionState
            & foundStructures %~ removeStructure fs
       where
        structureName = getName $ originalDefinition $ structureWithGrid fs

    return $ recognizer & recognitionState .~ stateRevision
   where
    oldRecognitionState = recognizer ^. recognitionState
    structureRegistry = oldRecognitionState ^. foundStructures

-- | In case this cell would match a candidate structure,
-- ensures that the entity in this cell is not already
-- participating in a registered structure.
--
-- Furthermore, treating cells in registered structures
-- as 'Nothing' has the effect of "masking" them out,
-- so that they can overlap empty cells within the bounding
-- box of the candidate structure.
candidateEntityAt ::
  (Monad s, Hashable a) =>
  GenericEntLocator s a ->
  FoundRegistry b a ->
  Cosmic Location ->
  s (Maybe a)
candidateEntityAt entLoader registry cLoc = runMaybeT $ do
  guard $ M.notMember cLoc $ foundByLocation registry
  MaybeT $ entLoader cLoc

-- | Excludes entities that are already part of a
-- registered found structure.
getWorldRow ::
  (Monad s, Hashable a) =>
  GenericEntLocator s a ->
  FoundRegistry b a ->
  Cosmic Location ->
  InspectionOffsets ->
  s [Maybe a]
getWorldRow entLoader registry cLoc (InspectionOffsets (Min offsetLeft) (Max offsetRight)) = do
  mapM getCandidate horizontalOffsets
 where
  getCandidate = candidateEntityAt entLoader registry
  horizontalOffsets = map mkLoc [offsetLeft .. offsetRight]
  mkLoc x = cLoc `offsetBy` V2 x 0

-- | This runs once per non-overlapping subset of found chunks
checkChunksCombination ::
  (Monad m, Hashable a, Eq b) =>
  Cosmic Location ->
  InspectionOffsets ->
  NE.NonEmpty (RowChunkMatchingReference b a) ->
  [Position (NE.NonEmpty a)] ->
  WriterT [SearchLog a] m [FoundStructure b a]
checkChunksCombination
  cLoc
  horizontalOffsets
  rowChunkReferences
  candidatesChunked = do
    tell . pure . FoundPiecewiseChunks . map swap $ HM.toList $ fmap NEIS.elems foundRowChunksLookup

    tell . pure . ChunkFailures $ candidateFailures

    tell . pure . ChunksMatchingExpected $
      map (modifyChunkedRowMatch $ fmap renderSharedNames) candidateExpected

    return structurePositionsToCheck
   where
    structurePositionsToCheck = concatMap mkFoundStructures candidateExpected

    candidateExpected = concatMap NE.toList candidateExpectedLists

    foundRowChunksLookup = fmap NEIS.fromList $ binTuplesHM $ map (pVal &&& pIndex) candidatesChunked

    (candidateFailures, candidateExpectedLists) =
      partitionEithers $
        map (checkCandidateAgainstObservedChunks horizontalOffsets foundRowChunksLookup) $
          NE.toList rowChunkReferences

    mkFoundStructures x =
      NE.toList $ NE.map mkFoundStructure . referencingRows . chunkStructure $ foundChunkRow x
     where
      mkFoundStructure r =
        FoundStructure
          (wholeStructure r)
          (cLoc `offsetBy` theOffset)
       where
        theOffset = V2 (horizontalStructPos $ foundChunkRow x) (rowIndex r)

    modifyChunkedRowMatch f (ChunkedRowMatch x y) = ChunkedRowMatch x (f y)

checkCandidateAgainstObservedChunks ::
  Hashable e =>
  InspectionOffsets ->
  HM.HashMap (NE.NonEmpty e) NEIntSet ->
  RowChunkMatchingReference b e ->
  Either (ChunkMatchFailureReason e) (NE.NonEmpty (ChunkedRowMatch (ConsolidatedRowReferences b e) e))
checkCandidateAgainstObservedChunks horizontalOffsets foundRowChunksLookup (RowChunkMatchingReference r chunkPositionMap) =
  left (ChunkMatchFailureReason $ renderSharedNames r) $ do
    unless isKeysSubset . Left $
      NoKeysSubset $
        (FoundChunkComparison `on` HS.toList) foundChunksKeys referenceChunksKeys

    nonEmptyPairs <-
      maybeToEither EmptyIntersection $
        NE.nonEmpty sortedByAlignmentChoices

    let maybeViables = do
          possibles <- seedPossibleOffsets $ snd $ NE.head nonEmptyPairs
          foldM findCoveringOffsets possibles $ NE.map (snd . snd) nonEmptyPairs

    viableRowOffsets <- maybeToEither EmptyIntersection maybeViables
    return $ NE.map mkRowMatch $ NEIS.toList viableRowOffsets
 where
  theIntersection =
    HM.intersectionWith
      FoundAndExpectedChunkPositions
      foundRowChunksLookup
      modifiedChunkPositionMap
  intersectionWithSizeDifferences = HM.map (criteria &&& id) theIntersection
   where
    criteria x = (subtract `on` NEIS.size) (expectedPositions x) (foundPositions x)

  -- Remove the pairings that have fewer occurrences than the required number.
  -- The 'fst' element of the tuple is the difference between the "observed" and "required" count.
  withSufficientCoverage = HM.filter ((>= 0) . fst) intersectionWithSizeDifferences
  sortedByAlignmentChoices = sortOn (fst . snd) $ HM.toList withSufficientCoverage

  isKeysSubset = referenceChunksKeys `HS.isSubsetOf` foundChunksKeys

  mkRowMatch rowOffset =
    ChunkedRowMatch
      (map swap $ HM.toList theIntersection)
      (FoundRowFromChunk rowOffset horizontalStructurePosition r)
   where
    horizontalStructurePosition = fromIntegral rowOffset + getMin (startOffset horizontalOffsets)

  modifiedChunkPositionMap = fmap NEIS.fromList chunkPositionMap
  foundChunksKeys = HM.keysSet foundRowChunksLookup
  referenceChunksKeys = HM.keysSet chunkPositionMap

-- | Search for any structure row that happens to
-- contain the placed entity.
registerRowMatches ::
  (Monad s, Hashable a, Eq b) =>
  GenericEntLocator s a ->
  Cosmic Location ->
  AutomatonInfo b a ->
  RecognitionState b a ->
  WriterT [SearchLog a] s (RecognitionState b a)
registerRowMatches entLoader cLoc (AutomatonInfo horizontalOffsets pwMatcher) rState = do
  tell $ pure $ StartSearchAt cLoc horizontalOffsets

  tell . pure . ExpectedChunks $ NE.map (HM.keys . confirmationMap) rowChunkReferences

  entitiesRow <-
    lift $
      getWorldRow
        entLoader
        registry
        cLoc
        horizontalOffsets

  let candidatesChunked = findAll pwSM entitiesRow
  unrankedCandidateStructures <- checkCombo candidatesChunked

  -- We only allow an entity to participate in one structure at a time,
  -- so multiple matches require a tie-breaker.
  -- The largest structure (by area) shall win.
  -- Sort by decreasing order of preference.
  let rankedCandidates = sortOn Down unrankedCandidateStructures
  tell . pure . FoundCompleteStructureCandidates $ map getStructInfo rankedCandidates

  -- We should not check all of the structures, which can be expensive.
  -- Instead, we ranked the candidates by preference a-priori
  -- and now choose the first one that is verified.
  maybeIntactStructure <- findM validateIntactness2d rankedCandidates

  lift $ registerBestStructureMatch maybeIntactStructure rState
 where
  registry = rState ^. foundStructures
  PiecewiseRecognition pwSM rowChunkReferences = pwMatcher

  getStructInfo (FoundStructure swg loc) = (distillLabel swg, loc)

  validateIntactness2d fs = do
    maybeIntactnessFailure <- lift $ ensureStructureIntact entLoader fs
    tell . pure . ChunkIntactnessVerification $
      IntactPlacementLog
        maybeIntactnessFailure
        (getName . originalDefinition . structureWithGrid $ fs)
        (upperLeftCorner fs)
    return $ null maybeIntactnessFailure

  checkCombo = checkChunksCombination cLoc horizontalOffsets rowChunkReferences

-- |
-- For a given "chunk", there could be multiple recurrences.
-- However, the position of each recurrence is unique
-- (i.e. the chunk cannot exist twice at the same location).
--
-- Either:
-- A) An observed chunk is "superfluous" w.r.t. matching the candidate, or
-- B) It is necessary for the match.
--
-- The lowest-numbered "reference position" (i.e. in the structure definition)
-- of a given chunk must align with exactly one "observed position".
--
-- The difference between the "observed" position of the chunk that aligns with the
-- lowest-numbered "reference position" shall be the global "row offset" applied to our observations.
-- This row offset value applies to all "chunks" (both identical and distinct) that comprise the row.
--
-- If a given chunk occurrence is necessary for the match, then we may attempt to use it to compute
-- the "row offset" by taking its position minus the lowest-numbered "reference position".
--
-- We can iterate over each occurrence position in ascending order.
-- In the ideal case, the first such candidate ends up being the the actual, valid, offset.
-- Otherwise, we know that all invalid offset candidates encountered before the first valid
-- offset constitute "superfluous" chunks.
--
-- Note that there may exist multiple valid "row offsets".
-- At most, there will be
--   {number of observed occurrences} minus {number of required occurrences}
-- such offsets.
--
-- = Performance notes
--
-- We only have to do this computation once, and only for the "smallest" size discrepancy
-- between occurrences and references of a chunk. This generates the "seed" pool of possible offsets.
-- All subsequent chunks will merely filter on this initial set.
seedPossibleOffsets :: (Int, FoundAndExpectedChunkPositions) -> Maybe NEIntSet
seedPossibleOffsets (sizeDifference, FoundAndExpectedChunkPositions found expected) =
  NEIS.nonEmptySet $ IS.fromList possibleOffsets
 where
  possibleOffsets =
    NE.take (sizeDifference + 1) $
      NE.map (subtract (NEIS.findMin expected)) $
        NEIS.toAscList found

-- | Return all of the offsets that are viable for repetitions of this chunk.
--
-- Note that if there are an equal number of observed occurrences
-- and expected occurrences, then there is only one possible offset.
-- If there are N expected and (N + 1) observed, then there are 2 possible offsets.
findCoveringOffsets :: NEIntSet -> FoundAndExpectedChunkPositions -> Maybe NEIntSet
findCoveringOffsets possibleOffsets x =
  NEIS.nonEmptySet $ NEIS.filter (isCoveredWithOffset x) possibleOffsets

isCoveredWithOffset :: FoundAndExpectedChunkPositions -> Int -> Bool
isCoveredWithOffset (FoundAndExpectedChunkPositions found expected) offset =
  NEIS.map (+ offset) expected `NEIS.isSubsetOf` found

registerBestStructureMatch ::
  (Monad s, Eq a, Eq b) =>
  Maybe (FoundStructure b a) ->
  RecognitionState b a ->
  s (RecognitionState b a)
registerBestStructureMatch maybeValidCandidate oldState =
  return $
    oldState
      & foundStructures %~ maybe id addFound maybeValidCandidate
