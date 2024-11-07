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

import Control.Arrow ((&&&))
import Control.Lens ((%~), (&), (.~), (^.))
import Control.Monad (forM, guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Writer.Strict
import Data.Foldable (foldrM)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.List (sortOn)
import Data.List.NonEmpty.Extra qualified as NE
import Data.Map qualified as M
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Semigroup (Max (..), Min (..))
import Data.Tuple (swap)
import Linear (V2 (..))
import Swarm.Game.Location (Location)
import Swarm.Game.Scenario.Topography.Structure.Recognition
import Swarm.Game.Scenario.Topography.Structure.Recognition.Log
import Swarm.Game.Scenario.Topography.Structure.Recognition.Prep (binTuplesHM)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Scenario.Topography.Terraform
import Swarm.Game.Universe
import Text.AhoCorasick

-- | Interface that provides monadic access to
-- querying entities at locations.
-- The provider may be a 'State' monad or just
-- a 'Reader'.
--
-- 's' is the state variable, 'a' is the return type.
type GenericEntLocator s a = Cosmic Location -> s (Maybe a)

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
    let oldRecognitionState = r ^. recognitionState
    stateRevision <- case HM.lookup newEntity entLookup of
      Nothing -> return oldRecognitionState
      Just finder -> do
        let logFinder f =
              EntityKeyedFinder
                (f ^. inspectionOffsets2)
                (NE.map fst $ f ^. searchPairs2)
                mempty
            msg =
              FoundParticipatingEntity $
                ParticipatingEntity newEntity $
                  logFinder finder
        tell $ pure msg
        foldrM (registerRowMatches entLoader cLoc) oldRecognitionState [finder]

    return $ r & recognitionState .~ stateRevision

  doRemoval = do
    -- Entity was removed; may need to remove registered structure.
    let oldRecognitionState = recognizer ^. recognitionState
        structureRegistry = oldRecognitionState ^. foundStructures
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

-- | In case this cell would match a candidate structure,
-- ensures that the entity in this cell is not already
-- participating in a registered structure.
--
-- Furthermore, treating cells in registered structures
-- as 'Nothing' has the effect of "masking" them out,
-- so that they can overlap empty cells within the bounding
-- box of the candidate structure.
--
-- Finally, entities that are not members of any candidate
-- structure are also masked out, so that it is OK for them
-- to intrude into the candidate structure's bounding box
-- where the candidate structure has empty cells.
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
  Int32 ->
  s [Maybe a]
getWorldRow entLoader registry cLoc (InspectionOffsets (Min offsetLeft) (Max offsetRight)) yOffset = do
  mapM getCandidate horizontalOffsets
 where
  getCandidate = candidateEntityAt entLoader registry
  horizontalOffsets = map mkLoc [offsetLeft .. offsetRight]

  -- NOTE: We negate the yOffset because structure rows are numbered increasing from top
  -- to bottom, but swarm world coordinates increase from bottom to top.
  mkLoc x = cLoc `offsetBy` V2 x (negate yOffset)

logRowCandidates :: [Maybe e] -> [Position (StructureSearcher b e)] -> SearchLog e
logRowCandidates entitiesRow candidates =
  FoundRowCandidates $ map mkCandidateLogEntry candidates
 where
  mkCandidateLogEntry c =
    FoundRowCandidate
      (HaystackContext entitiesRow (HaystackPosition $ pIndex c))
      (needleContent $ pVal c)
      rowMatchInfo
   where
    rowMatchInfo :: [MatchingRowFrom]
    rowMatchInfo = NE.toList . NE.map (f . myRow) . singleRowItems $ pVal c
     where
      f x =
        MatchingRowFrom (rowIndex x) $ distillLabel . wholeStructure $ x

-- | This is the first (one-dimensional) stage
-- in a two-stage (two-dimensional) search.
--
-- It searches for any structure row that happens to
-- contain the placed entity.
registerRowMatches ::
  (Monad s, Hashable a, Eq b) =>
  GenericEntLocator s a ->
  Cosmic Location ->
  AutomatonNewInfo a (StructureSearcher b a) ->
  RecognitionState b a ->
  WriterT [SearchLog a] s (RecognitionState b a)
registerRowMatches entLoader cLoc (AutomatonNewInfo horizontalOffsets sm _ pwMatcher) rState = do
  entitiesRow <-
    lift $
      getWorldRow
        entLoader
        registry
        cLoc
        horizontalOffsets
        0

  -- All of the eligible structure rows found
  -- within this horizontal swath of world cells
  let maskChoices = (entitiesRow, findAll sm entitiesRow)
  tell $ pure $ uncurry logRowCandidates maskChoices

  -- let PiecewiseRecognition pwSM (pwMap :: _) = pwMatcher
  let PiecewiseRecognition pwSM pwMap = pwMatcher
  let candidatesChunked = findAll pwSM entitiesRow
      chunksLookup = binTuplesHM $ map (pVal &&& pIndex) candidatesChunked

  tell . pure . ExpectedChunks $ map HS.toList $ HM.keys pwMap

  let subsetChecker k v = do
        let theIntersection =
              HM.intersection
                chunksLookup
                (HM.fromList $ map (,()) $ HS.toList k)
        return (1 :: Int)
  let candidateExpected = HM.mapMaybeWithKey subsetChecker pwMap

  tell . pure . FoundPiecewiseChunks . map swap $ HM.toList chunksLookup

  let candidates = snd maskChoices
  candidates2Dpairs <-
    lift $
      forM candidates $
        checkVerticalMatch entLoader registry cLoc horizontalOffsets

  let (verticalSpans, candidates2D) = unzip candidates2Dpairs

  tell $ pure $ VerticalSearchSpans verticalSpans

  registerStructureMatches (concat candidates2D) rState
 where
  registry = rState ^. foundStructures

-- | Examines contiguous rows of entities, accounting
-- for the offset of the initially found row.
checkVerticalMatch ::
  (Monad s, Hashable a) =>
  GenericEntLocator s a ->
  FoundRegistry b a ->
  Cosmic Location ->
  -- | Horizontal search offsets
  InspectionOffsets ->
  Position (StructureSearcher b a) ->
  s (VerticalSearch a, [FoundStructure b a])
checkVerticalMatch entLoader registry cLoc (InspectionOffsets (Min searchOffsetLeft) _) foundRow = do
  ((x, y), z) <- getMatches2D entLoader registry cLoc horizontalFoundOffsets $ automaton2D searcherVal
  return (VerticalSearch x rowStructureNames y, z)
 where
  searcherVal = pVal foundRow
  genLabel = distillLabel . wholeStructure . myRow
  rowStructureNames = NE.toList . NE.map genLabel . singleRowItems $ searcherVal

  foundLeftOffset = searchOffsetLeft + fromIntegral (pIndex foundRow)
  foundRightInclusiveIndex = foundLeftOffset + fromIntegral (pLength foundRow) - 1
  horizontalFoundOffsets = InspectionOffsets (pure foundLeftOffset) (pure foundRightInclusiveIndex)

getFoundStructures ::
  Hashable keySymb =>
  (Int32, Int32) ->
  Cosmic Location ->
  StateMachine keySymb (StructureWithGrid b a) ->
  [keySymb] ->
  [FoundStructure b a]
getFoundStructures (offsetTop, offsetLeft) cLoc sm entityRows =
  map mkFound candidates
 where
  candidates = findAll sm entityRows
  mkFound candidate = FoundStructure (pVal candidate) $ cLoc `offsetBy` loc
   where
    -- NOTE: We negate the yOffset because structure rows are numbered increasing from top
    -- to bottom, but swarm world coordinates increase from bottom to top.
    loc = V2 offsetLeft $ negate $ offsetTop + fromIntegral (pIndex candidate)

getMatches2D ::
  (Monad s, Hashable a) =>
  GenericEntLocator s a ->
  FoundRegistry b a ->
  Cosmic Location ->
  -- | Horizontal found offsets (inclusive indices)
  InspectionOffsets ->
  AutomatonInfo a (SymbolSequence a) (StructureWithGrid b a) ->
  s ((InspectionOffsets, [[Maybe a]]), [FoundStructure b a])
getMatches2D
  entLoader
  registry
  cLoc
  horizontalFoundOffsets@(InspectionOffsets (Min offsetLeft) _)
  (AutomatonInfo vRange@(InspectionOffsets (Min offsetTop) (Max offsetBottom)) sm _) = do
    entityRows <- mapM getRow vertOffsets
    return ((vRange, entityRows), getFoundStructures (offsetTop, offsetLeft) cLoc sm entityRows)
   where
    getRow = getWorldRow entLoader registry cLoc horizontalFoundOffsets
    vertOffsets = [offsetTop .. offsetBottom]

-- |
-- We only allow an entity to participate in one structure at a time,
-- so multiple matches require a tie-breaker.
-- The largest structure (by area) shall win.
registerStructureMatches ::
  (Monad s, Eq a, Eq b) =>
  [FoundStructure b a] ->
  RecognitionState b a ->
  WriterT [SearchLog a] s (RecognitionState b a)
registerStructureMatches unrankedCandidates oldState = do
  tell $ pure newMsg
  return $
    oldState
      & foundStructures %~ maybe id addFound (listToMaybe rankedCandidates)
 where
  -- Sorted by decreasing order of preference.
  rankedCandidates = sortOn Down unrankedCandidates

  getStructInfo (FoundStructure swg _) = distillLabel swg
  newMsg = FoundCompleteStructureCandidates $ map getStructInfo rankedCandidates
