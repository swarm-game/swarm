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

import Control.Lens ((%~), (&), (.~), (^.))
import Control.Monad (forM, guard)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Semigroup (Max (..), Min (..))
import Linear (V2 (..))
import Swarm.Game.Location (Location)
import Swarm.Game.Scenario.Topography.Structure.Recognition
import Swarm.Game.Scenario.Topography.Structure.Recognition.Log
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
entityModified entLoader modification cLoc recognizer =
  case modification of
    Add newEntity -> doAddition newEntity recognizer
    Remove _ -> doRemoval
    Swap _ newEntity -> doRemoval >>= doAddition newEntity
 where
  entLookup = recognizer ^. automatons . automatonsByEntity

  doAddition newEntity r = do
    let oldRecognitionState = r ^. recognitionState
    stateRevision <- case HM.lookup newEntity entLookup of
      Nothing -> return oldRecognitionState
      Just finder -> do
        let msg = FoundParticipatingEntity $ ParticipatingEntity newEntity (finder ^. inspectionOffsets)
            stateRevision' = oldRecognitionState & recognitionLog %~ (msg :)

        registerRowMatches entLoader cLoc finder stateRevision'

    return $ r & recognitionState .~ stateRevision

  doRemoval = do
    -- Entity was removed; may need to remove registered structure.
    let oldRecognitionState = recognizer ^. recognitionState
        structureRegistry = oldRecognitionState ^. foundStructures
    stateRevision <- case M.lookup cLoc $ foundByLocation structureRegistry of
      Nothing -> return oldRecognitionState
      Just fs ->
        return $
          oldRecognitionState
            & recognitionLog %~ (StructureRemoved structureName :)
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
  -- | participating entities
  HashSet a ->
  Cosmic Location ->
  s (Maybe a)
candidateEntityAt entLoader registry participating cLoc = runMaybeT $ do
  guard $ M.notMember cLoc $ foundByLocation registry
  ent <- MaybeT $ entLoader cLoc
  guard $ HS.member ent participating
  return ent

-- | Excludes entities that are already part of a
-- registered found structure.
getWorldRow ::
  (Monad s, Hashable a) =>
  GenericEntLocator s a ->
  FoundRegistry b a ->
  -- | participating entities
  HashSet a ->
  Cosmic Location ->
  InspectionOffsets ->
  Int32 ->
  s [Maybe a]
getWorldRow entLoader registry participatingEnts cLoc (InspectionOffsets (Min offsetLeft) (Max offsetRight)) yOffset = do
  mapM getCandidate horizontalOffsets
 where
  getCandidate = candidateEntityAt entLoader registry participatingEnts
  horizontalOffsets = map mkLoc [offsetLeft .. offsetRight]

  -- NOTE: We negate the yOffset because structure rows are numbered increasing from top
  -- to bottom, but swarm world coordinates increase from bottom to top.
  mkLoc x = cLoc `offsetBy` V2 x (negate yOffset)

-- | This is the first (one-dimensional) stage
-- in a two-stage (two-dimensional) search.
registerRowMatches ::
  (Monad s, Hashable a, Eq b) =>
  GenericEntLocator s a ->
  Cosmic Location ->
  AutomatonInfo a (AtomicKeySymbol a) (StructureSearcher b a) ->
  RecognitionState b a ->
  s (RecognitionState b a)
registerRowMatches entLoader cLoc (AutomatonInfo participatingEnts horizontalOffsets sm) rState = do
  let registry = rState ^. foundStructures

  entitiesRow <- getWorldRow entLoader registry participatingEnts cLoc horizontalOffsets 0
  let candidates = findAll sm entitiesRow
      mkCandidateLogEntry c =
        FoundRowCandidate
          (HaystackContext entitiesRow (HaystackPosition $ pIndex c))
          (needleContent $ pVal c)
          rowMatchInfo
       where
        rowMatchInfo = NE.toList . NE.map (f . myRow) . singleRowItems $ pVal c
         where
          f x =
            MatchingRowFrom (rowIndex x) $
              getName . originalDefinition . wholeStructure $
                x

      logEntry = FoundRowCandidates $ map mkCandidateLogEntry candidates

  candidates2D <-
    forM candidates $
      checkVerticalMatch entLoader registry cLoc horizontalOffsets

  return $
    registerStructureMatches (concat candidates2D) $
      rState & recognitionLog %~ (logEntry :)

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
  s [FoundStructure b a]
checkVerticalMatch entLoader registry cLoc (InspectionOffsets (Min searchOffsetLeft) _) foundRow =
  getMatches2D entLoader registry cLoc horizontalFoundOffsets $ automaton2D $ pVal foundRow
 where
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
  s [FoundStructure b a]
getMatches2D
  entLoader
  registry
  cLoc
  horizontalFoundOffsets@(InspectionOffsets (Min offsetLeft) _)
  (AutomatonInfo participatingEnts (InspectionOffsets (Min offsetTop) (Max offsetBottom)) sm) = do
    entityRows <- mapM getRow verticalOffsets
    return $ getFoundStructures (offsetTop, offsetLeft) cLoc sm entityRows
   where
    getRow = getWorldRow entLoader registry participatingEnts cLoc horizontalFoundOffsets
    verticalOffsets = [offsetTop .. offsetBottom]

-- |
-- We only allow an entity to participate in one structure at a time,
-- so multiple matches require a tie-breaker.
-- The largest structure (by area) shall win.
registerStructureMatches ::
  (Eq a, Eq b) =>
  [FoundStructure a b] ->
  RecognitionState a b ->
  RecognitionState a b
registerStructureMatches unrankedCandidates oldState =
  oldState
    & (recognitionLog %~ (newMsg :))
    & foundStructures %~ addFound (listToMaybe rankedCandidates)
 where
  -- Sorted by decreasing order of preference.
  rankedCandidates = sortOn Down unrankedCandidates

  getStructName (FoundStructure swg _) = getName $ originalDefinition swg
  newMsg = FoundCompleteStructureCandidates $ map getStructName rankedCandidates
