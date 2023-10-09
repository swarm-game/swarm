{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Structure recognizer: online operations
module Swarm.Game.Scenario.Topography.Structure.Recognition.Tracking where

import Control.Carrier.State.Lazy
import Control.Effect.Lens
import Control.Lens ((^.))
import Control.Monad (forM, forM_)
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Map.NonEmpty qualified as NEM
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Linear (V2 (..))
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Structure qualified as Structure
import Swarm.Game.Scenario.Topography.Structure.Recognition.Log
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type.Toplevel
import Swarm.Game.State
import Swarm.Game.Universe
import Swarm.Game.World.Modify
import Swarm.Util (deleteKeys)
import Text.AhoCorasick

entityModified ::
  (Has (State GameState) sig m) =>
  CellModification Entity ->
  Cosmic Location ->
  m ()
entityModified modification cLoc = do
  case modification of
    Add newEntity -> doAddition newEntity
    Remove _ -> doRemoval
    Swap _ newEntity -> doRemoval >> doAddition newEntity
 where
  doAddition newEntity = do
    entLookup <- use $ discovery . structureRecognition . automatons . automatonsByEntity
    forM_ (M.lookup newEntity entLookup) $ \finder -> do
      let msg = FoundParticipatingEntity $ ParticipatingEntity (view entityName newEntity) (finder ^. inspectionOffsets)
      discovery . structureRecognition . recognitionLog %= (msg :)
      registerRowMatches cLoc finder

  doRemoval = do
    -- Entity was removed; may need to remove registered structure.
    structureLookup <- use $ discovery . structureRecognition . foundStructures . foundByLocation
    forM_ (M.lookup cLoc structureLookup) $ \fs ->
      let allOccupiedCoords = genOccupiedCoords fs
          structureName = Structure.name $ originalDefinition $ structureWithGrid fs
          upperLeft = upperLeftCorner fs
          tidyDelete = NEM.nonEmptyMap . NEM.delete upperLeft
       in do
            discovery . structureRecognition . recognitionLog %= (StructureRemoved structureName :)
            discovery . structureRecognition . foundStructures . foundByLocation %= deleteKeys allOccupiedCoords

            -- NOTE: Observe similarities to
            -- Swarm.Game.State.removeRobotFromLocationMap
            discovery . structureRecognition . foundStructures . foundByName %= M.update tidyDelete structureName

-- | Ensures that the entity in this cell is not already
-- participating in a registered structure
availableEntityAt ::
  (Has (State GameState) sig m) =>
  Cosmic Location ->
  m (Maybe Entity)
availableEntityAt cLoc = do
  occupancyMap <- use $ discovery . structureRecognition . foundStructures . foundByLocation
  if M.member cLoc occupancyMap
    then return Nothing
    else entityAt cLoc

-- | Excludes entities that are already part of a
-- registered found structure.
getWorldRow ::
  (Has (State GameState) sig m) =>
  Cosmic Location ->
  InspectionOffsets ->
  Int32 ->
  m [Maybe Entity]
getWorldRow cLoc (InspectionOffsets offsetLeft offsetRight) yOffset =
  mapM availableEntityAt horizontalOffsets
 where
  horizontalOffsets = map mkLoc [offsetLeft .. offsetRight]

  -- NOTE: We negate the yOffset because structure rows are numbered increasing from top
  -- to bottom, but swarm world coordinates increase from bottom to top.
  mkLoc x = cLoc `offsetBy` V2 x (negate yOffset)

registerRowMatches ::
  (Has (State GameState) sig m) =>
  Cosmic Location ->
  AutomatonInfo AtomicKeySymbol StructureSearcher ->
  m ()
registerRowMatches cLoc (AutomatonInfo horizontalOffsets sm) = do
  entitiesRow <- getWorldRow cLoc horizontalOffsets 0
  let candidates = findAll sm entitiesRow
      mkMsg c =
        FoundRowCandidate
          (HaystackContext (map (fmap $ view entityName) entitiesRow) (HaystackPosition $ pIndex c))
          (map (fmap $ view entityName) . needleContent . theRow $ pVal c)
          rowMatchInfo
       where
        rowMatchInfo = NE.toList . NE.map (q . myRow) . singleRowItems . theRow $ pVal c
         where
          q x = MatchingRowFrom (rowIndex x) $ Structure.name . originalDefinition . wholeStructure $ x

      newMsg = FoundRowCandidates $ map mkMsg candidates

  discovery . structureRecognition . recognitionLog %= (newMsg :)
  candidates2D <- forM candidates $ checkVerticalMatch cLoc horizontalOffsets
  registerStructureMatches $ concat candidates2D

checkVerticalMatch ::
  (Has (State GameState) sig m) =>
  Cosmic Location ->
  -- | Horizontal search offsets
  InspectionOffsets ->
  Position StructureSearcher ->
  m [FoundStructure]
checkVerticalMatch cLoc (InspectionOffsets searchOffsetLeft _) foundRow =
  getMatches2D cLoc horizontalFoundOffsets $ automaton2D $ pVal foundRow
 where
  foundLeftOffset = searchOffsetLeft + fromIntegral (pIndex foundRow)
  foundRightInclusiveIndex = foundLeftOffset + fromIntegral (pLength foundRow) - 1
  horizontalFoundOffsets = InspectionOffsets foundLeftOffset foundRightInclusiveIndex

getFoundStructures ::
  Hashable keySymb =>
  (Int32, Int32) ->
  Cosmic Location ->
  StateMachine keySymb StructureRow ->
  [keySymb] ->
  [FoundStructure]
getFoundStructures (offsetTop, offsetLeft) cLoc sm entityRows =
  map mkFound candidates
 where
  candidates = findAll sm entityRows
  mkFound candidate = FoundStructure (wholeStructure $ pVal candidate) $ cLoc `offsetBy` loc
   where
    -- NOTE: We negate the yOffset because structure rows are numbered increasing from top
    -- to bottom, but swarm world coordinates increase from bottom to top.
    loc = V2 offsetLeft $ negate $ offsetTop + fromIntegral (pIndex candidate)

getMatches2D ::
  (Has (State GameState) sig m) =>
  Cosmic Location ->
  -- | Horizontal found offsets (inclusive indices)
  InspectionOffsets ->
  AutomatonInfo [AtomicKeySymbol] StructureRow ->
  m [FoundStructure]
getMatches2D
  cLoc
  horizontalFoundOffsets@(InspectionOffsets offsetLeft _)
  (AutomatonInfo (InspectionOffsets offsetTop offsetBottom) sm) = do
    entityRows <- mapM getRow verticalOffsets
    return $ getFoundStructures (offsetTop, offsetLeft) cLoc sm entityRows
   where
    getRow = getWorldRow cLoc horizontalFoundOffsets
    verticalOffsets = [offsetTop .. offsetBottom]

-- |
-- We only allow an entity to participate in one structure at a time,
-- so multiple matches require a tie-breaker.
-- The largest structure (by area) shall win.
registerStructureMatches ::
  (Has (State GameState) sig m) =>
  [FoundStructure] ->
  m ()
registerStructureMatches unrankedCandidates = do
  discovery . structureRecognition . recognitionLog %= (newMsg :)

  forM_ (listToMaybe rankedCandidates) insertFound
 where
  -- Sorted by decreasing order of preference.
  rankedCandidates = sortOn Down unrankedCandidates

  getStructureName (FoundStructure z _) = Structure.name . originalDefinition $ z
  newMsg = FoundCompleteStructureCandidates $ map getStructureName rankedCandidates

insertFound ::
  (Has (State GameState) sig m) =>
  FoundStructure ->
  m ()
insertFound fs@(FoundStructure swg loc) = do
  discovery . structureRecognition . foundStructures . foundByName %= M.insertWith (<>) k (NEM.singleton loc swg)
  discovery . structureRecognition . foundStructures . foundByLocation %= M.union occupationMap
 where
  k = Structure.name $ originalDefinition swg
  occupationMap = M.fromList $ map (,fs) $ genOccupiedCoords fs
