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
import Data.Map qualified as M
import Data.Map.NonEmpty qualified as NEM
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Text qualified as T
import Linear (V2 (..))
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Placement (StructureName (..))
import Swarm.Game.Scenario.Topography.Structure qualified as Structure
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.State
import Swarm.Game.Step.Util.Inspect
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
    Swap _ newEntity -> do
      doRemoval
      doAddition newEntity
 where
  doAddition newEntity = do
    entLookup <- use $ discovery . structureRecognition . automatons . automatonsByEntity
    forM_ (M.lookup newEntity entLookup) $ \finder -> do
      let msg =
            T.unwords
              [ "Found new"
              , view entityName newEntity
              , "; Finder:"
              , T.pack . show . (^. inspectionOffsets) $ finder
              ]
      discovery . structureRecognition . recognitionLog %= (msg :)
      registerRowMatches cLoc finder

  doRemoval = do
    -- Entity was removed; may need to remove registered structure.
    entLookup <- use $ discovery . structureRecognition . foundStructures . foundByLocation
    forM_ (M.lookup cLoc entLookup) $ \fs ->
      let allOccupiedCoords = genOccupiedCoords fs
          structureName@(StructureName sn) = Structure.name $ originalDefinition $ structureWithGrid fs
          upperLeft = upperLeftCorner fs
          tidyDelete = NEM.nonEmptyMap . NEM.delete upperLeft
       in do
            discovery . structureRecognition . recognitionLog %= (T.unwords ["Removed:", sn] :)
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
  let rowContentMsg =
        T.unwords
          [ "Row content:"
          , T.pack $ show $ map (fmap $ view entityName) entitiesRow
          ]

  discovery . structureRecognition . recognitionLog %= (rowContentMsg :)

  let candidates = findAll sm entitiesRow
      mkMsg c =
        T.unwords
          [ T.pack . show . Structure.name . originalDefinition . wholeStructure . theRow $ pVal c
          , "; Row"
          , T.pack . show . rowIndex . theRow $ pVal c
          ]
      newMsg = T.intercalate ", " $ map mkMsg candidates

  discovery . structureRecognition . recognitionLog %= ("Found: " <> newMsg :)
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

genOccupiedCoords :: FoundStructure -> [Cosmic Location]
genOccupiedCoords (FoundStructure swg loc) =
  [loc `offsetBy` V2 x (negate y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
 where
  AreaDimensions w h = getAreaDimensions $ entityGrid swg

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
  discovery . structureRecognition . recognitionLog %= ("Completed: " <> newMsg :)

  forM_ (listToMaybe rankedCandidates) insertFound
 where
  -- Sorted by decreasing order of preference.
  rankedCandidates = sortOn Down unrankedCandidates

  getStructureName (FoundStructure z _) = T.pack . show . Structure.name . originalDefinition $ z
  newMsg = T.intercalate ", " $ map getStructureName rankedCandidates

  insertFound fs@(FoundStructure swg loc) = do
    discovery . structureRecognition . foundStructures . foundByName %= M.insertWith (<>) k (NEM.singleton loc swg)
    discovery . structureRecognition . foundStructures . foundByLocation %= M.union occupationMap
   where
    k = Structure.name $ originalDefinition swg

    allOccupiedCoords = genOccupiedCoords fs
    occupationMap = M.fromList $ map (,fs) allOccupiedCoords
