-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Registry of found structures.
-- This datatype contains two maps that must be kept in sync.
-- Uses smart constructors to maintain this invariant.
module Swarm.Game.Scenario.Topography.Structure.Recognition.Registry (
  FoundRegistry,

  -- * Instantiation
  emptyFoundStructures,
  populateStaticFoundStructures,

  -- * Read-only accessors
  foundByName,
  foundByLocation,

  -- * Mutation
  addFound,
  removeStructure,
)
where

import Control.Arrow ((&&&))
import Data.List (partition, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEM
import Data.Maybe (listToMaybe, maybeToList)
import Data.Ord (Down (Down))
import Data.Set qualified as Set
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Structure.Named (StructureName, name)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Universe (Cosmic (..))
import Swarm.Util (binTuples, deleteKeys)

-- | The authoritative source of which built structures currently exist.
--
-- The two type parameters, `b` and `a`, correspond
-- to 'Cell' and 'Entity', respectively.
data FoundRegistry b a = FoundRegistry
  { _foundByName :: Map StructureName (NEMap (Cosmic Location) (StructureWithGrid b a))
  , _foundByLocation :: Map (Cosmic Location) (FoundStructure b a)
  }

emptyFoundStructures :: FoundRegistry b a
emptyFoundStructures = FoundRegistry mempty mempty

-- | We use a 'NEMap' here so that we can use the
-- safe-indexing function 'indexWrapNonEmpty' in the implementation
-- of the @structure@ command.
foundByName :: FoundRegistry b a -> Map StructureName (NEMap (Cosmic Location) (StructureWithGrid b a))
foundByName = _foundByName

-- | This is a worldwide "mask" that prevents members of placed
-- structures from participating in new structures and facilitates
-- deletion of structures when their elements are removed from the world.
--
-- Each recognized structure instance will have @MxN@ entries in this map.
foundByLocation :: FoundRegistry b a -> Map (Cosmic Location) (FoundStructure b a)
foundByLocation = _foundByLocation

removeStructure :: FoundStructure b a -> FoundRegistry b a -> FoundRegistry b a
removeStructure fs (FoundRegistry byName byLoc) =
  FoundRegistry
    (M.update tidyDelete structureName byName)
    (deleteKeys allOccupiedCoords byLoc)
 where
  allOccupiedCoords = genOccupiedCoords fs
  structureName = name . originalItem . entityGrid $ structureWithGrid fs
  upperLeft = upperLeftCorner fs

  -- NOTE: Observe similarities to
  -- Swarm.Game.State.removeRobotFromLocationMap
  tidyDelete = NEM.nonEmptyMap . NEM.delete upperLeft

addFound :: FoundStructure b a -> FoundRegistry b a -> FoundRegistry b a
addFound fs@(PositionedStructure loc swg) (FoundRegistry byName byLoc) =
  FoundRegistry
    (M.insertWith (<>) k (NEM.singleton loc swg) byName)
    (M.union occupationMap byLoc)
 where
  k = name . originalItem $ entityGrid swg
  occupationMap = M.fromList $ map (,fs) $ genOccupiedCoords fs

-- | Bulk insertion of structures statically placed in the scenario definition.
--
-- See the docs for 'Swarm.Game.State.Initialize.initializeRecognition' for more context.
--
-- Note that if any of these pre-placed structures overlap, we can't be sure of
-- the author's intent as to which member of the overlap should take precedence,
-- so perhaps it would be ideal to throw an error at scenario parse time.
--
-- However, determining whether a structure is all three of:
-- 1. placed
-- 2. still recognizable
-- 3. overlapping with another recognized structure
-- occurs at a later phase than scenario parse; it requires access to the 'GameState'.
--
-- So we just use the same sorting criteria as the one used to resolve recognition
-- conflicts at entity placement time (see [STRUCTURE RECOGNIZER CONFLICT RESOLUTION]).
populateStaticFoundStructures ::
  (Eq a, Eq b) =>
  [FoundStructure b a] ->
  FoundRegistry b a
populateStaticFoundStructures allFound =
  FoundRegistry byName byLocation
 where
  resolvedCollisions = resolvePreplacementCollisions allFound

  mkOccupationMap fs = M.fromList $ map (,fs) $ genOccupiedCoords fs
  byLocation = M.unions $ map mkOccupationMap resolvedCollisions

  byName =
    M.map (NEM.fromList . NE.map (upperLeftCorner &&& structureWithGrid)) $
      binTuples $
        map (name . originalItem . entityGrid . structureWithGrid &&& id) resolvedCollisions

  resolvePreplacementCollisions foundList =
    nonOverlappingFound <> maybeToList (listToMaybe overlapsByDecreasingPreference)
   where
    overlapsByDecreasingPreference = sortOn Down overlappingFound

    (overlappingFound, nonOverlappingFound) =
      partition ((`Set.member` overlappingPlacements) . fmap distillLabel) foundList

    -- We convert the full-fledged FoundStructure record
    -- to a less-expensive identity-preserving form
    -- for the purpose of set membership
    overlappingPlacements =
      Set.fromList
        . map (fmap distillLabel)
        . concatMap NE.toList
        . M.elems
        . M.filter ((> 1) . NE.length)
        . M.unionsWith (<>)
        $ map (M.map pure . mkOccupationMap) foundList
