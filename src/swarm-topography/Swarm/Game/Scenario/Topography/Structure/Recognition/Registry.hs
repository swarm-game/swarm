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
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEM
import Swarm.Game.Location (Location)
import Swarm.Game.Scenario.Topography.Placement (StructureName)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Scenario.Topography.Structure.Type qualified as Structure
import Swarm.Game.Universe (Cosmic)
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
  structureName = Structure.name $ originalDefinition $ structureWithGrid fs
  upperLeft = upperLeftCorner fs

  -- NOTE: Observe similarities to
  -- Swarm.Game.State.removeRobotFromLocationMap
  tidyDelete = NEM.nonEmptyMap . NEM.delete upperLeft

addFound :: FoundStructure b a -> FoundRegistry b a -> FoundRegistry b a
addFound fs@(FoundStructure swg loc) (FoundRegistry byName byLoc) =
  FoundRegistry
    (M.insertWith (<>) k (NEM.singleton loc swg) byName)
    (M.union occupationMap byLoc)
 where
  k = Structure.name $ originalDefinition swg
  occupationMap = M.fromList $ map (,fs) $ genOccupiedCoords fs

-- | Bulk insertion of found structures.
--
-- Each of these shall have been re-checked in case
-- a subsequent placement occludes them.
populateStaticFoundStructures :: [FoundStructure b a] -> FoundRegistry b a
populateStaticFoundStructures allFound =
  FoundRegistry byName byLocation
 where
  mkOccupationMap fs = M.fromList $ map (,fs) $ genOccupiedCoords fs
  byLocation = M.unions $ map mkOccupationMap allFound

  byName =
    M.map (NEM.fromList . NE.map (upperLeftCorner &&& structureWithGrid)) $
      binTuples $
        map (Structure.name . originalDefinition . structureWithGrid &&& id) allFound
