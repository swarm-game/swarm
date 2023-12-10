{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Structure recognizer types.
--
-- See overview of the structure recognizer feature in
-- "Swarm.Game.Scenario.Topography.Structure.Recognition.Precompute".
--
-- The following structure template shall be used to illustrate
-- roles of the types in this module:
--
-- @
-- cdc
-- aab
-- cdc
-- @
module Swarm.Game.Scenario.Topography.Structure.Recognition.Type where

import Control.Arrow ((&&&))
import Control.Lens (makeLenses)
import Data.Aeson (ToJSON)
import Data.Function (on)
import Data.Int (Int32)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Ord (Down (Down))
import Data.Semigroup (Max, Min)
import Data.Set (Set)
import GHC.Generics (Generic)
import Linear (V2 (..))
import Swarm.Game.Entity (Entity, EntityName)
import Swarm.Game.Location (Location)
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Placement (StructureName)
import Swarm.Game.Scenario.Topography.Structure (NamedGrid)
import Swarm.Game.Universe (Cosmic, offsetBy)
import Swarm.Language.Syntax (AbsoluteDir)
import Text.AhoCorasick (StateMachine)

-- | A "needle" consisting of a single cell within
-- the haystack (a row of cells) to be searched.
--
-- === Example
-- A single entity @a@ in the row:
--
-- @
-- aab
-- @
type AtomicKeySymbol = Maybe Entity

-- | A "needle" consisting row of cells within the haystack
-- (a sequence of rows) to be searched.
--
-- === Example
-- The complete row:
--
-- @
-- aab
-- @
type SymbolSequence = [AtomicKeySymbol]

-- | This is returned as a value of the 1-D searcher.
-- It contains search automatons customized to the 2-D structures
-- that may possibly contain the row found by the 1-D searcher.
data StructureSearcher = StructureSearcher
  { automaton2D :: AutomatonInfo SymbolSequence StructureWithGrid
  , needleContent :: SymbolSequence
  , singleRowItems :: NE.NonEmpty SingleRowEntityOccurrences
  }

-- |
-- Position specific to a single entity within a horizontal row.
--
-- === Example
-- For entity @b@ within the row:
--
-- @
-- aab
-- @
--
-- Its '_position' is @2@.
data PositionWithinRow = PositionWithinRow
  { _position :: Int32
  -- ^ horizontal index of the entity within the row
  , structureRow :: StructureRow
  }

-- Represents all of the locations that particular entity
-- occurs within a specific row of a particular structure.
--
-- === Example
-- For entity @a@ within the row:
--
-- @
-- aab
-- @
--
-- this record will contain two entries in its 'entityOccurrences' field.
data SingleRowEntityOccurrences = SingleRowEntityOccurrences
  { myRow :: StructureRow
  , myEntity :: Entity
  , entityOccurrences :: NE.NonEmpty PositionWithinRow
  , expandedOffsets :: InspectionOffsets
  }

-- | A a specific row within a particular structure.
--
-- === Example
-- For the second occurrence of @cdc@ within the structure:
--
-- @
-- cdc
-- aab
-- cdc
-- @
--
-- it's 'rowIndex' is @2@.
data StructureRow = StructureRow
  { wholeStructure :: StructureWithGrid
  , rowIndex :: Int32
  -- ^ vertical index of the row within the structure
  , rowContent :: SymbolSequence
  }

-- | The original definition of a structure, bundled
-- with its grid of cells having been extracted for convenience.
data StructureWithGrid = StructureWithGrid
  { originalDefinition :: NamedGrid (Maybe Cell)
  , rotatedTo :: AbsoluteDir
  , entityGrid :: [SymbolSequence]
  }
  deriving (Eq)

-- | Structure definitions with precomputed metadata for consumption by the UI
data StructureInfo = StructureInfo
  { withGrid :: StructureWithGrid
  , entityCounts :: Map Entity Int
  }

-- | For all of the rows that contain a given entity
-- (and are recognized by a single automaton),
-- compute the left-most and right-most position
-- within the row that the given entity may occur.
--
-- This determines how far to the left and to the right
-- our search of the world cells needs to begin and
-- end, respectively.
--
-- The 'Semigroup' instance always grows in extent, taking the minimum
-- of the leftward offsets and the maximum of the rightward offsets.
data InspectionOffsets = InspectionOffsets
  { startOffset :: Min Int32
  -- ^ Always non-positive (i.e. either zero or negative).
  -- For the first-level search, this extends to the left.
  -- For the second-level search, this extends upward.
  , endOffset :: Max Int32
  -- ^ Always non-negative.
  -- For the first-level search, this extends to the right.
  -- For the second-level search, this extends downward.
  }
  deriving (Show, Generic, ToJSON)

instance Semigroup InspectionOffsets where
  InspectionOffsets l1 r1 <> InspectionOffsets l2 r2 =
    InspectionOffsets (l1 <> l2) (r1 <> r2)

-- | Each automaton shall be initialized to recognize
-- a certain subset of structure rows, that may either
-- all be within one structure, or span multiple structures.
data AutomatonInfo k v = AutomatonInfo
  { _participatingEntities :: Set EntityName
  , _inspectionOffsets :: InspectionOffsets
  , _automaton :: StateMachine k v
  }
  deriving (Generic)

makeLenses ''AutomatonInfo

-- | The complete set of data needed to identify applicable
-- structures, based on a just-placed entity.
data RecognizerAutomatons = RecognizerAutomatons
  { _originalStructureDefinitions :: Map StructureName StructureInfo
  -- ^ all of the structures that shall participate in automatic recognition.
  -- This list is used only by the UI and by the 'Floorplan' command.
  , _automatonsByEntity :: Map Entity (AutomatonInfo AtomicKeySymbol StructureSearcher)
  }
  deriving (Generic)

makeLenses ''RecognizerAutomatons

-- | Final output of the search process.
-- These are the elements that are stored in the 'FoundRegistry'.
data FoundStructure = FoundStructure
  { structureWithGrid :: StructureWithGrid
  , upperLeftCorner :: Cosmic Location
  }
  deriving (Eq)

-- | Ordering is by increasing preference between simultaneously
-- completed structures.
-- The preference heuristic is for:
--
-- 1. Primarily, larger area.
-- 2. Secondarily, lower X-Y coords (X is compared first)
--
-- Since the natural order of coordinates increases as described,
-- we need to invert it with 'Down' so that this ordering is by
-- increasing preference.
instance Ord FoundStructure where
  compare = compare `on` (f1 &&& f2)
   where
    f1 = computeArea . getAreaDimensions . entityGrid . structureWithGrid
    f2 = Down . upperLeftCorner

-- | Yields coordinates that are occupied by an entity of a placed structure.
-- Cells within the rectangular bounds of the structure that are unoccupied
-- are not included.
genOccupiedCoords :: FoundStructure -> [Cosmic Location]
genOccupiedCoords (FoundStructure swg loc) =
  catMaybes . concat . zipWith mkRow [0 ..] $ entityGrid swg
 where
  mkCol y x ent = loc `offsetBy` V2 x (negate y) <$ ent
  mkRow rowIdx = zipWith (mkCol rowIdx) [0 ..]
