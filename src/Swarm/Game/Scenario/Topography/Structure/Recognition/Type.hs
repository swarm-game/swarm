{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Structure recognizer types
--
-- See overview of this feature in
-- "Swarm.Game.Scenario.Topography.Structure.Recognition.Tracking".
module Swarm.Game.Scenario.Topography.Structure.Recognition.Type where

import Control.Arrow ((&&&))
import Control.Lens hiding (from, (<.>))
import Data.Aeson (ToJSON)
import Data.Function (on)
import Data.Int (Int32)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map.NonEmpty (NEMap)
import Data.Ord (Down (Down))
import Data.Semigroup (Max, Min)
import GHC.Generics (Generic)
import Linear (V2 (..))
import Swarm.Game.Entity (Entity)
import Swarm.Game.Location (Location)
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Placement (StructureName)
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Universe (Cosmic, offsetBy)
import Text.AhoCorasick (StateMachine)

type AtomicKeySymbol = Maybe Entity
type SymbolSequence = [AtomicKeySymbol]

data StructureSearcher = StructureSearcher
  { automaton2D :: AutomatonInfo SymbolSequence StructureRow
  , needleContent :: SymbolSequence
  , singleRowItems :: NE.NonEmpty SingleRowEntityRecurrences
  }

data PositionWithinRow = PositionWithinRow
  { _position :: Int32
  -- ^ horizontal index of the entity within the row
  , structureRow :: StructureRow
  }

data SingleRowEntityRecurrences = SingleRowEntityRecurrences
  { myRow :: StructureRow
  , myEntity :: Entity
  , entityOccurrences :: NE.NonEmpty PositionWithinRow
  , expandedOffsets :: InspectionOffsets
  }

data StructureRow = StructureRow
  { wholeStructure :: StructureWithGrid
  , rowIndex :: Int32
  -- ^ vertical index of the row within the structure
  , rowContent :: SymbolSequence
  }

data StructureWithGrid = StructureWithGrid
  { originalDefinition :: NamedStructure (Maybe Cell)
  , entityGrid :: [SymbolSequence]
  }
  deriving (Eq)

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
  { _inspectionOffsets :: InspectionOffsets
  , _automaton :: StateMachine k v
  }
  deriving (Generic)

makeLenses ''AutomatonInfo

-- | The complete set of data needed to identify applicable
-- structures, based on a just-placed entity.
data RecognizerAutomatons = RecognizerAutomatons
  { _definitions :: [StructureInfo]
  -- ^ all of the structures that shall participate in automatic recognition
  , _automatonsByEntity :: Map Entity (AutomatonInfo AtomicKeySymbol StructureSearcher)
  }
  deriving (Generic)

makeLenses ''RecognizerAutomatons

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

genOccupiedCoords :: FoundStructure -> [Cosmic Location]
genOccupiedCoords (FoundStructure swg loc) =
  [loc `offsetBy` V2 x (negate y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
 where
  AreaDimensions w h = getAreaDimensions $ entityGrid swg

data FoundStructures = FoundStructures
  { _foundByName :: Map StructureName (NEMap (Cosmic Location) StructureWithGrid)
  -- ^ We use a 'NEMap' here so that we can use the
  -- safe-indexing function 'indexWrapNonEmpty' in the implementation
  -- of the "structure" command.
  , _foundByLocation :: Map (Cosmic Location) FoundStructure
  -- ^ This is a worldwide "mask" that prevents members of placed
  -- structures from participating in new structures.
  --
  -- Each recognized structure instance will have @MxN@ entries in this map.
  }
  deriving (Generic)

makeLenses ''FoundStructures
