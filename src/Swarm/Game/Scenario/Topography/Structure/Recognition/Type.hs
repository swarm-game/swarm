{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Structure recognizer
module Swarm.Game.Scenario.Topography.Structure.Recognition.Type where

import Control.Lens hiding (from, (<.>))
import Data.Function (on)
import Data.Int (Int32)
import Data.Map (Map)
import Data.Map.NonEmpty (NEMap)
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Game.Entity (Entity)
import Swarm.Game.Location (Location)
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Placement (StructureName)
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Universe (Cosmic)
import Text.AhoCorasick (StateMachine)

type AtomicKeySymbol = Maybe Entity

data StructureSearcher = StructureSearcher
  { automaton2D :: AutomatonInfo [AtomicKeySymbol] StructureRow
  , theRow :: StructureRow
  }

data StructureRow = StructureRow
  { wholeStructure :: StructureWithGrid
  , rowIndex :: Int32
  , rowContent :: [AtomicKeySymbol]
  }

data StructureWithGrid = StructureWithGrid
  { originalDefinition :: NamedStructure (Maybe Cell)
  , entityGrid :: [[AtomicKeySymbol]]
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
  { startOffset :: Int32
  -- ^ Always non-positive (i.e. either zero or negative).
  -- For the first-level search, this extends to the left.
  -- For the second-level search, this extends upward.
  , endOffset :: Int32
  -- ^ Always non-negative.
  -- For the first-level search, this extends to the right.
  -- For the second-level search, this extends downward.
  }
  deriving (Show)

instance Semigroup InspectionOffsets where
  InspectionOffsets l1 r1 <> InspectionOffsets l2 r2 =
    InspectionOffsets (min l1 l2) (max r1 r2)

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
-- The preference heuristic is for larger area.
-- TODO May also want to tiebreak that by furthest to the northwest
-- (lowest row-major matrix index).
instance Ord FoundStructure where
  compare = compare `on` computeArea . getAreaDimensions . entityGrid . structureWithGrid

data FoundStructures = FoundStructures
  { _foundByName :: Map StructureName (NEMap (Cosmic Location) StructureWithGrid)
  , _foundByLocation :: Map (Cosmic Location) FoundStructure
  -- ^ This is a worldwide "mask" that prevents members of placed
  -- structures from participating in new structures.
  --
  -- Each recognized structure instance will have @MxN@ entries in this map.
  }
  deriving (Generic)

makeLenses ''FoundStructures

emptyFoundStructures :: FoundStructures
emptyFoundStructures = FoundStructures mempty mempty

data StructureRecognizer = StructureRecognizer
  { _automatons :: RecognizerAutomatons
  , _foundStructures :: FoundStructures
  -- ^ Records the top-left corner of the found structure
  , _recognitionLog :: [Text]
  }
  deriving (Generic)

makeLenses ''StructureRecognizer
