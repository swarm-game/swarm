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
import Data.HashMap.Strict (HashMap)
import Data.Int (Int32)
import Data.IntSet.NonEmpty (NEIntSet)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Ord (Down (Down))
import Data.Semigroup (Max, Min)
import GHC.Generics (Generic)
import Swarm.Game.Location (Location, asVector)
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Structure.Named (NamedGrid, StructureName)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Static
import Swarm.Game.Universe (Cosmic, offsetBy)
import Swarm.Game.World.Coords (coordsToLoc)
import Swarm.Language.Syntax.Direction (AbsoluteDir)
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
type AtomicKeySymbol a = Maybe a

-- | A "needle" consisting row of cells within the haystack
-- (a sequence of rows) to be searched.
--
-- === Example
-- The complete row:
--
-- @
-- aab
-- @
type SymbolSequence a = [AtomicKeySymbol a]

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
data PositionWithinRow b a = PositionWithinRow
  { _position :: Int32
  -- ^ horizontal index of the entity within the row
  , structureRow :: ConsolidatedRowReferences b a
  }

-- | A chunkified version of a structure row.
-- Each unique structure row will need to test one of these
-- against the world row being examined.
data RowChunkMatchingReference b a = RowChunkMatchingReference
  { locatableRows :: ConsolidatedRowReferences b a
  , confirmationMap :: HashMap (NonEmpty a) (NonEmpty Int)
  }

data PiecewiseRecognition b a = PiecewiseRecognition
  { piecewiseSM :: StateMachine (AtomicKeySymbol a) (NonEmpty a)
  , picewiseLookup :: NonEmpty (RowChunkMatchingReference b a)
  -- ^ A lookup structure for use with results of the
  -- Aho-Corasick matcher. This lookup will determine whether
  -- the discontiguous "chunks" found by the matcher occur at
  -- the right positions with respect to the reference structure.
  }

data PositionedChunk a = PositionedChunk
  { chunkStartPos :: Int
  , chunkContents :: NonEmpty a
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
data SingleRowEntityOccurrences b a = SingleRowEntityOccurrences
  { myRow :: ConsolidatedRowReferences b a
  , myEntity :: a
  , contiguousChunks :: [PositionedChunk a]
  , expandedOffsets :: InspectionOffsets
  }

newtype RowWidth = RowWidth Int32
  deriving (Eq)

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
--
-- The two type parameters, `b` and `a`, correspond
-- to 'Cell' and 'Entity', respectively.
data StructureRow b a = StructureRow
  { wholeStructure :: StructureWithGrid b a
  , rowIndex :: Int32
  -- ^ vertical index of the row within the structure
  , rowContent :: NonEmpty (AtomicKeySymbol a)
  }

-- | Represents all rows across all structures that share
-- a particular row content
data ConsolidatedRowReferences b a = ConsolidatedRowReferences
  { sharedRowContent :: NonEmpty (AtomicKeySymbol a)
  , referencingRows :: NonEmpty (StructureRow b a)
  , theRowWidth :: RowWidth
  }

-- | This wrapper facilitates naming the original structure
-- (i.e. the "payload" for recognition)
-- for the purpose of both UI display and internal uniqueness,
-- while remaining agnostic to its internals.
data NamedOriginal b = NamedOriginal
  { getName :: StructureName
  , orig :: NamedGrid b
  }
  deriving (Show, Eq)

-- | The original definition of a structure, bundled
-- with its grid of cells having been extracted for convenience.
--
-- The two type parameters, `b` and `a`, correspond
-- to 'Cell' and 'Entity', respectively.
data StructureWithGrid b a = StructureWithGrid
  { originalDefinition :: NamedOriginal b
  , rotatedTo :: AbsoluteDir
  , gridWidth :: RowWidth
  , entityGrid :: NonEmptyGrid (AtomicKeySymbol a)
  }
  deriving (Eq)

-- | Structure definitions with precomputed metadata for consumption by the UI
data StructureInfo b a = StructureInfo
  { annotatedGrid :: SymmetryAnnotatedGrid b
  , entityProcessedGrid :: [SymbolSequence a]
  , entityCounts :: Map a Int
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

data AutomatonInfo v k = AutomatonInfo
  { _inspectionOffsets :: InspectionOffsets
  , _piecewiseRecognizer :: PiecewiseRecognition v k
  }
  deriving (Generic)

makeLenses ''AutomatonInfo

-- | The complete set of data needed to identify applicable
-- structures, based on a just-placed entity.
data RecognizerAutomatons b a = RecognizerAutomatons
  { _originalStructureDefinitions :: Map StructureName (StructureInfo b a)
  -- ^ all of the structures that shall participate in automatic recognition.
  -- This list is used only by the UI and by the 'Floorplan' command.
  , _automatonsByEntity :: HashMap a (AutomatonInfo b a)
  }
  deriving (Generic)

makeLenses ''RecognizerAutomatons

-- | Final output of the search process.
-- These are the elements that are stored in the 'FoundRegistry'.
--
-- The two type parameters, `b` and `a`, correspond
-- to 'Cell' and 'Entity', respectively.
type FoundStructure b a = PositionedStructure (StructureWithGrid b a)

-- | NOTE: A structure's name + orientation + position will uniquely
-- identify it in the world.  Note that position alone is not sufficient;
-- due to transparency, a completely intact smaller structure can co-exist
-- within a larger structure, both sharing the same upper-left coordinate.
-- However, two identical structures (with identical orientation) cannot
-- occupy the same space.
--
-- Compare "PositionedStructure OrientedStructure" to:
-- "Swarm.Game.Scenario.Topography.Structure.Recognition.Static.LocatedStructure"
data PositionedStructure s = FoundStructure
  { upperLeftCorner :: Cosmic Location
  , structureWithGrid :: s
  }
  deriving (Eq, Functor, Generic, ToJSON)

deriving instance (Ord (PositionedStructure OrientedStructure))

data FoundRowFromChunk a = FoundRowFromChunk
  { chunkOffsetFromSearchBorder :: Int
  , horizontalStructPos :: Int32
  , chunkStructure :: a
  }
  deriving (Functor, Generic, ToJSON)

-- | The located occurrences of a specific contiguous chunk of entities.
-- Note that an identical chunk may recur more than once in a structure row.
-- This record represents all of the recurrences of one such chunk.
--
-- Any different chunks contained within a row will be described by
-- their own instance of this record.
data FoundAndExpectedChunkPositions = FoundAndExpectedChunkPositions
  { foundPositions :: NEIntSet
  , expectedPositions :: NEIntSet
  }
  deriving (Generic, ToJSON)

data ChunkedRowMatch a e = ChunkedRowMatch
  { positionsComparison :: [(FoundAndExpectedChunkPositions, NonEmpty e)]
  , foundChunkRow :: FoundRowFromChunk a
  }
  deriving (Functor, Generic, ToJSON)

data EntityDiscrepancy e = EntityDiscrepancy
  { expectedEntity :: e
  , observedEntity :: AtomicKeySymbol e
  }
  deriving (Functor, Generic, ToJSON)

distillLabel :: StructureWithGrid b a -> OrientedStructure
distillLabel swg = OrientedStructure (getName $ originalDefinition swg) (rotatedTo swg)

data IntactnessFailureReason e
  = DiscrepantEntity (EntityDiscrepancy e)
  | AlreadyUsedBy OrientedStructure
  deriving (Functor, Generic, ToJSON)

data StructureIntactnessFailure e = StructureIntactnessFailure
  { failedOnIndex :: Location
  , totalSize :: AreaDimensions
  , reason :: IntactnessFailureReason e
  }
  deriving (Functor, Generic, ToJSON)

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
instance (Eq b, Eq a) => Ord (FoundStructure b a) where
  compare = compare `on` (f1 &&& f2)
   where
    f1 = computeArea . getNEGridDimensions . entityGrid . structureWithGrid
    f2 = Down . upperLeftCorner

-- | Yields coordinates that are occupied by an entity of a placed structure.
-- Cells within the rectangular bounds of the structure that are unoccupied
-- are not included.
genOccupiedCoords :: FoundStructure b a -> [Cosmic Location]
genOccupiedCoords (FoundStructure loc swg) =
  catMaybes . NE.toList . mapWithCoordsNE f $ entityGrid swg
 where
  -- replaces an "occupied" grid cell with its location
  f cellLoc maybeEnt = ((loc `offsetBy`) . asVector . coordsToLoc $ cellLoc) <$ maybeEnt
