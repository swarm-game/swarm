-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types strictly for debugging structure recognition via the web interface
module Swarm.Game.Scenario.Topography.Structure.Recognition.Log where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty.Extra qualified as NE
import GHC.Generics (Generic)
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD
import Swarm.Game.Location (Location)
import Swarm.Game.Scenario.Topography.Structure.Named (StructureName, name)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Static (
  OrientedStructure,
 )
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Universe (Cosmic)

renderSharedNames :: ConsolidatedRowReferences b a -> NonEmpty StructureName
renderSharedNames =
  NE.nubOrd . NE.map (name . originalItem . entityGrid . wholeStructure) . referencingRows

data ParticipatingEntity e = ParticipatingEntity
  { entity :: e
  , searchOffsets :: InspectionOffsets
  }
  deriving (Functor, Generic, ToJSON)

data IntactPlacementLog e = IntactPlacementLog
  { intactnessFailure :: Maybe (StructureIntactnessFailure e)
  , placedStructure :: PositionedStructure OrientedStructure
  }
  deriving (Functor, Generic, ToJSON)

data ChunkMatchFailureReason e
  = ChunkMatchFailureReason (NonEmpty StructureName) (RowMismatchReason e)
  deriving (Functor, Generic, ToJSON)

data FoundChunkComparison e = FoundChunkComparison
  { foundChunkKeys :: [NonEmpty e]
  , referenceChunkKeys :: [NonEmpty e]
  }
  deriving (Functor, Generic, ToJSON)

data RowMismatchReason e
  = NoKeysSubset (FoundChunkComparison e)
  | -- | NOTE: we should never see 'EmptyIntersection',
    -- since the earlier 'NoKeysSubset' condition
    -- results in an empty intersection
    EmptyIntersection
  deriving (Functor, Generic, ToJSON)

data SearchLog e
  = IntactStaticPlacement [IntactPlacementLog e]
  | StartSearchAt (Cosmic Location) InspectionOffsets
  | FoundParticipatingEntity (ParticipatingEntity e)
  | FoundCompleteStructureCandidates [(OrientedStructure, Cosmic Location)]
  | -- | this is actually internally used as a (Map (NonEmpty e) (NonEmpty Int)),
    -- but the requirements of Functor force us to invert the mapping
    FoundPiecewiseChunks [(NonEmpty Int, NonEmpty e)]
  | ExpectedChunks (NonEmpty [NonEmpty e])
  | WorldRowContent [Maybe e]
  | ChunksMatchingExpected [ChunkedRowMatch (NonEmpty StructureName) e]
  | ChunkFailures [ChunkMatchFailureReason e]
  | ChunkIntactnessVerification (IntactPlacementLog e)
  | StructureRemoved StructureName
  deriving (Functor, Generic)

instance (ToJSON e) => ToJSON (SearchLog e) where
  toJSON = genericToJSON searchLogOptions

searchLogOptions :: Options
searchLogOptions =
  defaultOptions
    { sumEncoding = ObjectWithSingleField
    }

instance ToSample (SearchLog e) where
  toSamples _ = SD.noSamples

data StructureLocation = StructureLocation StructureName (Cosmic Location)
  deriving (Generic, ToJSON)

instance ToSample StructureLocation where
  toSamples _ = SD.noSamples
