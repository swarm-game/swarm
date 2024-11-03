{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types strictly for debugging structure recognition via the web interface
module Swarm.Game.Scenario.Topography.Structure.Recognition.Log where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD
import Swarm.Game.Location (Location)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Universe (Cosmic)
import Swarm.Language.Syntax.Direction (AbsoluteDir)

data OrientedStructure = OrientedStructure
  { oName :: OriginalName
  , oDir :: AbsoluteDir
  }
  deriving (Generic, ToJSON)

distillLabel :: StructureWithGrid b a -> OrientedStructure
distillLabel swg = OrientedStructure (getName $ originalDefinition swg) (rotatedTo swg)

renderSharedNames :: ConsolidatedRowReferences b a -> Text
renderSharedNames =
  T.intercalate "/" . NE.toList . NE.nub . NE.map (getName . originalDefinition . wholeStructure) . referencingRows

data ParticipatingEntity e = ParticipatingEntity
  { entity :: e
  , searchOffsets :: InspectionOffsets
  }
  deriving (Functor, Generic, ToJSON)

data IntactPlacementLog e = IntactPlacementLog
  { intactnessFailure :: Maybe (StructureIntactnessFailure e)
  , sName :: OriginalName
  , locUpperLeft :: Cosmic Location
  }
  deriving (Functor, Generic, ToJSON)

data ChunkMatchFailureReason e
  = ChunkMatchFailureReason OriginalName (RowMismatchReason e)
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
  | ChunksMatchingExpected [ChunkedRowMatch OriginalName e]
  | ChunkFailures [ChunkMatchFailureReason e]
  | ChunkIntactnessVerification (IntactPlacementLog e)
  | StructureRemoved OriginalName
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

data StructureLocation = StructureLocation OriginalName (Cosmic Location)
  deriving (Generic, ToJSON)

instance ToSample StructureLocation where
  toSamples _ = SD.noSamples
