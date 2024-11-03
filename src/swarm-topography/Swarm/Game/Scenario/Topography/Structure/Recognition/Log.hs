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

newtype EntityKeyedFinder = EntityKeyedFinder
  { searchOffsets :: InspectionOffsets
  }
  deriving (Generic, ToJSON)

data ParticipatingEntity e = ParticipatingEntity
  { entity :: e
  , entityKeyedFinders :: EntityKeyedFinder
  }
  deriving (Functor, Generic, ToJSON)

data IntactPlacementLog = IntactPlacementLog
  { intactnessFailure :: Maybe StructureIntactnessFailure
  , sName :: OriginalName
  , locUpperLeft :: Cosmic Location
  }
  deriving (Generic, ToJSON)

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
  | -- | NOTE: should be redundant with 'NoKeysSubset'
    EmptyIntersection
  deriving (Functor, Generic, ToJSON)

data SearchLog e
  = IntactStaticPlacement [IntactPlacementLog]
  | StartSearchAt (Cosmic Location) InspectionOffsets
  | FoundParticipatingEntity (ParticipatingEntity e)
  | FoundCompleteStructureCandidates [(OrientedStructure, Cosmic Location)]
  | -- | this is actually internally used as a (Map (NonEmpty e) (NonEmpty Int)),
    -- but the requirements of Functor force us to invert the mapping
    FoundPiecewiseChunks [(NonEmpty Int, NonEmpty e)]
  | ExpectedChunks (NonEmpty [NonEmpty e])
  | ChunksMatchingExpected [ChunkedRowMatch OriginalName e]
  | ChunkFailures [ChunkMatchFailureReason e]
  | ChunkIntactnessVerification IntactPlacementLog
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
