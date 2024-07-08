-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types strictly for debugging structure recognition via the web interface
module Swarm.Game.Scenario.Topography.Structure.Recognition.Log where

import Data.Aeson
import Data.Int (Int32)
import GHC.Generics (Generic)
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD
import Swarm.Game.Location (Location)
import Swarm.Game.Scenario.Topography.Placement (StructureName)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Universe (Cosmic)

type StructureRowContent en = [Maybe en]
type WorldRowContent en = [Maybe en]

data MatchingRowFrom = MatchingRowFrom
  { rowIdx :: Int32
  , structure :: StructureName
  }
  deriving (Generic, ToJSON)

newtype HaystackPosition = HaystackPosition Int
  deriving (Generic, ToJSON)

data HaystackContext en = HaystackContext
  { worldRow :: WorldRowContent en
  , haystackPosition :: HaystackPosition
  }
  deriving (Generic, ToJSON)

data FoundRowCandidate en = FoundRowCandidate
  { haystackContext :: HaystackContext en
  , structureContent :: StructureRowContent en
  , rowCandidates :: [MatchingRowFrom]
  }
  deriving (Generic, ToJSON)

data ParticipatingEntity en = ParticipatingEntity
  { entity :: en
  , searchOffsets :: InspectionOffsets
  }
  deriving (Generic, ToJSON)

data IntactPlacementLog = IntactPlacementLog
  { isIntact :: Bool
  , sName :: StructureName
  , locUpperLeft :: Cosmic Location
  }
  deriving (Generic, ToJSON)

data SearchLog en
  = FoundParticipatingEntity (ParticipatingEntity en)
  | StructureRemoved StructureName
  | FoundRowCandidates [FoundRowCandidate en]
  | FoundCompleteStructureCandidates [StructureName]
  | IntactStaticPlacement [IntactPlacementLog]
  deriving (Generic)

instance (ToJSON en) => ToJSON (SearchLog en) where
  toJSON = genericToJSON searchLogOptions

searchLogOptions :: Options
searchLogOptions =
  defaultOptions
    { sumEncoding = ObjectWithSingleField
    }

instance ToSample (SearchLog en) where
  toSamples _ = SD.noSamples

data StructureLocation = StructureLocation StructureName (Cosmic Location)
  deriving (Generic, ToJSON)

instance ToSample StructureLocation where
  toSamples _ = SD.noSamples
