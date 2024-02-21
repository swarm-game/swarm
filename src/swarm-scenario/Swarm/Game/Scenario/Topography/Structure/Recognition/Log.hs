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
import Swarm.Game.Entity (EntityName)
import Swarm.Game.Location (Location)
import Swarm.Game.Scenario.Topography.Placement (StructureName)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Universe (Cosmic)

type StructureRowContent = [Maybe EntityName]
type WorldRowContent = [Maybe EntityName]

data MatchingRowFrom = MatchingRowFrom
  { rowIdx :: Int32
  , structure :: StructureName
  }
  deriving (Generic, ToJSON)

newtype HaystackPosition = HaystackPosition Int
  deriving (Generic, ToJSON)

data HaystackContext = HaystackContext
  { worldRow :: WorldRowContent
  , haystackPosition :: HaystackPosition
  }
  deriving (Generic, ToJSON)

data FoundRowCandidate = FoundRowCandidate
  { haystackContext :: HaystackContext
  , structureContent :: StructureRowContent
  , rowCandidates :: [MatchingRowFrom]
  }
  deriving (Generic, ToJSON)

data ParticipatingEntity = ParticipatingEntity
  { entity :: EntityName
  , searchOffsets :: InspectionOffsets
  }
  deriving (Generic, ToJSON)

data IntactPlacementLog = IntactPlacementLog
  { isIntact :: Bool
  , sName :: StructureName
  , locUpperLeft :: Cosmic Location
  }
  deriving (Generic, ToJSON)

data SearchLog
  = FoundParticipatingEntity ParticipatingEntity
  | StructureRemoved StructureName
  | FoundRowCandidates [FoundRowCandidate]
  | FoundCompleteStructureCandidates [StructureName]
  | IntactStaticPlacement [IntactPlacementLog]
  deriving (Generic)

instance ToJSON SearchLog where
  toJSON = genericToJSON searchLogOptions

searchLogOptions :: Options
searchLogOptions =
  defaultOptions
    { sumEncoding = ObjectWithSingleField
    }

instance ToSample SearchLog where
  toSamples _ = SD.noSamples

data StructureLocation = StructureLocation StructureName (Cosmic Location)
  deriving (Generic, ToJSON)

instance ToSample StructureLocation where
  toSamples _ = SD.noSamples
