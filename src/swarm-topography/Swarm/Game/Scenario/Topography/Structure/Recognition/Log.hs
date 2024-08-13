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
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Universe (Cosmic)
import Swarm.Language.Syntax.Direction (AbsoluteDir)

type StructureRowContent e = [Maybe e]
type WorldRowContent e = [Maybe e]

data OrientedStructure = OrientedStructure
  { oName :: OriginalName
  , oDir :: AbsoluteDir
  }
  deriving (Generic, ToJSON)

distillLabel :: StructureWithGrid b a -> OrientedStructure
distillLabel swg = OrientedStructure (getName $ originalDefinition swg) (rotatedTo swg)

data MatchingRowFrom = MatchingRowFrom
  { rowIdx :: Int32
  , structure :: OrientedStructure
  }
  deriving (Generic, ToJSON)

newtype HaystackPosition = HaystackPosition Int
  deriving (Generic, ToJSON)

data HaystackContext e = HaystackContext
  { maskedWorldRow :: WorldRowContent e
  -- ^ entities that do not constitute any of the eligible structures
  -- are replaced with 'null' in this list.
  , haystackPosition :: HaystackPosition
  }
  deriving (Functor, Generic, ToJSON)

data FoundRowCandidate e = FoundRowCandidate
  { haystackContext :: HaystackContext e
  , structureContent :: StructureRowContent e
  , rowCandidates :: [MatchingRowFrom]
  }
  deriving (Functor, Generic, ToJSON)

data ParticipatingEntity e = ParticipatingEntity
  { entity :: e
  , searchOffsets :: InspectionOffsets
  }
  deriving (Functor, Generic, ToJSON)

data IntactPlacementLog = IntactPlacementLog
  { isIntact :: Bool
  , sName :: OriginalName
  , locUpperLeft :: Cosmic Location
  }
  deriving (Generic, ToJSON)

data SearchLog e
  = FoundParticipatingEntity (ParticipatingEntity e)
  | StructureRemoved OriginalName
  | FoundRowCandidates [FoundRowCandidate e]
  | FoundCompleteStructureCandidates [OrientedStructure]
  | -- | There may be multiple candidate structures that could be
    -- completed by the element that was just placed. This lists all of them.
    VerticalSearchSpans [(InspectionOffsets, [OrientedStructure])]
  | IntactStaticPlacement [IntactPlacementLog]
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
