-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types strictly for debugging structure recognition via the web interface
module Swarm.Game.Scenario.Topography.Structure.Recognition.Log where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Int (Int32)
import GHC.Generics (Generic)
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD
import Swarm.Game.Location (Location)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type
import Swarm.Game.Universe (Cosmic)
import Swarm.Language.Syntax.Direction (AbsoluteDir)

-- | Type aliases for documentation
type StructureRowContent e = SymbolSequence e

type WorldRowContent e = SymbolSequence e

data OrientedStructure = OrientedStructure
  { oName :: OriginalName
  , oDir :: AbsoluteDir
  }
  deriving (Generic, ToJSON)

distillLabel :: StructureWithGrid b a -> OrientedStructure
distillLabel swg = OrientedStructure (getName $ originalDefinition swg) (rotatedTo swg)

data MatchingRowFrom = MatchingRowFrom
  { topDownRowIdx :: Int32
  -- ^ numbered from the top down
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
  , soughtContent :: StructureRowContent e
  , matchedCandidates :: [MatchingRowFrom]
  }
  deriving (Functor, Generic, ToJSON)

data EntityKeyedFinder e = EntityKeyedFinder {
    searchOffsets :: InspectionOffsets
  , candidateStructureRows :: [StructureRowContent e]
  , entityMask :: [e]
     -- ^ NOTE: HashSet has no Functor instance,
     -- so we represent this as a list here.
  }
  deriving (Functor, Generic, ToJSON)

data ParticipatingEntity e = ParticipatingEntity
  { entity :: e
  , entityKeyedFinders :: NonEmpty (EntityKeyedFinder e)
  }
  deriving (Functor, Generic, ToJSON)

data IntactPlacementLog = IntactPlacementLog
  { isIntact :: Bool
  , sName :: OriginalName
  , locUpperLeft :: Cosmic Location
  }
  deriving (Generic, ToJSON)

data VerticalSearch e = VerticalSearch
  { haystackVerticalExtents :: InspectionOffsets
  -- ^ vertical offset of haystack relative to the found row
  , soughtStructures :: [OrientedStructure]
  , verticalHaystack :: [WorldRowContent e]
  }
  deriving (Functor, Generic, ToJSON)

data SearchLog e
  = FoundParticipatingEntity (ParticipatingEntity e)
  | StructureRemoved OriginalName
  | FoundRowCandidates [FoundRowCandidate e]
  | FoundCompleteStructureCandidates [OrientedStructure]
  | -- | There may be multiple candidate structures that could be
    -- completed by the element that was just placed. This lists all of them.
    VerticalSearchSpans [VerticalSearch e]
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
