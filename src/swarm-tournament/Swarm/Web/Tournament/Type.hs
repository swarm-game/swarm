{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for Swarm tournaments.
module Swarm.Web.Tournament.Type where

import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime)
import Database.SQLite.Simple.ToField
import GHC.Generics (Generic)
import Servant
import Servant.Docs (ToCapture)
import Servant.Docs qualified as SD
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.State (Sha1 (..))
import Swarm.Game.Tick (TickNumber (..))
import Swarm.Game.World (Seed)
import System.Time.Extra

newtype UserAlias = UserAlias TL.Text
  deriving stock Generic
  deriving newtype ToJSON

instance ToField UserAlias where
  toField (UserAlias x) = toField x

instance ToField Sha1 where
  toField (Sha1 x) = toField x

data FileMetadata = FileMetadata
  { filename :: T.Text
  , fileHash :: Sha1
  }
  deriving (Generic, ToJSON)

data FileUpload = FileUpload
  { fileContent :: LBS.ByteString
  , fileMetadata :: FileMetadata
  }

data TournamentGame = TournamentGame
  { originalFilename :: T.Text
  , scenarioUploader :: T.Text
  , scenarioHash :: Sha1
  , submissionCount :: Int
  , swarmGitSha1 :: Sha1
  , scenarioTitle :: T.Text
  }
  deriving (Generic, ToJSON)

data TournamentSolution = TournamentSolution
  { submissionTime :: UTCTime
  , solutionSubmitter :: T.Text
  , submissionScore :: SolutionFileCharacterization
  }
  deriving (Generic, ToJSON)

data GameWithSolutions = GameWithSolutions
  { theGame :: TournamentGame
  , theSolutions :: [TournamentSolution]
  }
  deriving (Generic, ToJSON)

data AssociatedSolutionCharacterization = AssociatedSolutionCharacterization
  { forScenario :: Sha1
  , characterization :: SolutionCharacterization
  }

data SolutionCharacterization = SolutionCharacterization
  { solutionWallTime :: Seconds
  , solutionTicks :: TickNumber
  , scenarioSeed :: Seed
  , solutionCodeMetrics :: ScenarioCodeMetrics
  }
  deriving (Generic, ToJSON)

data SolutionFileCharacterization = SolutionFileCharacterization
  { solutionHash :: Sha1
  , solutionCharacterization :: SolutionCharacterization
  }
  deriving (Generic, ToJSON)

data ScenarioCharacterization = ScenarioCharacterization
  { scenarioFileMetadata :: FileMetadata
  , builtinSolution :: SolutionCharacterization
  }
  deriving (Generic, ToJSON)

instance FromHttpApiData Sha1 where
  parseUrlPiece = return . Sha1 . T.unpack

instance ToCapture (Capture "sha1" Sha1) where
  toCapture _ =
    SD.DocCapture
      "sha1" -- name
      "(text) scenario sha1" -- description
