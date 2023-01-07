{-# LANGUAGE TemplateHaskell #-}

module Swarm.Game.Scenario.Scoring.Metrics where

import Control.Lens hiding (from, (<.>))
import Data.Aeson
import Data.Char (toLower)
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)
import Swarm.Game.Scenario.Scoring.CodeSize

data BestByCriteria
  = BestByTime
  | BestByTicks
  | BestByCharCount
  | BestByAstSize

scenarioOptions :: Options
scenarioOptions =
  defaultOptions
    { fieldLabelModifier = map toLower . drop (length "_scenario")
    }

data DurationMetrics = DurationMetrics
  { _scenarioElapsed :: NominalDiffTime
  -- ^ Time elapsed until quitting the scenario.
  , _scenarioElapsedTicks :: Integer
  -- ^ Ticks elapsed until quitting the scenario.
  }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''DurationMetrics

emptyDurationMetric :: DurationMetrics
emptyDurationMetric = DurationMetrics 0 0

instance FromJSON DurationMetrics where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON DurationMetrics where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

data AttemptMetrics = AttemptMetrics
  { _scenarioDurationMetrics :: DurationMetrics
  , _scenarioCodeMetrics :: Maybe ScenarioCodeMetrics
  -- ^ Size of the user's program.
  }
  deriving (Eq, Ord, Show, Read, Generic)

emptyAttemptMetric :: AttemptMetrics
emptyAttemptMetric = AttemptMetrics emptyDurationMetric Nothing

makeLenses ''AttemptMetrics

instance FromJSON AttemptMetrics where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON AttemptMetrics where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions
