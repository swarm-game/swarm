{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Swarm.Game.Scenario.Status where

import Control.Lens hiding (from, (<.>))
import Data.Aeson (
  genericParseJSON,
  genericToEncoding,
  genericToJSON,
 )
import Data.Function (on)
import Data.Time (ZonedTime, diffUTCTime, zonedTimeToUTC)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.Scenario.Scoring.Metrics

data ProgressMetric = ProgressMetric
  { _scenarioStarted :: ZonedTime
  -- ^ Time when the scenario was started including time zone.
  , _scenarioAttemptMetrics :: AttemptMetrics
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON ProgressMetric where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON ProgressMetric where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

-- | A @ScenarioStatus@ stores the status of a scenario along with
--   appropriate metadata: not started, in progress, or complete.
--   Note that "in progress" is currently a bit of a misnomer since
--   games cannot be saved; at the moment it really means more like
--   "you played this scenario before but didn't win".
data ScenarioStatus
  = NotStarted
  | InProgress ProgressMetric
  | Complete ProgressMetric
  deriving (Eq, Ord, Show, Read, Generic)

-- Some orphan ZonedTime instances

instance Eq ZonedTime where
  (==) = (==) `on` zonedTimeToUTC

instance Ord ZonedTime where
  (<=) = (<=) `on` zonedTimeToUTC

instance FromJSON ScenarioStatus where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON ScenarioStatus where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

data BestRecords = BestRecords
  { _scenarioBestByTime :: ScenarioStatus
  , _scenarioBestByTicks :: ScenarioStatus
  , _scenarioBestByCharCount :: ScenarioStatus
  , _scenarioBestByAstSize :: ScenarioStatus
  }
  deriving (Eq, Ord, Show, Read, Generic)

makeLensesWith (lensRules & generateSignatures .~ False) ''BestRecords

-- | The best status of the scenario, measured in real world time.
scenarioBestByTime :: Lens' BestRecords ScenarioStatus

-- | The best status of the scenario, measured in game ticks.
scenarioBestByTicks :: Lens' BestRecords ScenarioStatus

-- | The best code size of the scenario, measured both in character count and AST size.
scenarioBestByCharCount :: Lens' BestRecords ScenarioStatus

-- | The best code size of the scenario, measured both in character count and AST size.
scenarioBestByAstSize :: Lens' BestRecords ScenarioStatus

instance FromJSON BestRecords where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON BestRecords where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

-- | A @ScenarioInfo@ record stores metadata about a scenario: its
--   canonical path, most recent status, and best-ever status.
data ScenarioInfo = ScenarioInfo
  { _scenarioPath :: FilePath
  , _scenarioStatus :: ScenarioStatus
  , _scenarioBestRecords :: BestRecords
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON ScenarioInfo where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON ScenarioInfo where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

type ScenarioInfoPair = (Scenario, ScenarioInfo)

makeLensesWith (lensRules & generateSignatures .~ False) ''ScenarioInfo

-- | The path of the scenario, relative to @data/scenarios@.
scenarioPath :: Lens' ScenarioInfo FilePath

-- | The status of the scenario.
scenarioStatus :: Lens' ScenarioInfo ScenarioStatus

-- | The records for best scenario attempt by various metrics
scenarioBestRecords :: Lens' ScenarioInfo BestRecords

-- | Update the current @ScenarioInfo@ record when quitting a game.
--
-- Note that when comparing "best" times, shorter is not always better!
-- As long as the scenario is not completed (e.g. some do not have win condition)
-- we consider having fun _longer_ to be better.
updateScenarioInfoOnQuit ::
  CodeSizeDeterminators ->
  ZonedTime ->
  Integer ->
  Bool ->
  ScenarioInfo ->
  ScenarioInfo
updateScenarioInfoOnQuit
  csd
  z
  ticks
  completed
  (ScenarioInfo p s (BestRecords bTime bTicks _prevCharCount _prevAstSize)) = case s of
    InProgress (ProgressMetric start _) ->
      let el = (diffUTCTime `on` zonedTimeToUTC) z start
          cs = codeSizeFromDeterminator csd
          arg = ProgressMetric start $ AttemptMetrics (DurationMetrics el ticks) cs
          cur = (if completed then Complete else InProgress) arg
       in ScenarioInfo p cur $
            BestRecords -- TODO FIXME
              cur
              cur
              cur
              cur
    _ -> error "Logical error: trying to quit scenario which is not in progress!"
