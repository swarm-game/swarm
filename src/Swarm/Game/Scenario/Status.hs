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
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Time (ZonedTime, diffUTCTime, zonedTimeToUTC)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.Scenario.Scoring.Metrics
import Swarm.Game.Scenario.Scoring.Progress

data BestByCriteria
  = BestByTime
  | BestByTicks
  | BestByCharCount
  | BestByAstSize
  deriving (Eq, Ord, Show)

-- * Some orphan ZonedTime instances

instance Eq ZonedTime where
  (==) = (==) `on` zonedTimeToUTC

instance Ord ZonedTime where
  (<=) = (<=) `on` zonedTimeToUTC

data ProgressStats = ProgressStats
  { _scenarioStarted :: ZonedTime
  -- ^ Time when the scenario was started including time zone.
  , _scenarioAttemptMetrics :: AttemptMetrics
  }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''ProgressStats

instance FromJSON ProgressStats where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON ProgressStats where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

type ProgressMetric = Metric ProgressStats

data BestRecords = BestRecords
  { _scenarioBestByTime :: ProgressMetric
  , _scenarioBestByTicks :: ProgressMetric
  , _scenarioBestByCharCount :: ProgressMetric
  , _scenarioBestByAstSize :: ProgressMetric
  }
  deriving (Eq, Ord, Show, Read, Generic)

emptyBest :: ZonedTime -> BestRecords
emptyBest t = BestRecords x x x x
 where
  x = Metric Attempted $ ProgressStats t emptyAttemptMetric

updateBest :: ProgressMetric -> BestRecords -> BestRecords
updateBest newPlayMetric (BestRecords oldA oldB oldC oldD) =
  BestRecords
    (bestTime oldA scenarioElapsed)
    (bestTime oldB scenarioElapsedTicks)
    (bestSize oldC sourceTextLength)
    (bestSize oldD astSize)
 where
  f x y = chooseBetter y newPlayMetric x
  bestTime x y = f x (view $ scenarioAttemptMetrics . scenarioDurationMetrics . y)
  bestSize x y = f x (fmap y . view (scenarioAttemptMetrics . scenarioCodeMetrics))

makeLensesWith (lensRules & generateSignatures .~ False) ''BestRecords

-- | The best status of the scenario, measured in real world time.
scenarioBestByTime :: Lens' BestRecords ProgressMetric

-- | The best status of the scenario, measured in game ticks.
scenarioBestByTicks :: Lens' BestRecords ProgressMetric

-- | The best code size of the scenario, measured both in character count and AST size.
scenarioBestByCharCount :: Lens' BestRecords ProgressMetric

-- | The best code size of the scenario, measured both in character count and AST size.
scenarioBestByAstSize :: Lens' BestRecords ProgressMetric

instance FromJSON BestRecords where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON BestRecords where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

bestToMap :: BestRecords -> Map BestByCriteria ProgressMetric
bestToMap (BestRecords t1 t2 s1 s2) =
  M.fromList $
    catMaybes
      [ Just (BestByTime, t1)
      , Just (BestByTicks, t2)
      , sequenceA (BestByCharCount, ensurePresent s1)
      , sequenceA (BestByAstSize, ensurePresent s2)
      ]
 where
  ensurePresent x =
    (getMetric x ^. scenarioAttemptMetrics . scenarioCodeMetrics) >> Just x

-- | A @ScenarioStatus@ stores the status of a scenario along with
--   appropriate metadata: not started, in progress, or complete.
--   Note that "in progress" is currently a bit of a misnomer since
--   games cannot be saved; at the moment it really means more like
--   "you played this scenario before but didn't win".
data ScenarioStatus
  = NotStarted
  | Played ProgressMetric BestRecords
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON ScenarioStatus where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON ScenarioStatus where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

-- | A @ScenarioInfo@ record stores metadata about a scenario: its
--   canonical path, most recent status, and best-ever status.
data ScenarioInfo = ScenarioInfo
  { _scenarioPath :: FilePath
  , _scenarioStatus :: ScenarioStatus
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
  (ScenarioInfo p prevPlayState) = case prevPlayState of
    Played (Metric Attempted (ProgressStats start _currentPlayMetrics)) prevBestRecords ->
      ScenarioInfo p $
        Played newPlayMetric $
          updateBest newPlayMetric prevBestRecords
     where
      el = (diffUTCTime `on` zonedTimeToUTC) z start
      cs = codeSizeFromDeterminator csd
      newCompletionFlag = if completed then Completed else Attempted
      newPlayMetric =
        Metric newCompletionFlag $
          ProgressStats start $
            AttemptMetrics (DurationMetrics el ticks) cs
    _ -> error "Logical error: trying to quit scenario which is not in progress!"
