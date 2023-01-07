{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Swarm.Game.Scenario.Status where

import Control.Arrow ((&&&))
import Control.Lens hiding (from, (<.>))
import Data.Aeson (
  genericParseJSON,
  genericToEncoding,
  genericToJSON,
 )
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time (ZonedTime, diffUTCTime, zonedTimeToUTC)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.Scenario.Scoring.Metrics
import Swarm.Game.Scenario.Scoring.Progress

-- * Some orphan ZonedTime instances

instance Eq ZonedTime where
  (==) = (==) `on` zonedTimeToUTC

instance Ord ZonedTime where
  (<=) = (<=) `on` zonedTimeToUTC

-- * High scores by various criteria

data BestByCriteria
  = BestByTime
  | BestByTicks
  | BestByCharCount
  | BestByAstSize
  deriving (Eq, Ord, Show)

describeCriteria :: BestByCriteria -> Text
describeCriteria = \case
  BestByTime -> "time"
  BestByTicks -> "ticks"
  BestByCharCount -> "char count"
  BestByAstSize -> "AST size"

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

-- * High scores by various criteria

data BestRecords = BestRecords
  { _scenarioBestByTime :: ProgressMetric
  , _scenarioBestByTicks :: ProgressMetric
  , _scenarioBestByCharCount :: ProgressMetric
  , _scenarioBestByAstSize :: ProgressMetric
  }
  deriving (Eq, Ord, Read, Generic)

angleBracket :: String -> String
angleBracket x = "<" <> x <> ">"

instance Show BestRecords where
  show (BestRecords a b c d) =
    unlines $
      map
        angleBracket
        [ show a
        , show b
        , show c
        , show d
        ]

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
  bestTime x y = f x (Just . view (scenarioAttemptMetrics . scenarioDurationMetrics . y))
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
  M.fromList $ durationElements <> mapMaybe (traverse ensurePresent) codeSizeElements
 where
  durationElements =
    [ (BestByTime, t1)
    , (BestByTicks, t2)
    ]
  codeSizeElements =
    [ (BestByCharCount, s1)
    , (BestByAstSize, s2)
    ]

  ensurePresent x =
    (getMetric x ^. scenarioAttemptMetrics . scenarioCodeMetrics) >> Just x

-- | Uses the start time of the play-attempt to de-dupe
-- records that are from the same game. The start time should
-- be sufficient to uniquely identify a game.
getBestGroups ::
  BestRecords ->
  [(Metric ProgressStats, NonEmpty BestByCriteria)]
getBestGroups =
  rearrangeTuples . M.toList . bestToMap
 where
  groupByStartTime = NE.groupAllWith $ view scenarioStarted . getMetric . snd
  rearrangeTuples = map (snd . NE.head &&& NE.map fst) . groupByStartTime

-- * High-level status of scenario play

-- | A "ScenarioStatus" stores the status of a scenario along with
--   appropriate metadata: "NotStarted", or "Played".
--   The "Played" status has two sub-states: "Attempted" or "Completed".
data ScenarioStatus
  = NotStarted
  | Played ProgressMetric BestRecords
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON ScenarioStatus where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON ScenarioStatus where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

-- | A "ScenarioInfo" record stores metadata about a scenario: its
-- canonical path and status.
-- By way of the "ScenarioStatus" record, it stores the
-- most recent status and best-ever status.
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

-- | Update the current "ScenarioInfo" record when quitting a game.
--
-- Note that when comparing \"best\" times, shorter is not always better!
-- As long as the scenario is not completed (e.g. some do not have win condition)
-- we consider having fun _longer_ to be better.
updateScenarioInfoOnFinish ::
  CodeSizeDeterminators ->
  ZonedTime ->
  Integer ->
  Bool ->
  ScenarioInfo ->
  ScenarioInfo
updateScenarioInfoOnFinish
  csd
  z
  ticks
  completed
  si@(ScenarioInfo p prevPlayState) = case prevPlayState of
    Played (Metric _ (ProgressStats start _currentPlayMetrics)) prevBestRecords ->
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
    _ -> si
