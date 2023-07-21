{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | High-level status of scenario play.
-- Representation of progress, logic for updating.
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
import Swarm.Game.CESK (TickNumber)
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Scoring.Best
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.Scenario.Scoring.ConcreteMetrics
import Swarm.Game.Scenario.Scoring.GenericMetrics
import Swarm.Game.World.Gen (Seed)
import Swarm.Util.Lens (makeLensesNoSigs)

-- | These launch parameters are used in a number of ways:
-- * Serializing the seed/script path for saves
-- * Holding parse status from form fields, including Error info
-- * Carrying fully-validated launch parameters.
--
-- Type parameters are utilized to support all of these use cases.
data ParameterizableLaunchParams code f = LaunchParams
  { seedVal :: f (Maybe Seed)
  , initialCode :: f (Maybe code)
  }

type SerializableLaunchParams = ParameterizableLaunchParams FilePath Identity
deriving instance Eq SerializableLaunchParams
deriving instance Ord SerializableLaunchParams
deriving instance Show SerializableLaunchParams
deriving instance Read SerializableLaunchParams
deriving instance Generic SerializableLaunchParams
deriving instance FromJSON SerializableLaunchParams
deriving instance ToJSON SerializableLaunchParams

-- | A "ScenarioStatus" stores the status of a scenario along with
--   appropriate metadata: "NotStarted", or "Played".
--   The "Played" status has two sub-states: "Attempted" or "Completed".
data ScenarioStatus
  = NotStarted
  | Played
      SerializableLaunchParams
      ProgressMetric
      BestRecords
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON ScenarioStatus where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON ScenarioStatus where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

getLaunchParams :: ScenarioStatus -> SerializableLaunchParams
getLaunchParams = \case
  NotStarted -> LaunchParams (pure Nothing) (pure Nothing)
  Played x _ _ -> x

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

makeLensesNoSigs ''ScenarioInfo

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
  TickNumber ->
  Bool ->
  ScenarioInfo ->
  ScenarioInfo
updateScenarioInfoOnFinish
  csd
  z
  ticks
  completed
  si@(ScenarioInfo p prevPlayState) = case prevPlayState of
    Played launchParams (Metric _ (ProgressStats start _currentPlayMetrics)) prevBestRecords ->
      ScenarioInfo p $
        Played launchParams newPlayMetric $
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
