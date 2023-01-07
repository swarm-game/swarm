{-# LANGUAGE OverloadedStrings #-}

-- | High score records
module TestScoring where

import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.Scenario.Scoring.Metrics
import Swarm.Game.Scenario.Scoring.Progress
import Swarm.Game.Scenario.Status
import Test.Tasty
import Test.Tasty.HUnit

testHighScores :: TestTree
testHighScores =
  testGroup
    "Precedence"
    [ testGroup
        "Single metrics"
        [ testCase
            "Attempted long > Attempted short"
            $ assertEqual "Longer is better for incomplete games" (Metric Attempted (5 :: Int))
            $ chooseBetter pure (Metric Attempted 3) (Metric Attempted 5)
        , testCase
            "Completed short > Completed long"
            $ assertEqual "Shorter is better for completed games" (Metric Completed (3 :: Int))
            $ chooseBetter pure (Metric Completed 3) (Metric Completed 5)
        ]
    , testGroup
        "Grouped metrics"
        [ betterReplTimeAfterCodeSizeRecord
        , betterCodeWorseTime
        ]
    ]

betterReplTimeAfterCodeSizeRecord :: TestTree
betterReplTimeAfterCodeSizeRecord =
  testCase
    "new repl win after code size record"
    $ assertEqual "incorrect" newExpectedBest newActualBest
 where
  mkZonedTime t = ZonedTime (LocalTime (fromOrdinalDate 2023 1) (TimeOfDay 0 t 0)) utc
  newRunWithoutCodeSize =
    Metric Completed $
      ProgressStats (mkZonedTime 1) $
        AttemptMetrics
          (DurationMetrics 1 1)
          Nothing

  oldCompletedRunWithCodeSize =
    Metric Completed $
      ProgressStats (mkZonedTime 0) $
        AttemptMetrics
          (DurationMetrics 2 2)
          (Just $ ScenarioCodeMetrics 1 1)

  oldBestWithCodeSize =
    BestRecords
      oldCompletedRunWithCodeSize
      oldCompletedRunWithCodeSize
      oldCompletedRunWithCodeSize
      oldCompletedRunWithCodeSize

  newExpectedBest =
    BestRecords
      newRunWithoutCodeSize
      newRunWithoutCodeSize
      oldCompletedRunWithCodeSize
      oldCompletedRunWithCodeSize

  newActualBest =
    updateBest
      newRunWithoutCodeSize
      oldBestWithCodeSize

betterCodeWorseTime :: TestTree
betterCodeWorseTime =
  testCase
    "improvement upon code size with a worse time"
    $ assertEqual "incorrect" newExpectedBests newActualBests
 where
  mkZonedTime t = ZonedTime (LocalTime (fromOrdinalDate 2023 1) (TimeOfDay 0 t 0)) utc
  newRunBetterCodeSize =
    Metric Completed $
      ProgressStats (mkZonedTime 1) $
        AttemptMetrics
          (DurationMetrics 2 2)
          (Just $ ScenarioCodeMetrics 1 1)

  oldRunPoorCodeSize =
    Metric Completed $
      ProgressStats (mkZonedTime 0) $
        AttemptMetrics
          (DurationMetrics 1 1)
          (Just $ ScenarioCodeMetrics 2 2)

  oldBests =
    BestRecords
      oldRunPoorCodeSize
      oldRunPoorCodeSize
      oldRunPoorCodeSize
      oldRunPoorCodeSize

  newExpectedBests =
    BestRecords
      oldRunPoorCodeSize
      oldRunPoorCodeSize
      newRunBetterCodeSize
      newRunBetterCodeSize

  newActualBests =
    updateBest
      newRunBetterCodeSize
      oldBests
