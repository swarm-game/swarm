{-# LANGUAGE OverloadedStrings #-}

-- | High score records
module TestScoring where

import Control.Carrier.Error.Either (runError)
import Data.Text.IO qualified as TIO
import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Swarm.Failure (SystemFailure)
import Swarm.Game.Scenario.Scoring.Best
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.Scenario.Scoring.ConcreteMetrics
import Swarm.Game.Scenario.Scoring.GenericMetrics
import Swarm.Game.Tick (TickNumber (..))
import Swarm.Language.Load (SyntaxWithImports (getSyntax))
import Swarm.Language.Pipeline
import Swarm.Language.Syntax
import Swarm.Pretty (prettyString)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

baseTestPath :: FilePath
baseTestPath = "data/test/language-snippets/code-size"

testHighScores :: TestTree
testHighScores =
  testGroup
    "Scoring"
    [ testGroup
        "Code size"
        [ compareAstSize 1 "single-move-bare.sw"
        , compareAstSize 2 "single-move-def.sw"
        , compareAstSize 3 "single-move-let-with-invocation.sw"
        , compareAstSize 5 "double-move-let-with-invocation.sw"
        , compareAstSize 3 "single-move-def-with-invocation.sw"
        , compareAstSize 5 "double-move-def-with-invocation.sw"
        , compareAstSize 25 "single-def-two-args-recursive.sw"
        , compareAstSize 30 "single-def-two-args-recursive-with-invocation.sw"
        ]
    , testGroup
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
    ]

compareAstSize :: Int -> FilePath -> TestTree
compareAstSize expectedSize path = testCase (unwords ["size of", path]) $ do
  contents <- TIO.readFile $ baseTestPath </> path
  src <- runError @SystemFailure $ requireNonEmptyTerm =<< processSource contents Nothing
  t <- either (assertFailure . prettyString) (pure . getSyntax) src
  assertEqual "incorrect size" expectedSize (measureAstSize t)

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
          (DurationMetrics 1 $ TickNumber 1)
          Nothing

  oldCompletedRunWithCodeSize =
    Metric Completed $
      ProgressStats (mkZonedTime 0) $
        AttemptMetrics
          (DurationMetrics 2 $ TickNumber 2)
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
          (DurationMetrics 2 $ TickNumber 2)
          (Just $ ScenarioCodeMetrics 1 1)

  oldRunPoorCodeSize =
    Metric Completed $
      ProgressStats (mkZonedTime 0) $
        AttemptMetrics
          (DurationMetrics 1 $ TickNumber 1)
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
