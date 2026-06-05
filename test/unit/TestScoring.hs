{-# LANGUAGE OverloadedStrings #-}

-- | High score records
module TestScoring where

import Control.Carrier.Error.Either (runError)
import Data.Semigroup (Sum (..))
import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Swarm.Failure (SystemFailure)
import Swarm.Game.Scenario.Scoring.Best
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.Scenario.Scoring.ConcreteMetrics
import Swarm.Game.Scenario.Scoring.GenericMetrics
import Swarm.Game.Tick (TickNumber (..))
import Swarm.Language.Pipeline
import Swarm.Language.Syntax
import Swarm.Pretty (prettyString)
import Swarm.Util (Encoding (..), readFileMayT)
import Swarm.Util.Effect ((???))
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
        [ compareASTSize 1 "single-move-bare.sw"
        , compareASTSize 2 "single-move-def.sw"
        , compareASTSize 3 "single-move-let-with-invocation.sw"
        , compareASTSize 5 "double-move-let-with-invocation.sw"
        , compareASTSize 3 "single-move-def-with-invocation.sw"
        , compareASTSize 5 "double-move-def-with-invocation.sw"
        , compareASTSize 25 "single-def-two-args-recursive.sw"
        , compareASTSize 30 "single-def-two-args-recursive-with-invocation.sw"
        , compareASTSize 6 "c.sw"
        , compareASTSize 8 "def-with-import.sw"
        , compareASTSize 28 "diamond-import.sw"
        ]
    , testGroup
        "Code character count"
        [ compareASTChars 4 "single-move-bare.sw"
        , compareASTChars 22 "single-move-def.sw"
        , compareASTChars 28 "single-move-let-with-invocation.sw"
        , compareASTChars 34 "double-move-let-with-invocation.sw"
        , compareASTChars 32 "single-move-def-with-invocation.sw"
        , compareASTChars 38 "double-move-def-with-invocation.sw"
        , compareASTChars 56 "single-def-two-args-recursive.sw"
        , compareASTChars 70 "single-def-two-args-recursive-with-invocation.sw"
        , compareASTChars (47 + 16) "def-with-import.sw"
        , compareASTChars (40 + 29 + 29 + 17) "diamond-import.sw"
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

compareASTMetric :: (Eq m, Monoid m, Show m) => String -> (Syntax Elaborated -> m) -> m -> FilePath -> TestTree
compareASTMetric desc measure expected path = testCase (unwords [desc, path]) $ do
  let filePath = baseTestPath </> path
  contents <- readFileMayT UTF8 filePath ??? assertFailure "Can't read file!"
  src <- runError @SystemFailure $ processSource (Just filePath) Nothing contents
  t <- either (assertFailure . prettyString) pure src
  size <- transitiveMetric measure t
  assertEqual "incorrect metric" expected size

compareASTSize :: Int -> FilePath -> TestTree
compareASTSize = compareASTMetric "size of" measureASTSize . Sum

compareASTChars :: Int -> FilePath -> TestTree
compareASTChars = compareASTMetric "characters in" measureASTChars . Sum

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
