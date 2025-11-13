{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Make sure we can parse valid scenarios, and that explicitly invalid
-- scenarios fail to parse
module TestScenarioParse (isUnparseableTest, scenarioParseTests) where

import Control.Monad (forM_)
import Data.List (partition)
import Data.Yaml (ParseException, prettyPrintParseException)
import Swarm.Game.Scenario (Scenario, ScenarioInputs)
import Swarm.Util.Yaml (decodeFileEitherE)
import System.FilePath (splitDirectories)
import Test.Tasty
import Test.Tasty.HUnit (assertFailure, testCase)

isUnparseableTest :: FilePath -> Bool
isUnparseableTest fp = "_Validation" `elem` splitDirectories fp

scenarioParseTests :: ScenarioInputs -> [FilePath] -> TestTree
scenarioParseTests scenarioInputs scenarioPaths =
  testGroup
    "Scenario parsing"
    [ testGroup
        "Test valid scenarios parses"
        (map (scenarioParseTest Parsed scenarioInputs) parseableScenarios)
    , testGroup
        "Test invalid scenarios fail to parse"
        (map (scenarioParseTest Failed scenarioInputs) unparseableScenarios)
    ]
 where
  (unparseableScenarios, parseableScenarios) = partition isUnparseableTest scenarioPaths

data ParseResult = Parsed | Failed

scenarioParseTest :: ParseResult -> ScenarioInputs -> FilePath -> TestTree
scenarioParseTest expRes scenarioInputs path =
  testCase ("parse scenario " ++ show path) (getScenario expRes scenarioInputs path)

getScenario :: ParseResult -> ScenarioInputs -> FilePath -> IO ()
getScenario expRes scenarioInputs p = do
  res <- decodeFileEitherE scenarioInputs p :: IO (Either ParseException Scenario)
  case expRes of
    Parsed -> case res of
      Left err -> assertFailure $ prettyPrintParseException err
      Right _s -> return ()
    Failed -> forM_ res $ const $ assertFailure "Unexpectedly parsed invalid scenario!"
