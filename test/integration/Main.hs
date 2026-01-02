{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm integration tests
module Main where

import Control.Carrier.Error.Either (runError)
import Control.Carrier.Lift (runM)
import Control.Carrier.Throw.Either (runThrow)
import Control.Lens ((&), (.~), (^.))
import Data.Set qualified as S
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Swarm.Failure (SystemFailure)
import Swarm.Game.Scenario (gsiScenarioInputs)
import Swarm.Game.Scenario.Scoring.GenericMetrics (Metric (..), Progress (..))
import Swarm.Game.ScenarioInfo (ScenarioInfo, ScenarioStatus (..), scenarioStatus)
import Swarm.Game.State.Runtime (eventLog, stdGameConfigInputs)
import Swarm.Game.State.Substate (initState)
import Swarm.Language.Pipeline (processSource, requireNonEmptyTerm)
import Swarm.Pretty (prettyString)
import Swarm.TUI.Model (debugOptions, defaultAppOpts)
import Swarm.TUI.Model.DebugOption (DebugOption (LoadTestingScenarios))
import Swarm.TUI.Model.StateUpdate (PersistentState (..), initPersistentState)
import Swarm.Util (Encoding (..), findAllWithExt, readFileMayT)
import Swarm.Util.Effect ((???))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestEditorFiles
import TestFormat
import TestLoadingErrors
import TestRecipeCoverage
import TestScenarioParse
import TestScenarioSolutions

main :: IO ()
main = do
  examplePaths <- findAllWithExt "example" "sw"
  scenarioPaths <- findAllWithExt "data/scenarios" "yaml"
  scenarioPrograms <- findAllWithExt "data/scenarios" "sw"
  PersistentState rs ui key progState <- do
    let testingOptions = defaultAppOpts {debugOptions = S.singleton LoadTestingScenarios}
    out <- runM . runThrow @SystemFailure $ initPersistentState testingOptions
    either (assertFailure . prettyString) return out
  let scenarioInputs = gsiScenarioInputs $ initState $ rs ^. stdGameConfigInputs
      rs' = rs & eventLog .~ mempty
  recipeTests <- testRecipeCoverage
  formatTests <- testFormatting
  defaultMain $
    testGroup
      "Tests"
      [ testNoLoadingErrors rs
      , exampleTests examplePaths
      , exampleTests scenarioPrograms
      , scenarioParseTests scenarioInputs scenarioPaths
      , formatTests
      , noScenarioOverlap
      , testScenarioSolutions scenarioPaths $ PersistentState rs' ui key progState
      , testEditorFiles
      , recipeTests
      , saveFileTests
      ]

------------------------------------------------------------
-- Make sure we can process (parse + typecheck) .sw files

exampleTests :: [FilePath] -> TestTree
exampleTests = testGroup "Process .sw files" . map exampleTest

exampleTest :: FilePath -> TestTree
exampleTest path =
  testCase ("processTerm for contents of " ++ show path) $ do
    content <- readFileMayT UTF8 path ??? assertFailure "Can't read file!"
    res <- runError @SystemFailure (processSource (Just path) Nothing content >>= requireNonEmptyTerm)
    either (assertFailure . prettyString) (const $ pure ()) res

------------------------------------------------------------
-- Make sure we can read save files

saveFileTests :: TestTree
saveFileTests =
  testGroup
    "Save files"
    [ checkLoaded "backstory" "0.6.0.0"
    , checkLoaded "backstory" "latest"
    ]
 where
  checkLoaded scenario version = testCase ("save from version " <> version) $ do
    ef <- decodeFileEither @ScenarioInfo $ "data/test/saves/" <> scenario <> "-" <> version <> ".yaml"
    case ef of
      Left e -> assertFailure $ prettyPrintParseException e
      Right si -> case si ^. scenarioStatus of
        Played _par (Metric Completed _) _best -> pure ()
        other -> assertFailure $ "scenario save file loaded wrong - contains: " <> show other
