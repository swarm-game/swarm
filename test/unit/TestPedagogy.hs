{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm pedagogical tests
module TestPedagogy where

import Control.Lens (view)
import Data.Map qualified as M
import Swarm.Doc.Pedagogy
import Swarm.Game.Scenario.Status (ScenarioPath (..), ScenarioWith (..))
import Swarm.Game.State.Runtime (RuntimeState, scenarios)
import Test.Tasty
import Test.Tasty.HUnit

testPedagogy :: RuntimeState -> TestTree
testPedagogy rs =
  testGroup
    "Pedagogical soundness"
    [ testGroup
        "Introduce new commands in the description"
        testList
    ]
 where
  tutorialInfos = generateIntroductionsSequence $ view scenarios rs

  testFromTut :: Int -> CoverageInfo -> TestTree
  testFromTut idx (CoverageInfo (TutorialInfo (ScenarioWith _s (ScenarioPath scPath)) _ _ descCommands) novelCommands) =
    testCase
      (unwords [show idx, scPath])
      $ assertBool errMsg allCommandsCovered
   where
    missingCmds = M.withoutKeys novelCommands descCommands
    errMsg =
      unwords
        [ "command(s) missing from description:"
        , show $ M.keysSet missingCmds
        ]

    allCommandsCovered = M.null missingCmds

  testList = zipWith testFromTut [0 ..] tutorialInfos
