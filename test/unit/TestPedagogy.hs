{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm pedagogical tests
module TestPedagogy where

import Control.Lens (view)
import Data.Map qualified as M
import Swarm.Doc.Pedagogy
import Swarm.Game.ScenarioInfo (scenarioPath)
import Swarm.Game.State
import Test.Tasty
import Test.Tasty.HUnit

testPedagogy :: GameState -> TestTree
testPedagogy gs =
  testGroup
    "Pedagogical soundness"
    [ testGroup
        "Introduce new commands in the description"
        testList
    ]
 where
  tutorialInfos = generateIntroductionsSequence $ view scenarios gs

  testFromTut :: Int -> CoverageInfo -> TestTree
  testFromTut idx (CoverageInfo (TutorialInfo (_s, si) _ descCommands) novelCommands) =
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

    scPath = view scenarioPath si
    allCommandsCovered = M.null missingCmds

  testList = zipWith testFromTut [0 ..] tutorialInfos
