{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm pedagogical tests
module TestPedagogy where

import Control.Lens (view)
import Data.Set qualified as S
import Swarm.Docs.Pedagogy
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
    missingCmds = novelCommands `S.difference` descCommands
    errMsg =
      unwords
        [ "command(s) missing from description:"
        , show missingCmds
        ]

    scPath = view scenarioPath si
    allCommandsCovered = S.null missingCmds

  testList = zipWith testFromTut [0 ..] tutorialInfos
