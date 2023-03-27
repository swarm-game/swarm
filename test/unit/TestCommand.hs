{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm command implementation unit tests
module TestCommand where

import Swarm.Game.Location
import Swarm.Language.Syntax
import Test.Tasty
import Test.Tasty.HUnit

testCommands :: TestTree
testCommands =
  testGroup
    "Directions"
    [ testGroup
        "Relative direction"
        [ testCase
            "West to East"
            $ assertEqual "Incorrect relative dir" (relativeTo DWest DEast) DBack
        , testCase
            "South to South"
            $ assertEqual "Incorrect relative dir" (relativeTo DSouth DSouth) DForward
        , testCase
            "South to West"
            $ assertEqual "Incorrect relative dir" (DSouth `relativeTo` DWest) DLeft
        , testCase
            "West to South"
            $ assertEqual "Incorrect relative dir" (DWest `relativeTo` DSouth) DRight
        ]
    ]
