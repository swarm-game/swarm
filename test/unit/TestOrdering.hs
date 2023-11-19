{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm unit tests
module TestOrdering where

import Data.List (sort)
import Swarm.Game.Location
import Test.Tasty
import Test.Tasty.HUnit

testOrdering :: TestTree
testOrdering =
  testGroup
    "Ordering"
    [ testCase "Sorted locations" $ do
        assertEqual "Locations should be ascending" expectedOrder (sort unsortedLocs)
    ]
 where
  unsortedLocs =
    [ Location 4 6
    , Location 3 7
    ]

  expectedOrder =
    [ Location 3 7
    , Location 4 6
    ]
