{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Unit tests for generic grid overlay logic
module TestOverlay where

import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Test.Tasty
import Test.Tasty.HUnit

testOverlay :: TestTree
testOverlay =
  testGroup
    "Overlay"
    [ -- Overlay is to the east and north of the base.
      -- Therefore, the origin of the combined grid must
      -- be adjusted southward to match its original position
      -- in the base layer.
      mkOriginTestCase "Southward" (Location 3 2) (Location 0 (-2))
    , -- Overlay is to the west and south of the base.
      -- Therefore, the origin of the combined grid must
      -- be adjusted eastward to match its original position
      -- in the base layer.
      mkOriginTestCase "Eastward" (Location (-7) (-1)) (Location 7 0)
    ]

mkOriginTestCase ::
  String ->
  Location ->
  Location ->
  TestTree
mkOriginTestCase adjustmentDescription overlayLocation expectedBaseLoc =
  testCase (unwords [adjustmentDescription, "origin adjustment"]) $ do
    assertEqual "Base loc wrong" expectedBaseLoc actualBaseLoc
 where
  baseLayer = PositionedGrid (Location 0 0) (EmptyGrid :: Grid (Maybe ()))
  overlayLayer = PositionedGrid overlayLocation EmptyGrid
  PositionedGrid actualBaseLoc _ = baseLayer <> overlayLayer
