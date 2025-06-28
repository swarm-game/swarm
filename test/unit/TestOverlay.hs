{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Unit tests for generic grid overlay logic
module TestOverlay (testOverlay) where

import Control.Monad (when)
import Data.Function (on)
import Data.Text (Text)
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Area (
  AreaDimensions (AreaDimensions),
 )
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Scenario.Topography.Structure.Assembly (
  foldLayer,
 )
import Swarm.Game.Scenario.Topography.Structure.Named
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Test.Tasty
import Test.Tasty.HUnit

debugRenderGrid :: Bool
debugRenderGrid = False

-- * Example grids

-- | Single cell
oneByOneGrid :: [[Int]]
oneByOneGrid = [[0]]

-- | Single row with two columns
oneByTwoGrid :: [[Int]]
oneByTwoGrid = [[5, 6]]

-- | Two rows with two columns
twoByTwoGrid :: [[Int]]
twoByTwoGrid =
  [ [1, 2]
  , [3, 4]
  ]

testOverlay :: TestTree
testOverlay =
  testGroup
    "Overlay"
    [ testGroup
        "Empty grids, base grid at origin"
        [ mkOriginTestCase "Northward" (Location 3 2) (Location 0 2)
        , mkOriginTestCase "Westward" (Location (-7) (-1)) (Location (-7) 0)
        ]
    , testGroup
        "Overlay sequences"
        [ testGroup
            "Horizontal siblings"
            [ mkOverlaySequenceOriginTest
                "negative first west of second"
                [ placeUnshifted "sibling1" (Location (-2) 0) twoByTwoGrid
                , placeUnshifted "sibling2" (Location 0 0) oneByTwoGrid
                ]
                (Location (-2) 0)
            , mkOverlaySequenceOriginTest
                "first east of negative second"
                [ placeUnshifted "sibling1" (Location 0 0) twoByTwoGrid
                , placeUnshifted "sibling2" (Location (-2) 0) oneByTwoGrid
                ]
                (Location (-2) 0)
            ]
        , testGroup
            "Vertical siblings"
            [ mkOverlaySequenceOriginTest
                "positive first south of second"
                [ placeUnshifted "sibling1" (Location 0 2) twoByTwoGrid
                , placeUnshifted "sibling2" (Location 0 0) oneByTwoGrid
                ]
                (Location 0 2)
            , mkOverlaySequenceOriginTest
                "first north of positive second"
                [ placeUnshifted "sibling1" (Location 0 0) twoByTwoGrid
                , placeUnshifted "sibling2" (Location 0 2) oneByTwoGrid
                ]
                (Location 0 2)
            ]
        , testGroup
            "Merge sizes"
            [ testMergedSize
                "merge an offset 1x1 atop a 0x0 base"
                (mkNamedStructure "baseLayer" (Location 0 0) [[]])
                (mkNamedStructure "sibling1" (Location (-1) 1) oneByOneGrid)
                (AreaDimensions 1 1)
            , testMergedSize
                "merge a 2x2 atop a 1x1 with an offset"
                (mkNamedStructure "sibling1" (Location (-1) 1) oneByOneGrid)
                (mkNamedStructure "sibling2" (Location 0 0) twoByTwoGrid)
                (AreaDimensions 3 3)
            ]
        , testGroup
            "Northwesterly offset of first sibling"
            [ mkOverlaySequenceOriginTest
                "positive first south of second"
                [ placeUnshifted "sibling1" (Location (-1) 1) oneByOneGrid
                , placeUnshifted "sibling2" (Location 0 0) twoByTwoGrid
                ]
                (Location (-1) 1)
            ]
        ]
    ]

-- * Test construction
testMergedSize ::
  String ->
  NamedStructure (Maybe Int) ->
  NamedStructure (Maybe Int) ->
  AreaDimensions ->
  TestTree
testMergedSize testLabel ns1 ns2 expectedArea =
  testCase testLabel $ do
    assertEqual "Merged area is wrong" expectedArea mergedSize
 where
  mergedSize =
    computeMergedArea $
      (OverlayPair `on` (area . structure)) ns1 ns2

-- | Base layer is at the origin (0, 0).
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

mkOverlaySequenceOriginTest ::
  String ->
  [Placed (Maybe Int)] ->
  Location ->
  TestTree
mkOverlaySequenceOriginTest = mkOverlaySequenceTest gridPosition

mkOverlaySequenceTest ::
  (Show a, Eq a) =>
  (PositionedGrid (Maybe Int) -> a) ->
  String ->
  [Placed (Maybe Int)] ->
  a ->
  TestTree
mkOverlaySequenceTest f testLabel overlays expectedBaseLoc =
  testCase testLabel $ do
    when debugRenderGrid $
      renderGridResult eitherResultGrid

    assertEqual "Base loc wrong" (Right expectedBaseLoc) $
      f <$> eitherResultGrid
 where
  baseArea = PositionedGrid (Location 0 0) (EmptyGrid :: Grid (Maybe Int))

  eitherResultGrid = getGridFromMergedStructure <$> eitherResult
  eitherResult =
    foldLayer baseArea overlays

getGridFromMergedStructure :: MergedStructure c -> PositionedGrid c
getGridFromMergedStructure (MergedStructure g _ _) = g

-- | Place an structure at an offset.
-- The structure's local origin is (0, 0).
placeUnshifted ::
  Text ->
  Location ->
  [[a]] ->
  Placed (Maybe a)
placeUnshifted = place (Location 0 0)

-- | Place a structure at an offset.
-- That structure's local origin might not be (0, 0).
place ::
  Location ->
  Text ->
  Location ->
  [[a]] ->
  Placed (Maybe a)
place localOrigin theName placementOffset g =
  Placed (Placement sName (Pose placementOffset defaultOrientation)) $
    mkNamedStructure theName localOrigin g
 where
  sName = StructureName theName

mkNamedStructure ::
  Text ->
  Location ->
  [[a]] ->
  NamedArea (PStructure (Maybe a))
mkNamedStructure theName pos g =
  NamedArea sName mempty mempty s
 where
  sName = StructureName theName
  s =
    Structure
      (PositionedGrid pos $ Just <$> mkGrid g)
      mempty
      mempty
      mempty

renderGridResult :: Either a (PositionedGrid (Maybe Int)) -> IO ()
renderGridResult = mapM_ $ \pg -> do
  print pg
  print $ getRows $ gridContent pg
