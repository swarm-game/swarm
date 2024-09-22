{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Unit tests for generic grid overlay logic
module TestOverlay where

import Control.Monad (when)
import Data.Text (Text)
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Scenario.Topography.Structure.Assembly
import Swarm.Game.Scenario.Topography.Structure.Named
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Test.Tasty
import Test.Tasty.HUnit

debugRenderGrid :: Bool
debugRenderGrid = True

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
    , testGroup
        "Overlay sequences"
        [ testGroup
            "Horizontal siblings"
            [ mkOverlaySequenceOriginTest
                "negative first west of second"
                [ placeUnshifted "sibling1" (Location (-2) 0) twoByTwoGrid
                , placeUnshifted "sibling2" (Location 0 0) oneByTwoGrid
                ]
                (Location 2 0)
            , mkOverlaySequenceOriginTest
                "first east of negative second"
                [ placeUnshifted "sibling1" (Location 0 0) twoByTwoGrid
                , placeUnshifted "sibling2" (Location (-2) 0) oneByTwoGrid
                ]
                (Location 2 0)
            ]
        , testGroup
            "Vertical siblings"
            [ mkOverlaySequenceOriginTest
                "positive first south of second"
                [ placeUnshifted "sibling1" (Location 0 2) twoByTwoGrid
                , placeUnshifted "sibling2" (Location 0 0) oneByTwoGrid
                ]
                (Location 0 (-2))
            , mkOverlaySequenceOriginTest
                "first north of positive second"
                [ placeUnshifted "sibling1" (Location 0 0) twoByTwoGrid
                , placeUnshifted "sibling2" (Location 0 2) oneByTwoGrid
                ]
                (Location 0 (-2))
            ]
        , testGroup
            "Nonzero local origin"
            [ mkOverlaySequenceOriginTest
                "positive first south of second"
                [ place (Location 2 0) "sibling1" (Location 0 0) twoByTwoGrid
                ]
                (Location 2 0)
            ]
        , testGroup
            "Northwesterly offset of first sibling"
            [ --  testMergedSize
              --   "test merged size"
              --   (placeUnshifted "baseLayer" (Location 0 0) [[]])
              --   (placeUnshifted "sibling1" (Location (-1) 1) oneByOneGrid)
              --   (AreaDimensions 1 1)

              -- , testMergedSize
              --   "test merged size"
              --   (place (Location 1 (-1)) "sibling1" (Location (-1) 1) oneByOneGrid)
              --   (placeUnshifted "sibling2" (Location 0 0) twoByTwoGrid)
              --   (AreaDimensions 3 3)
              -- ,

              mkOverlaySequenceOriginTest
                "positive first south of second"
                [ placeUnshifted "sibling1" (Location (-1) 1) oneByOneGrid
                , placeUnshifted "sibling2" (Location 0 0) twoByTwoGrid
                -- [ placeUnshifted "sibling2" (Location 0 0) twoByTwoGrid
                -- , placeUnshifted "sibling1" (Location (-1) 1) oneByOneGrid
                ]
                (Location 1 (-1))
            ]
        ]
    ]

-- * Test construction
testMergedSize ::
  String ->
  Placed (Maybe Int) ->
  Placed (Maybe Int) ->
  AreaDimensions ->
  TestTree
testMergedSize testLabel (Placed _ ns1) (Placed _ ns2) expectedArea =
  testCase testLabel $ do
    assertEqual "Merged area is wrong" expectedArea mergedSize
 where
  a1 = area $ structure ns1
  a2 = area $ structure ns2
  mergedSize = computeMergedArea $ OverlayPair a1 a2

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
    foldLayer
      mempty
      baseArea
      overlays
      []

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
    NamedArea sName mempty mempty s
 where
  sName = StructureName theName
  s =
    Structure
      (PositionedGrid localOrigin $ Just <$> mkGrid g)
      mempty
      mempty
      mempty

renderGridResult :: Either a (PositionedGrid (Maybe Int)) -> IO ()
renderGridResult eitherResult = case eitherResult of
  Right pg -> do
    print pg
    print $ getRows $ gridContent pg
  Left _ -> return ()
