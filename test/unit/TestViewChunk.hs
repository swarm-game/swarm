{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- View chunk unit tests
module TestViewChunk where

import Control.Lens ((^.), allOf, both, to)
import Data.Int (Int32)
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.TUI.Model.ViewChunk
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary Coords where
  arbitrary = curry Coords <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Cosmic a) where
  arbitrary = Cosmic
     <$> oneof [pure DefaultRootSubworld, pure $ SubworldName "a", pure $ SubworldName "b"]
     <*> arbitrary

instance Arbitrary ViewChunk where
  arbitrary = ViewChunk <$> arbitrary

testViewChunk :: TestTree
testViewChunk =
  testGroup
    "View chunks"
    [ testProperty "view chunk size" $ \vc ->
        sizeOf (viewChunkBounds vc ^. planar) == (viewChunkSize, viewChunkSize)
    , testProperty "view chunk contains loc" $ \c ->
        c `isWithin` viewChunkBounds (viewChunk c)
    , testProperty "view chunk is aligned" $ \vc ->
        allOf (planar . to fst . to unCoords . both) ((==0) . (`mod` viewChunkSize)) (viewChunkBounds vc)
    ]

sizeOf :: BoundsRectangle -> (Int32, Int32)
sizeOf (Coords (r1,c1), Coords (r2, c2)) = (r2 - r1 + 1, c2 - c1 + 1)

isWithin :: Cosmic Coords -> Cosmic BoundsRectangle -> Bool
isWithin (Cosmic s (Coords (r,c))) (Cosmic s' (Coords (tlr, tlc), Coords (brr, brc)))
  = s == s' && tlr <= r && r <= brr && tlc <= c && c <= brc
