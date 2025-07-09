{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- View chunk unit tests
module TestViewChunk where

import Control.Lens (allOf, both, to, (^.))
import Data.Int (Int32)
import Data.Ix (range)
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as S
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.TUI.Model.ViewChunk
import Swarm.Util (rangeNE, chunksOfNE)
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary Coords where
  arbitrary = curry Coords <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Cosmic a) where
  arbitrary =
    Cosmic
      <$> oneof [pure DefaultRootSubworld, pure $ SubworldName "a", pure $ SubworldName "b"]
      <*> arbitrary

instance Arbitrary ViewChunk where
  arbitrary = ViewChunk <$> arbitrary

newtype BR = BR { getBR :: BoundsRectangle }
  deriving Show

instance Arbitrary BR where
  arbitrary = do
    ul <- arbitrary
    dr <- choose (-5,50)
    dc <- choose (-5,50)
    pure $ BR (ul, ul `addTuple` (dr, dc))

testViewChunk :: TestTree
testViewChunk =
  testGroup
    "View chunks"
    [ testProperty "rangeNE is nonempty" $ \rng -> NE.length (rangeNE @(Int,Int) rng) > 0
    , testProperty "chunksOfNE is nonempty" $ \i (NonEmpty ne) -> NE.length (chunksOfNE @() i (NE.fromList ne)) > 0
    , testProperty "view chunk size" $ \vc ->
        sizeOf (viewChunkBounds vc ^. planar) == (viewChunkSize, viewChunkSize)
    , testProperty "view chunk contains loc" $ \c ->
        c `isWithin` viewChunkBounds (viewChunk c)
    , testProperty "view chunk is aligned" $ \vc ->
        allOf (planar . to fst . to unCoords . both) ((== 0) . (`mod` viewChunkSize)) (viewChunkBounds vc)
    , testProperty "viewChunkCover in same subworld" $ \(fmap getBR -> cbr) ->
        (all . all) ((== cbr ^. subworld) . (^. subworld) . unViewChunk) (viewChunkCover cbr)
    , testProperty "viewChunkCover is cover" $ \(fmap getBR -> cbr) ->
        viewChunkCover cbr `covers` (cbr ^. planar)
    , testProperty "viewChunkCover is smallest cover" $ \(fmap getBR -> cbr) ->
        viewChunkCover cbr `isMinimalCover` (cbr ^. planar)
    ]

sizeOf :: BoundsRectangle -> (Int32, Int32)
sizeOf (Coords (r1, c1), Coords (r2, c2)) = (r2 - r1 + 1, c2 - c1 + 1)

isWithin :: Cosmic Coords -> Cosmic BoundsRectangle -> Bool
isWithin (Cosmic s (Coords (r, c))) (Cosmic s' (Coords (tlr, tlc), Coords (brr, brc))) =
  s == s' && tlr <= r && r <= brr && tlc <= c && c <= brc

unnest :: NE.NonEmpty (NE.NonEmpty a) -> [a]
unnest = concatMap NE.toList . NE.toList

vcCoords :: ViewChunk -> Set Coords
vcCoords = S.fromList . range . (^. planar) . viewChunkBounds

covers :: NE.NonEmpty (NE.NonEmpty ViewChunk) -> BoundsRectangle -> Bool
covers vcs br = S.fromList (NE.toList $ rangeNE br) `S.isSubsetOf` (foldMap vcCoords . unnest $ vcs)

isMinimalCover :: NE.NonEmpty (NE.NonEmpty ViewChunk) -> BoundsRectangle -> Bool
isMinimalCover vcg br = flip all vcs $ \vc ->
  not $ S.fromList (NE.toList $ rangeNE br) `S.isSubsetOf` foldMap vcCoords (S.delete vc vcs)
 where
  vcs = S.fromList $ unnest vcg
