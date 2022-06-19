{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Swarm.Game.WorldGen
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Procedural world generation via coherent noise.
module Swarm.Game.WorldGen where

import Data.Bool
import Data.Enumeration
import Data.Hash.Murmur
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Numeric.Noise.Perlin
import Witch

import Data.Array.IArray
import Swarm.Game.Terrain
import Swarm.Game.World

-- | A simple test world I used for a while during early development.
testWorld1 :: WorldFun TerrainType Text
testWorld1 (Coords (-5, 3)) = (StoneT, Just "flerb")
testWorld1 (Coords (2, -1)) = (GrassT, Just "elephant")
testWorld1 (Coords (i, j))
  | noiseValue pn1 (fromIntegral i, fromIntegral j, 0) > 0 = (DirtT, Just "tree")
  | noiseValue pn2 (fromIntegral i, fromIntegral j, 0) > 0 = (StoneT, Just "rock")
  | otherwise = (GrassT, Nothing)
 where
  pn1, pn2 :: Perlin
  pn1 = perlin 0 5 0.05 0.5
  pn2 = perlin 0 5 0.05 0.75

data Size = Small | Big deriving (Eq, Ord, Show, Read)
data Hardness = Soft | Hard deriving (Eq, Ord, Show, Read)
data Origin = Natural | Artificial deriving (Eq, Ord, Show, Read)
type Seed = Int

-- | A more featureful test world.
testWorld2 :: Seed -> WorldFun TerrainType Text
testWorld2 baseSeed (Coords ix@(r, c)) =
  genBiome
    (bool Small Big (sample ix pn0 > 0))
    (bool Soft Hard (sample ix pn1 > 0))
    (bool Natural Artificial (sample ix pn2 > 0))
 where
  h = murmur3 0 (into (show ix))

  genBiome Big Hard Natural
    | sample ix cl0 > 0.5 = (StoneT, Just "mountain")
    | h `mod` 30 == 0 = (StoneT, Just "boulder")
    | sample ix cl0 > 0 =
      case h `mod` 30 of
        1 -> (DirtT, Just "LaTeX")
        _ -> (DirtT, Just "tree")
    | otherwise = (GrassT, Nothing)
  genBiome Small Hard Natural
    | h `mod` 10 == 0 = (StoneT, Just "rock")
    | otherwise = (StoneT, Nothing)
  genBiome Big Soft Natural
    | abs (sample ix pn1) < 0.1 = (DirtT, Just "sand")
    | even (r + c) = (DirtT, Just "wavy water")
    | otherwise = (DirtT, Just "water")
  genBiome Small Soft Natural
    | h `mod` 10 == 0 = (GrassT, Just "flower")
    | otherwise = (GrassT, Nothing)
  genBiome Small Soft Artificial
    | h `mod` 10 == 0 = (GrassT, Just (T.concat ["bit (", from (show ((r + c) `mod` 2)), ")"]))
    | otherwise = (GrassT, Nothing)
  genBiome Big Soft Artificial
    | h `mod` 5000 == 0 = (DirtT, Just "linux")
    | sample ix cl0 > 0.5 = (GrassT, Nothing)
    | otherwise = (DirtT, Nothing)
  genBiome Small Hard Artificial
    | h `mod` 120 == 1 = (StoneT, Just "lambda")
    | h `mod` 50 == 0 = (StoneT, Just (T.concat ["pixel (", from ["RGB" !! fromIntegral ((r + c) `mod` 3)], ")"]))
    | otherwise = (StoneT, Nothing)
  genBiome Big Hard Artificial
    | sample ix cl0 > 0.85 = (StoneT, Just "copper ore")
    | otherwise = (StoneT, Nothing)

  sample (i, j) noise = noiseValue noise (fromIntegral i / 2, fromIntegral j / 2, 0)

  pn :: Int -> Perlin
  pn seed = perlin (seed + baseSeed) 6 0.05 0.6

  pn0 = pn 0
  pn1 = pn 1
  pn2 = pn 2

  -- alternative noise function
  -- rg :: Int -> Ridged
  -- rg seed = ridged seed 6 0.05 1 2

  clumps :: Int -> Perlin
  clumps seed = perlin (seed + baseSeed) 4 0.08 0.5

  cl0 = clumps 0

-- | Create a world function from a finite array of specified cells
--   plus a seed to randomly generate the rest.
testWorld2FromArray :: Array (Int64, Int64) (TerrainType, Maybe Text) -> Seed -> WorldFun TerrainType Text
testWorld2FromArray arr seed co@(Coords (r, c))
  | inRange bnds (r, c) = arr ! (r, c)
  | otherwise = tw2 co
 where
  tw2 = testWorld2 seed
  bnds = bounds arr

-- | Offset a world by a multiple of the @skip@ in such a way that it
--   satisfies the given predicate.
findOffset :: Integer -> (WorldFun t Text -> Bool) -> WorldFun t Text -> WorldFun t Text
findOffset skip isGood f = f'
 where
  offset :: Enumeration Int64
  offset = fromIntegral . (skip *) <$> int

  f' =
    fromMaybe (error "the impossible happened, no offsets were found!")
      . find isGood
      . map shift
      . enumerate
      $ offset >< offset

  shift (dr, dc) (Coords (r, c)) = f (Coords (r - dr, c - dc))

-- | Offset the world so the base starts in a 32x32 patch containing at least one
--   of each of a list of required entities.
findPatchWith :: [Text] -> WorldFun t Text -> WorldFun t Text
findPatchWith reqs = findOffset 32 isGoodPatch
 where
  patchCoords = [(r, c) | r <- [-16 .. 16], c <- [-16 .. 16]]
  isGoodPatch f = all (`S.member` es) reqs
   where
    es = S.fromList . mapMaybe (snd . f . Coords) $ patchCoords

-- | Offset the world so the base starts on empty spot next to tree and grass.
findTreeOffset :: WorldFun t Text -> WorldFun t Text
findTreeOffset = findOffset 1 isGoodPlace
 where
  isGoodPlace f =
    hasEntity Nothing (0, 0)
      && any (hasEntity (Just "tree")) neighbors
      && all (\c -> hasEntity (Just "tree") c || hasEntity Nothing c) neighbors
   where
    hasEntity mayE = (== mayE) . snd . f . Coords

  neighbors = [(r, c) | r <- [-1 .. 1], c <- [-1 .. 1]]

-- | Offset the world so the base starts in a good patch (near
--   necessary items), next to a tree.
findGoodOrigin :: WorldFun t Text -> WorldFun t Text
findGoodOrigin = findTreeOffset . findPatchWith ["tree", "copper ore", "bit (0)", "bit (1)", "rock", "lambda", "water", "sand"]
