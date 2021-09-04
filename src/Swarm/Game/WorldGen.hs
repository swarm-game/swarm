-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.WorldGen
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Procedural world generation via coherent noise.
--
-----------------------------------------------------------------------------

module Swarm.Game.WorldGen where

import           Data.Bool
import           Data.Enumeration
import           Data.Hash.Murmur
import           Numeric.Noise.Perlin
import           Witch

import           Data.List            (find)
import qualified Swarm.Game.Entities  as E
import           Swarm.Game.Entity    (Entity)
import           Swarm.Game.Terrain
import           Swarm.Game.World


-- | A simple test world I used for a while during early development.
testWorld1 :: WorldFun TerrainType Entity
testWorld1 (-5, 3) = (StoneT, Just E.flerb)
testWorld1 (2, -1) = (GrassT, Just E.elephant)
testWorld1 (i,j)
  | noiseValue pn1 (fromIntegral i, fromIntegral j, 0) > 0 = (DirtT, Just E.tree)
  | noiseValue pn2 (fromIntegral i, fromIntegral j, 0) > 0 = (StoneT, Just E.rock)
  | otherwise = (GrassT, Nothing)
  where
    pn1, pn2 :: Perlin
    pn1 = perlin 0 5 0.05 0.5
    pn2 = perlin 0 5 0.05 0.75

data Size     = Small   | Big        deriving (Eq, Ord, Show, Read)
data Hardness = Soft    | Hard       deriving (Eq, Ord, Show, Read)
data Origin   = Natural | Artificial deriving (Eq, Ord, Show, Read)

-- | A more featureful test world.
testWorld2 :: WorldFun TerrainType Entity
testWorld2 ix@(r,c)
  = genBiome
    (bool Small Big (sample ix pn0 > 0))
    (bool Soft Hard (sample ix pn1 > 0))
    (bool Natural Artificial (sample ix pn2 > 0))
  where
    h = murmur3 0 (into (show ix))

    genBiome Big Hard Natural
      | sample ix cl0 > 0.7  = (StoneT, Just E.mountain)
      | h `mod` 30 == 0      = (StoneT, Just E.rock)
      | sample ix cl0 > 0    = (DirtT, Just E.tree)
      | otherwise            = (GrassT, Nothing)
    genBiome Small Hard Natural
      | h `mod` 30  == 0  = (StoneT, Just E.pebbles)
      | otherwise         = (StoneT, Nothing)
    genBiome Big Soft Natural
      | even (r+c) = (WaterT, Just E.wave)
      | otherwise  = (WaterT, Nothing)
    genBiome Small Soft Natural
      | h `mod` 10 == 0  = (GrassT, Just E.flower)
      | otherwise        = (GrassT, Nothing)
    genBiome Small Soft Artificial
      | h `mod` 10 == 0  = (GrassT, Just (E.bit (even (r+c))))
      | otherwise        = (GrassT, Nothing)
    genBiome Big Soft Artificial
      | h `mod` 5000 == 0   = (DirtT, Just E.linux)
      | sample ix cl0 > 0.5 = (GrassT, Nothing)
      | otherwise           = (DirtT, Nothing)
    genBiome Small Hard Artificial
      | h `mod` 120 == 1  = (StoneT, Just E.lambda)
      | otherwise = (StoneT, Nothing)
    genBiome Big Hard Artificial
      | sample ix cl0 > 0.8 = (IceT, Nothing)
      | otherwise           = (StoneT, Nothing)

    sample (i,j) noise = noiseValue noise (fromIntegral i / 2, fromIntegral j / 2, 0)

    pn :: Int -> Perlin
    pn seed = perlin seed 6 0.05 0.6

    pn0 = pn 0
    pn1 = pn 1
    pn2 = pn 2

    clumps :: Int -> Perlin
    clumps seed = perlin seed 4 0.08 0.5

    cl0 = clumps 0

findGoodOrigin :: WorldFun t Entity -> WorldFun t Entity
findGoodOrigin f = \(r,c) -> f (r + fromIntegral rOffset, c + fromIntegral cOffset)
  where
    int' :: Enumeration Int
    int' = fromIntegral <$> int
    Just (rOffset, cOffset) = find isTree (enumerate (int' >< int'))
    isTree = (== Just E.tree) . snd . f
