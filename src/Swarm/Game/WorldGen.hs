{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Procedural world generation via coherent noise.
module Swarm.Game.WorldGen where

import Control.Lens (view)
import Data.Enumeration
import Data.Int (Int32)
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Swarm.Game.Entity
import Swarm.Game.World

data Size = Small | Big deriving (Eq, Ord, Show, Read)
data Hardness = Soft | Hard deriving (Eq, Ord, Show, Read)
data Origin = Natural | Artificial deriving (Eq, Ord, Show, Read)
type Seed = Int

-- | A list of entities available in the initial world.
testWorld2Entites :: S.Set Text
testWorld2Entites =
  S.fromList
    [ "mountain"
    , "boulder"
    , "LaTeX"
    , "tree"
    , "rock"
    , "lodestone"
    , "sand"
    , "wavy water"
    , "water"
    , "flower"
    , "bit (0)"
    , "bit (1)"
    , "Linux"
    , "lambda"
    , "pixel (R)"
    , "pixel (G)"
    , "pixel (B)"
    , "copper ore"
    ]

-- | Offset a world by a multiple of the @skip@ in such a way that it
--   satisfies the given predicate.
findOffset :: Integer -> ((Coords -> (t, Maybe e)) -> Bool) -> WorldFun t e -> WorldFun t e
findOffset skip isGood (WF f) = WF f'
 where
  offset :: Enumeration Int32
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
findPatchWith :: [Text] -> WorldFun t Entity -> WorldFun t Entity
findPatchWith reqs = findOffset 32 isGoodPatch
 where
  patchCoords = [(r, c) | r <- [-16 .. 16], c <- [-16 .. 16]]
  isGoodPatch f = all (`S.member` es) reqs
   where
    es = S.fromList . map (view entityName) . mapMaybe (snd . f . Coords) $ patchCoords

-- | Offset the world so the base starts on empty spot next to tree and grass.
findTreeOffset :: WorldFun t Entity -> WorldFun t Entity
findTreeOffset = findOffset 1 isGoodPlace
 where
  isGoodPlace f =
    hasEntity Nothing (0, 0)
      && any (hasEntity (Just "tree")) neighbors
      && all (\c -> hasEntity (Just "tree") c || hasEntity Nothing c) neighbors
   where
    hasEntity mayE = (== mayE) . fmap (view entityName) . snd . f . Coords

  neighbors = [(r, c) | r <- [-1 .. 1], c <- [-1 .. 1]]

-- | Offset the world so the base starts in a good patch (near
--   necessary items), next to a tree.
findGoodOrigin :: WorldFun t Entity -> WorldFun t Entity
findGoodOrigin = findTreeOffset . findPatchWith ["tree", "copper ore", "bit (0)", "bit (1)", "rock", "lambda", "water", "sand"]
