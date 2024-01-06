{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for working with procedurally generated worlds.
module Swarm.Game.World.Gen where

import Control.Lens (view)
import Data.Enumeration
import Data.Int (Int32)
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Semigroup (Last (..), getLast)
import Data.Set qualified as S
import Data.Text (Text)
import Swarm.Game.Entity
import Swarm.Game.World
import Swarm.Game.World.Syntax (CellVal (..))
import Swarm.Game.World.Typecheck (Const (CCell), TTerm (..))
import Swarm.Util.Erasable

type Seed = Int

-- | Extract a list of all entities mentioned in a given world DSL term.
extractEntities :: TTerm g a -> S.Set Entity
extractEntities (TLam t) = extractEntities t
extractEntities (TApp t1 t2) = extractEntities t1 <> extractEntities t2
extractEntities (TConst (CCell (CellVal _ ee _))) = getEntity ee
 where
  getEntity (EJust (Last e)) = S.singleton e
  getEntity _ = S.empty
extractEntities _ = S.empty

-- | Offset a world by a multiple of the @skip@ in such a way that it
--   satisfies the given predicate.
findOffset :: Integer -> ((Coords -> (t, Erasable (Last e))) -> Bool) -> WorldFun t e -> WorldFun t e
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
    es = S.fromList . map (view entityName) . mapMaybe (erasableToMaybe . fmap getLast . snd . f . Coords) $ patchCoords

-- | Offset the world so the base starts on empty spot next to tree and grass.
findTreeOffset :: WorldFun t Entity -> WorldFun t Entity
findTreeOffset = findOffset 1 isGoodPlace
 where
  isGoodPlace f =
    hasEntity Nothing (0, 0)
      && any (hasEntity (Just "tree")) neighbors
      && all (\c -> hasEntity (Just "tree") c || hasEntity Nothing c) neighbors
   where
    hasEntity mayE = (== mayE) . erasableToMaybe . fmap (view entityName . getLast) . snd . f . Coords

  neighbors = [(r, c) | r <- [-1 .. 1], c <- [-1 .. 1]]

-- | Offset the world so the base starts in a good patch (near
--   necessary items), next to a tree.
findGoodOrigin :: WorldFun t Entity -> WorldFun t Entity
findGoodOrigin = findTreeOffset . findPatchWith ["tree", "copper ore", "bit (0)", "bit (1)", "rock", "lambda", "water", "sand"]
