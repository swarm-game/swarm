-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.World
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A /world/ refers to the grid on which the game takes place, and the
-- things in it (besides robots). A world has a base, immutable
-- /terrain/ layer, where each cell contains a terrain type, and a
-- mutable /entity/ layer, with at most one entity per cell.
--
-- A world is technically finite but practically infinite (worlds are
-- indexed by 64-bit signed integers, so they correspond to a \(
-- 2^{64} \times 2^{64} \) torus).
--
-- This module provides several implementations of a world
-- abstraction, which allows creating a new world, querying or setting
-- the world at a given location, and informing the world that it
-- should preload a certain region to make subsequent lookups faster.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Swarm.Game.World
  ( WorldFun, World
  , newWorld, lookupTerrain, lookupEntity
  , update, loadRegion
  ) where

import           Control.Arrow      ((&&&))
import           Control.Lens
import qualified Data.Array         as A
import           Data.Array.IArray
import qualified Data.Array.Unboxed as U
import           Data.Bits
import           Data.Foldable      (foldl')
import qualified Data.Map.Strict    as M
import           Prelude            hiding (lookup)

import           Swarm.Util

------------------------------------------------------------
-- Worldly type class

-- | A @WorldFun t e@ represents a 2D world with terrain of type @t@
-- (exactly one per cell) and entities of type @e@ (at most one per
-- cell).
type WorldFun t e = (Int,Int) -> (t, Maybe e)

  -- XXX Allow smaller, finite worlds Too?  Maybe add a variant of
  -- newWorld that creates a finite world from an array.  This could
  -- be used e.g. to create puzzle levels, which can be loaded from a
  -- file instead of generated via noise functions.

------------------------------------------------------------
-- TileCachingWorld

tileBits :: Int
tileBits = 6     -- Each tile is 2^tileBits x 2^tileBits

tileMask :: Int
tileMask = (1 `shiftL` tileBits) - 1

tileBounds :: ((Int,Int), (Int,Int))
tileBounds = ((0,0),(tileMask,tileMask))

type TerrainTile t = U.UArray (Int,Int) t
type EntityTile e  = A.Array (Int,Int) (Maybe e)

-- | A 'TileCachingWorld' keeps a cache of recently accessed square
--   tiles to make lookups faster.  Currently, tiles are \(64 \times
--   64\), but this is adjustible.  Honestly, it does not seem to make
--   much difference as compared to 'SimpleWorld'.
--
--   Right now the 'TileCachingWorld' simply holds on to all the tiles
--   it has ever loaded.  Ideally it would use some kind of LRU
--   caching scheme to keep memory usage bounded, but it would be a
--   bit tricky, and in any case it's probably not going to matter
--   much for a while.

data World t e = World
  { _worldFun  :: WorldFun t e
  , _tileCache :: M.Map (Int,Int) (TerrainTile t, EntityTile e)
  , _changed   :: M.Map (Int,Int) (Maybe e)
  }

-- makeLenses ''World

newWorld :: WorldFun t e -> World t e
newWorld f = World f M.empty M.empty

lookupTerrain :: IArray U.UArray t => (Int, Int) -> World t e -> t
lookupTerrain i (World f t _)
  = ((U.! over both (.&. tileMask) i) . fst <$> M.lookup (tileIndex i) t)
    ? fst (f i)

lookupEntity :: (Int, Int) -> World t e -> Maybe e
lookupEntity i (World f t m)
  = M.lookup i m
      ? ((A.! over both (.&. tileMask) i) . snd <$> M.lookup (tileIndex i) t)
      ? snd (f i)

update :: (Int, Int) -> (Maybe e -> Maybe e) -> World t e -> World t e
update i g w@(World f t m)
  = World f t (M.insert i (g (lookupEntity i w)) m)

loadRegion :: IArray U.UArray t => ((Int,Int), (Int,Int)) -> World t e -> World t e
loadRegion reg (World f t m) = World f t' m
  where
    tiles = range (over both tileIndex reg)
    t' = foldl' (\hm (i,tile) -> maybeInsert i tile hm) t (map (id &&& loadTiles) tiles)

    maybeInsert k v hm
      | k `M.member` hm = hm
      | otherwise       = M.insert k v hm

    -- loadTiles :: (Int,Int) -> (TerrainTile, EntityTile e)
    loadTiles ti = (listArray tileBounds terrain, listArray tileBounds entities)
      where
        tileCorner = over both (`shiftL` tileBits) ti
        (terrain, entities) = unzip $ map (f . plusRng tileCorner) (range tileBounds)

plusRng :: (Int,Int) -> (Int,Int) -> (Int,Int)
plusRng (x1,y1) (x2,y2) = (x1 `xor` x2,y1 `xor` y2)

tileIndex :: (Int,Int) -> (Int,Int)
tileIndex = over both (`shiftR` tileBits)
