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
-- mutable /entity/ layer, where each cell has at most one entity.
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

{-# LANGUAGE FlexibleContexts #-}

module Swarm.Game.World
  ( -- * The @Worldly@ type class

    Worldly(..)

    -- * World implementations

  , SimpleWorld, TileCachingWorld

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

-- | A class to abstract over different world implementations.  We
--   really only need one world implementation at a time, but it's
--   helpful to think about what operations a world needs to support,
--   and to be able to play with swapping in different implementations
--   to see how it affects performance.
class Worldly w where

  -- | Create a new world from a function that defines what is at
  --   every location: a character representing the terrain, and
  --   possibly an entity.
  newWorld   :: ((Int,Int) -> (Char, Maybe e)) -> w e

  -- | Look up the terrain character at given (row, column)
  --   coordinates.  Note that this does /not/ return an updated
  --   world, since it would be difficult to manage all the updates.
  --   Instead, the 'loadRegion' function can be used before calling
  --   'lookupTerrain'.
  lookupTerrain :: (Int,Int) -> w e -> Char

  -- | Look up the entity at given (row, column) coordinates.
  lookupEntity :: (Int,Int) -> w e -> Maybe e

  -- | Update the entity at a given location. into the world.
  update     :: (Int,Int) -> (Maybe e -> Maybe e) -> w e -> w e

  -- | Give a hint to the world that it should preload the region in
  --   between the given coordinates (upper left and bottom right) to
  --   make subsequent lookups faster.
  loadRegion :: ((Int,Int), (Int,Int)) -> w e -> w e

  -- XXX Allow smaller, finite worlds Too?  Maybe add a variant of
  -- newWorld that creates a finite world from an array.  This could
  -- be used e.g. to create puzzle levels, which can be loaded from a
  -- file instead of generated via noise functions.

------------------------------------------------------------
-- SimpleWorld

-- | A 'SimpleWorld' just stores the world function and a map with
--   changed locations.  It does not do any fancy caching; in
--   particular the 'loadRegion' function does nothing.  The 'lookup'
--   function first just looks up the location in the map of changed
--   locations, and if it's not there it simply runs the function to
--   find out what character should be there.
data SimpleWorld e = SimpleWorld
  ((Int,Int) -> (Char, Maybe e))    -- ^ World generation function
  (M.Map (Int,Int) (Maybe e))       -- ^ Map of locations that have a different entity
                                    --   than originally

instance Worldly SimpleWorld where
  newWorld f                   = SimpleWorld f M.empty
  lookupTerrain i (SimpleWorld f _) = fst (f i)
  lookupEntity i (SimpleWorld f m)  = M.lookup i m ? snd (f i)
  update i g (SimpleWorld f m) = SimpleWorld f (M.adjust g i m)
  loadRegion _ w               = w

------------------------------------------------------------
-- TileCachingWorld

tileBits :: Int
tileBits = 6     -- Each tile is 2^tileBits x 2^tileBits

tileMask :: Int
tileMask = (1 `shiftL` tileBits) - 1

tileBounds :: ((Int,Int), (Int,Int))
tileBounds = ((0,0),(tileMask,tileMask))

type TerrainTile  = U.UArray (Int,Int) Char
type EntityTile e = A.Array (Int,Int) (Maybe e)

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

data TileCachingWorld e = TileCachingWorld
  ((Int,Int) -> (Char, Maybe e))                 -- ^ World generation function
  (M.Map (Int,Int) (TerrainTile, EntityTile e))  -- ^ Tile cache
  (M.Map (Int,Int) (Maybe e))                    -- ^ Map of locations that have changed entities

instance Worldly TileCachingWorld where
  newWorld f = TileCachingWorld f M.empty M.empty
  lookupTerrain i (TileCachingWorld f t _)
    = ((U.! over both (.&. tileMask) i) . fst <$> M.lookup (tileIndex i) t)
      ? fst (f i)
  lookupEntity i (TileCachingWorld f t m)
    = M.lookup i m
        ? ((A.! over both (.&. tileMask) i) . snd <$> M.lookup (tileIndex i) t)
        ? snd (f i)
  update i g (TileCachingWorld f t m) = TileCachingWorld f t (M.adjust g i m)
  loadRegion reg (TileCachingWorld f t m) = TileCachingWorld f t' m
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
