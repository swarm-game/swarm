-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.World
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A /world/ refers to the grid of characters, aka resources, on which
-- the game takes place.  A world is technically finite but
-- practically infinite (worlds are indexed by 64-bit signed integers,
-- so they correspond to a \( 2^{64} \times 2^{64} \) torus).
--
-- This module provides several implementations of a world
-- abstraction, which allows creating a new world, querying or setting
-- the world at a given location, and informing the world that it
-- should preload a certain region to make subsequent lookups faster.
--
-----------------------------------------------------------------------------

module Swarm.Game.World
  ( -- * The @Worldly@ type class

    Worldly(..)

    -- * World implementations

  , SimpleWorld, TileCachingWorld

  ) where

import           Control.Arrow      ((&&&))
import           Control.Lens
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

  -- | Create a new world from a function that defines what character
  --   is at every location.
  newWorld   :: ((Int,Int) -> Char) -> w

  -- | Look up the character at given (row, column) coordinates.  Note
  --   that this does /not/ return an updated world, since it would be
  --   difficult to manage all the updates.  Instead, the 'loadRegion'
  --   function can be used before calling 'lookup'.
  lookup     :: (Int,Int) -> w -> Char

  -- | Insert a new character into the world at the given coordinates.
  insert     :: (Int,Int) -> Char -> w -> w

  -- | Give a hint to the world that it should preload the region in
  --   between the given coordinates (upper left and bottom right) to
  --   make subsequent lookups faster.
  loadRegion :: ((Int,Int), (Int,Int)) -> w -> w

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
data SimpleWorld = SimpleWorld
  ((Int,Int) -> Char)     -- ^ World generation function
  (M.Map (Int,Int) Char)  -- ^ Map of locations that have changed from
                          --   the original value

instance Worldly SimpleWorld where
  newWorld f                   = SimpleWorld f M.empty
  lookup i (SimpleWorld f m)   = M.lookup i m ? f i
  insert i c (SimpleWorld f m) = SimpleWorld f (M.insert i c m)
  loadRegion _ w               = w

------------------------------------------------------------
-- TileCachingWorld

tileBits :: Int
tileBits = 6     -- Each tile is 2^tileBits x 2^tileBits

tileMask :: Int
tileMask = (1 `shiftL` tileBits) - 1

tileBounds :: ((Int,Int), (Int,Int))
tileBounds = ((0,0),(tileMask,tileMask))

type Tile = U.UArray (Int,Int) Char

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

data TileCachingWorld = TileCachingWorld
  ((Int,Int) -> Char)     -- ^ World generation function
  (M.Map (Int,Int) Tile)  -- ^ Tile cache
  (M.Map (Int,Int) Char)  -- ^ Map of locations that have changed

instance Worldly TileCachingWorld where
  newWorld f = TileCachingWorld f M.empty M.empty
  lookup i (TileCachingWorld f t m)
    = M.lookup i m
        ? ((U.! over both (.&. tileMask) i) <$> M.lookup (tileIndex i) t)
        ? f i
  insert i c (TileCachingWorld f t m) = TileCachingWorld f t (M.insert i c m)
  loadRegion reg (TileCachingWorld f t m) = TileCachingWorld f t' m
    where
      tiles = range (over both tileIndex reg)
      t' = foldl' (\hm (i,tile) -> maybeInsert i tile hm) t (map (id &&& loadTile) tiles)

      maybeInsert k v hm
        | k `M.member` hm = hm
        | otherwise       = M.insert k v hm

      loadTile :: (Int,Int) -> Tile
      loadTile ti = listArray tileBounds (map (f . plusRng tileCorner) (range tileBounds))
        where
          tileCorner = over both (`shiftL` tileBits) ti

plusRng :: (Int,Int) -> (Int,Int) -> (Int,Int)
plusRng (x1,y1) (x2,y2) = (x1 `xor` x2,y1 `xor` y2)

tileIndex :: (Int,Int) -> (Int,Int)
tileIndex = over both (`shiftR` tileBits)
