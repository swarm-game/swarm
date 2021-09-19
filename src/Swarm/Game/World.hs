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

{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Swarm.Game.World
  ( -- * World coordinates
    Coords(..), locToCoords, coordsToLoc

    -- * Worlds
  , WorldFun, World

    -- ** Tile management
  , loadCell, loadRegion

    -- ** World functions
  , newWorld, lookupTerrain, lookupEntity, update

    -- ** Monadic variants
  , lookupTerrainM, lookupEntityM, updateM
  ) where

import           Debug.Trace

import           Control.Arrow             ((&&&))
import           Control.Lens
import           Control.Monad.State.Class
import qualified Data.Array                as A
import           Data.Array.IArray
import qualified Data.Array.Unboxed        as U
import           Data.Bits
import           Data.Foldable             (foldl')
import qualified Data.Map.Strict           as M
import           GHC.Generics              (Generic)
import           Linear
import           Prelude                   hiding (lookup)

import           Swarm.Util

------------------------------------------------------------
-- World coordinates
------------------------------------------------------------

-- | World coordinates use (row,column) format, with the row
--   increasing as we move down the screen.  This format plays nicely
--   with drawing the screen.
newtype Coords = Coords { unCoords :: (Int,Int)}
  deriving (Eq, Ord, Show, Ix, Generic)

instance Rewrapped Coords t
instance Wrapped Coords

-- | Convert an (x,y) location to a 'Coords' value.
locToCoords :: V2 Int -> Coords
locToCoords (V2 x y) = Coords (-y,x)

-- | Convert 'Coords' to an (x,y) location.
coordsToLoc :: Coords -> V2 Int
coordsToLoc (Coords (r,c)) = V2 c (-r)

-- | A @WorldFun t e@ represents a 2D world with terrain of type @t@
-- (exactly one per cell) and entities of type @e@ (at most one per
-- cell).
type WorldFun t e = Coords -> (t, Maybe e)

  -- XXX Allow smaller, finite worlds Too?  Maybe add a variant of
  -- newWorld that creates a finite world from an array.  This could
  -- be used e.g. to create puzzle levels, which can be loaded from a
  -- file instead of generated via noise functions.

tileBits :: Int
tileBits = 6     -- Each tile is 2^tileBits x 2^tileBits

tileMask :: Int
tileMask = (1 `shiftL` tileBits) - 1

tileBounds :: (TileOffset, TileOffset)
tileBounds = (TileOffset (Coords (0,0)), TileOffset (Coords (tileMask,tileMask)))

newtype TileCoords = TileCoords { unTileCoords :: Coords}
  deriving (Eq, Ord, Show, Ix, Generic)

instance Rewrapped TileCoords t
instance Wrapped TileCoords

tileCoords :: Coords -> TileCoords
tileCoords = TileCoords . over (_Wrapped . both) (`shiftR` tileBits)

tileOrigin :: TileCoords -> Coords
tileOrigin = over (_Wrapped . both) (`shiftL` tileBits) . unTileCoords

newtype TileOffset = TileOffset Coords
  deriving (Eq, Ord, Show, Ix, Generic)

tileOffset :: Coords -> TileOffset
tileOffset = TileOffset . over (_Wrapped . both) (.&. tileMask)

plusOffset :: Coords -> TileOffset -> Coords
plusOffset (Coords (x1,y1)) (TileOffset (Coords (x2,y2))) = Coords (x1 `xor` x2, y1 `xor` y2)

instance Rewrapped TileOffset t
instance Wrapped TileOffset

type TerrainTile t = U.UArray TileOffset t
type EntityTile e  = A.Array TileOffset (Maybe e)

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
  , _tileCache :: M.Map TileCoords (TerrainTile t, EntityTile e)
  , _changed   :: M.Map Coords (Maybe e)
  }

-- makeLenses ''World

newWorld :: WorldFun t e -> World t e
newWorld f = World f M.empty M.empty

lookupTerrain :: IArray U.UArray t => Coords -> World t e -> t
lookupTerrain i (World f t _)
  = ((U.! tileOffset i) . fst <$> M.lookup (tileCoords i) t)
    ? fst (f i)

lookupTerrainM :: (MonadState (World t e) m, IArray U.UArray t) => Coords -> m t
lookupTerrainM c = do
  modify $ loadCell c
  lookupTerrain c <$> get

lookupEntity :: Coords -> World t e -> Maybe e
lookupEntity i (World f t m)
  = M.lookup i m
      ? ((A.! tileOffset i) . snd <$> M.lookup (tileCoords i) t)
      ? snd (f i)

lookupEntityM :: (MonadState (World t e) m, IArray U.UArray t) => Coords -> m (Maybe e)
lookupEntityM c = do
  modify $ loadCell c
  lookupEntity c <$> get

update :: Coords -> (Maybe e -> Maybe e) -> World t e -> World t e
update i g w@(World f t m)
  = World f t (M.insert i (g (lookupEntity i w)) m)

updateM :: (MonadState (World t e) m, IArray U.UArray t) => Coords -> (Maybe e -> Maybe e) -> m ()
updateM c g = modify $ update c g . loadCell c

loadCell :: IArray U.UArray t => Coords -> World t e -> World t e
loadCell c = loadRegion (c,c)

loadRegion :: forall t e. IArray U.UArray t => (Coords, Coords) -> World t e -> World t e
loadRegion reg (World f t m) = World f t' m
  where
    tiles = range (over both tileCoords reg)
    t' = foldl' (\hm (i,tile) -> maybeInsert i tile hm) t (map (id &&& loadTile) tiles)

    maybeInsert k v hm
      | k `M.member` hm = hm
      | otherwise       = M.insert k v hm

    loadTile :: TileCoords -> (TerrainTile t, EntityTile e)
    loadTile tc = (listArray tileBounds terrain, listArray tileBounds entities)
      where
        tileCorner = tileOrigin tc
        (terrain, entities) = unzip $ map (f . plusOffset tileCorner) (range tileBounds)
