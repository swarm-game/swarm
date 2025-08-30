-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Game-related state and utilities
--
-- World tiles are subsquares of the world grid.
-- This is an optimization so that we can check
-- neighboring cells faster, generate world cells
-- in parallel batches, etc.
module Swarm.Game.World.Tile (
  -- * Tiles
  TerrainTile,
  EntityTile,

  -- ** Tile coordinates
  TileCoords,
  tileCoords,
  tileOrigin,
  tileBounds,

  -- ** Tile offsets
  TileOffset,
  tileOffset,
  plusOffset,

  -- ** Bitwise operations
  tileBits,
  tileMask,
) where

import Control.Lens hiding (use)
import Data.Array.IArray
import Data.Array.IArray qualified as A
import Data.Array.Unboxed qualified as U
import Data.Bits
import Data.Int (Int32)
import GHC.Generics (Generic)
import Swarm.Game.World.Coords
import Data.Strict qualified as Strict

-- | A terrain tile is an unboxed array of terrain values.
type TerrainTile t = U.UArray TileOffset t

-- | An entity tile is an array of possible entity values.  Note it
--   cannot be an unboxed array since entities are complex records
--   which have to be boxed.
type EntityTile e = A.Array TileOffset (Strict.Maybe e)

-- | The number of bits we need in each coordinate to represent all
--   the locations in a tile.  In other words, each tile has a size of
--   @2^tileBits x 2^tileBits@.
--
--   Currently, 'tileBits' is set to 5, giving us 32x32 tiles, with
--   1024 cells in each tile. This seems to result in more smooth
--   scrolling of the world map.
tileBits :: Int
tileBits = 5

-- | The number consisting of 'tileBits' many 1 bits.  We can use this
--   to mask out the tile offset of a coordinate.
tileMask :: Int32
tileMask = (1 `shiftL` tileBits) - 1

-- | If we think of the world as a grid of /tiles/, we can assign each
--   tile some coordinates in the same way we would if each tile was a
--   single cell.  These are the tile coordinates.
newtype TileCoords = TileCoords {unTileCoords :: Coords}
  deriving (Eq, Ord, Show, Ix, Generic)

instance Rewrapped TileCoords t
instance Wrapped TileCoords

-- | Convert from a cell's coordinates to the coordinates of its tile,
--   simply by shifting out 'tileBits' many bits.
tileCoords :: Coords -> TileCoords
tileCoords = TileCoords . over (_Wrapped . both) (`shiftR` tileBits)

-- | Find the coordinates of the upper-left corner of a tile.
tileOrigin :: TileCoords -> Coords
tileOrigin = over (_Wrapped . both) (`shiftL` tileBits) . unTileCoords

-- | A 'TileOffset' represents an offset from the upper-left corner of
--   some tile to a cell in its interior.
newtype TileOffset = TileOffset Coords
  deriving (Eq, Ord, Show, Ix, Generic)

-- | The offsets of the upper-left and lower-right corners of a tile:
--   (0,0) to ('tileMask', 'tileMask').
tileBounds :: (TileOffset, TileOffset)
tileBounds = (TileOffset (Coords (0, 0)), TileOffset (Coords (tileMask, tileMask)))

-- | Compute the offset of a given coordinate within its tile.
tileOffset :: Coords -> TileOffset
tileOffset = TileOffset . over (_Wrapped . both) (.&. tileMask)

-- | Add a tile offset to the coordinates of the tile's upper left
--   corner.  NOTE that for efficiency, this function only works when
--   the first argument is in fact the coordinates of a tile's
--   upper-left corner (/i.e./ it is an output of 'tileOrigin').  In
--   that case the coordinates will end with all 0 bits, and we can
--   add the tile offset just by doing a coordinatewise 'xor'.
plusOffset :: Coords -> TileOffset -> Coords
plusOffset (Coords (x1, y1)) (TileOffset (Coords (x2, y2))) = Coords (x1 `xor` x2, y1 `xor` y2)

instance Rewrapped TileOffset t
instance Wrapped TileOffset
