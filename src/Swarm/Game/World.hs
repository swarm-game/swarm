module Swarm.Game.World where

import           Control.Lens
import           Data.Array.IArray
import qualified Data.Array.Unboxed as U
import           Data.Bits
-- import           Data.Cache.LRU
import           Control.Arrow      ((&&&))
import           Data.Foldable      (foldl')
import qualified Data.Map.Strict    as M
import           Data.Maybe
import           Prelude            hiding (lookup)

infixr 1 ?
(?) :: Maybe a -> a -> a
(?) = flip fromMaybe

------------------------------------------------------------
-- Worldly type class

class Worldly w where
  newWorld   :: ((Int,Int) -> Char) -> w
  lookup     :: (Int,Int) -> w -> Char
  insert     :: (Int,Int) -> Char -> w -> w
  loadRegion :: ((Int,Int), (Int,Int)) -> w -> w

------------------------------------------------------------
-- SimpleWorld

-- A SimpleWorld just stores the world function and a map with changed
-- locations.  It does not do any fancy caching.

data SimpleWorld = SimpleWorld
  { sworldFun :: (Int,Int) -> Char
  , sworldMap :: M.Map (Int,Int) Char
  }

instance Worldly SimpleWorld where
  newWorld f                    = SimpleWorld f M.empty
  lookup ix (SimpleWorld f m)   = M.lookup ix m ? f ix
  insert ix c (SimpleWorld f m) = SimpleWorld f (M.insert ix c m)
  loadRegion _ w                = w

------------------------------------------------------------
-- TileCachingWorld

-- A TileCachingWorld keeps a cache of recently accessed square tiles
-- to make lookups faster.

tileBits :: Int
tileBits = 6     -- Each tile is 2^tileBits x 2^tileBits

tileMask :: Int
tileMask = (1 `shiftL` tileBits) - 1

tileBounds :: ((Int,Int), (Int,Int))
tileBounds = ((0,0),(tileMask,tileMask))

type Tile = U.UArray (Int,Int) Char

data TileCachingWorld = TileCachingWorld
  { tcWorldFun :: (Int,Int) -> Char
  , tcTileMap  :: M.Map (Int,Int) Tile
  , tcWorldMap :: M.Map (Int,Int) Char
  }

instance Worldly TileCachingWorld where
  newWorld f = TileCachingWorld f M.empty M.empty
  lookup ix (TileCachingWorld f t m)
    = M.lookup ix m
        ? ((U.! over both (.&. tileMask) ix) <$> M.lookup (tileIndex ix) t)
        ? f ix
  insert ix c (TileCachingWorld f t m) = TileCachingWorld f t (M.insert ix c m)
  loadRegion reg (TileCachingWorld f t m) = TileCachingWorld f t' m
    where
      tiles = range (over both tileIndex reg)
      t' = foldl' (\hm (ix,tile) -> maybeInsert ix tile hm) t (map (id &&& loadTile) tiles)

      maybeInsert k v m
        | k `M.member` m = m
        | otherwise       = M.insert k v m

      loadTile :: (Int,Int) -> Tile
      loadTile tix = listArray tileBounds (map (f . plusRng tileCorner) (range tileBounds))
        where
          tileCorner = over both (`shiftL` tileBits) tix

plusRng :: (Int,Int) -> (Int,Int) -> (Int,Int)
plusRng (x1,y1) (x2,y2) = (x1 `xor` x2,y1 `xor` y2)

tileIndex :: (Int,Int) -> (Int,Int)
tileIndex = over both (`shiftR` tileBits)
