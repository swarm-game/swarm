module Swarm.Game.World where

import           Data.Array.IArray
import qualified Data.Array.Unboxed  as U
import           Data.Bits
-- import           Data.Cache.LRU
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Prelude             hiding (lookup)

data World = World
  { worldFun :: (Int,Int) -> Char
  , worldMap :: HM.HashMap (Int,Int) Char
  }

newWorld :: ((Int,Int) -> Char) -> World
newWorld f = World f HM.empty

lookup :: (Int, Int) -> World -> Char
lookup ix w@(World f m) = fromMaybe (f ix) (HM.lookup ix m)

insert :: (Int, Int) -> Char -> World -> World
insert ix c (World f m) = World f (HM.insert ix c m)

-- Make sure a region is loaded into the world cache.
loadRegion :: (Int, Int) -> (Int, Int) -> World -> World
loadRegion topLeft bottomRight w = w

-- tileBits :: Int
-- tileBits = 6     -- Each tile is 2^tileBits x 2^tileBits

-- tileMask = (1 `shiftL` tileBits) - 1

-- type Tile = IOUArray (Int,Int) Char

-- data World = World
--   { worldFun :: Int -> Int -> Char
--   , tileMap  :: HM.HashMap (Int,Int) Tile
--   , worldMap :: HM.HashMap (Int,Int) Char
--   }

-- wlookup :: World -> (Int,Int) -> (Char, World)
-- wlookup w@(World _ tiles _) (i,j)
--   | tileIndex `HM.member` tiles = ((tiles HM.! tileIndex) U.! (i .&. tileMask, j .&. tileMask), w)
--   | otherwise                   = wlookup (loadTile tileIndex w) (i,j)
--   where
--     tileIndex = (i `shiftR` tileBits, j `shiftR` tileBits)

-- loadTile :: (Int,Int) -> World -> World
-- loadTile (i,j) (World f t w)
--   = World f (HM.insert (i,j) newTile t) w
--   where
--     newTile = listArray ((0,0), (tileMask, tileMask)) (map cellValue indexList)
--     cellValue ix = fromMaybe (uncurry f ix) (HM.lookup ix w)
--     indexList = [(ii,jj) | ii <- [i `shiftL` tileBits .. ((i+1) `shiftL` tileBits) - 1]
--                          , jj <- [j `shiftL` tileBits .. ((j+1) `shiftL` tileBits) - 1]]
