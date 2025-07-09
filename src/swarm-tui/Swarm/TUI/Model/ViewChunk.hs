-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A "view chunk" is a square 2^k x 2^k section of the world
-- which is individually cached when drawing the world.  View chunks
-- which have not changed do not need to be re-rendered.
module Swarm.TUI.Model.ViewChunk where

import Control.Lens
import Data.Bits (shiftL, shiftR)
import Data.Int (Int32)
import Data.Ix (rangeSize)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Swarm.Game.Location (Location)
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.Util (chunksOfNE, rangeNE)

-- | A 'ViewChunk' is a square 2^k x 2^k chunk of the world which is
--   individually cached when rendering.  View chunks are always
--   aligned with multiples of 2^k, so that the view chunk for given
--   coordinates can be determined by a simple bit shift.
--
--   Note the 'Coords' stored here are in "view chunk space", which is
--   expressed in units of 2^k.  For example, if @viewChunkBits = 2@
--   then the view chunk with coordinates @(1,2)@ spans world
--   coordinates @(4,8) - (7,11)@.
newtype ViewChunk = ViewChunk {unViewChunk :: Cosmic Coords}
  deriving (Eq, Ord, Show, Read)

-- | If viewChunkBits = k then each view chunk will be 2^k x 2^k
--   cells.  Empirically, k = 2 seems to give the best performance.
viewChunkBits :: Int
viewChunkBits = 2

-- | The number of cells on one side of a view chunk.
viewChunkSize :: Int32
viewChunkSize = 1 `shiftL` viewChunkBits

-- | Compute the view chunk that contains the given coordinates.
--   Since view chunks are always aligned to multiples of 2^k, this
--   can be computed simply by right shifting each coordinate by
--   'viewChunkBits'.
viewChunk :: Cosmic Coords -> ViewChunk
viewChunk c = ViewChunk (over (_Wrapped . both) (`shiftR` viewChunkBits) (c ^. planar) <$ c)

-- | Like 'viewChunk', but for a 'Location' instead of 'Coords'.
viewChunkFor :: Cosmic Location -> ViewChunk
viewChunkFor = viewChunk . fmap locToCoords

-- | Compute the bounding rectangle, in world coordinates, for a given
--   'ViewChunk'.  For example, if @viewChunkBits = 2@ then view chunk
--   @(1,2)@ will have bounds @(4,8) - (7,11)@.
viewChunkBounds :: ViewChunk -> Cosmic BoundsRectangle
viewChunkBounds (ViewChunk cc) = (,lr) <$> ul
 where
  ul = mapCoords (`shiftL` viewChunkBits) <$> cc
  lr = addTuple (ul ^. planar) (viewChunkSize - 1, viewChunkSize - 1)

-- | @viewChunkCover r@ generates the smallest possible set of
--   'ViewChunk's necessary to entirely cover the given rectangle @r@.
--   The resulting 'ViewChunk's are returned in row-major order, that
--   is, the first list represents the topmost row from left to right,
--   the second list represents the second row, and so on.
viewChunkCover :: Cosmic BoundsRectangle -> NonEmpty (NonEmpty ViewChunk)
viewChunkCover c@(Cosmic _ (tl, br))
  | rangeSize (tl, br) == 0 = NE.singleton (NE.singleton (viewChunk (tl <$ c)))
  | otherwise = chunksOfNE (fromIntegral cols) vcs
 where
  -- Coordinates of the top-left and bottom-right view chunks, in
  -- view chunk space
  tlvc, brvc :: Cosmic Coords
  tlvc = unViewChunk $ viewChunk (tl <$ c)
  brvc = unViewChunk $ viewChunk (br <$ c)

  -- Number of view chunks needed to cover the rectangle horizontally
  cols :: Int32
  cols = getCol brvc - getCol tlvc + 1
   where
    getCol = snd . unCoords . view planar

  -- List of all view chunks from top-left to bottom-right,
  -- generated using 'rangeNE'
  vcs :: NonEmpty ViewChunk
  vcs = ViewChunk <$> traverse (curry rangeNE (tlvc ^. planar)) brvc
