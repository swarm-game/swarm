-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A "view chunk" is the smallest rectangular unit by which the game caches XXX world view
--
module Swarm.TUI.Model.ViewChunk where

import Control.Lens
import Data.Bits (shiftL, shiftR)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Swarm.Game.Location (Location)
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.Util (chunksOfNE, rangeNE)

newtype ViewChunk = ViewChunk { unViewChunk :: Cosmic Coords }
  deriving (Eq, Ord, Show, Read)

-- | XXX
viewChunkBits :: Int
viewChunkBits = 4

viewChunkSize :: Int32
viewChunkSize = 1 `shiftL` viewChunkBits

viewChunk :: Cosmic Coords -> ViewChunk
viewChunk c = ViewChunk (over (_Wrapped . both) (`shiftR` viewChunkBits) (c ^. planar) <$ c)

viewChunkFor :: Cosmic Location -> ViewChunk
viewChunkFor = viewChunk . fmap locToCoords

viewChunkBounds :: ViewChunk -> Cosmic BoundsRectangle
viewChunkBounds (ViewChunk cc) = (,lr) <$> ul
  where
    ul = mapCoords (`shiftL` viewChunkBits) <$> cc
    lr = addTuple (ul ^. planar) (viewChunkSize - 1, viewChunkSize - 1)

-- XXX property-based test that viewChunkBounds always produces rectangle which is viewChunkSize^2
-- XXX add a property-based test that c is always within viewChunkBounds (viewChunk c)

-- XXX generate smallest possible list of ViewChunks necessary to
-- entirely cover the given BoundsRectangle.  Returned in row-major
-- (i.e. top-to-bottom, left-to-right) order.  i.e. a list of rows
-- XXX change to use NonEmpty
viewChunkCover :: Cosmic BoundsRectangle -> NonEmpty (NonEmpty ViewChunk)
viewChunkCover c@(Cosmic _ (tl, br)) = chunksOfNE (fromIntegral cols) (fmap ViewChunk vcCoords)
  where
    tlvc = viewChunk (tl <$ c)
    brvc = viewChunk (br <$ c)
    getCol = snd . unCoords . view planar . unViewChunk
    cols = getCol brvc - getCol tlvc + 1
    vcCoords :: NonEmpty (Cosmic Coords)
    vcCoords = traverse (curry rangeNE (unViewChunk tlvc ^. planar)) (unViewChunk brvc)
