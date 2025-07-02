-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A "view chunk" is the smallest rectangular unit by which the game caches XXX world view
--
module Swarm.TUI.Model.ViewChunk where

import Control.Lens
import Data.Bits (shiftL, shiftR)
import Data.Int (Int32)
import Data.Ix (range)
import Data.List.Split (chunksOf)
import Swarm.Game.Universe
import Swarm.Game.World.Coords

newtype ViewChunk = ViewChunk { unViewChunk :: Cosmic Coords }
  deriving (Eq, Ord, Show, Read)

-- | XXX
viewChunkBits :: Int
viewChunkBits = 4

viewChunkSize :: Int32
viewChunkSize = 1 `shiftL` viewChunkBits

viewChunk :: Cosmic Coords -> ViewChunk
viewChunk c = ViewChunk (over (_Wrapped . both) (`shiftR` viewChunkBits) (c ^. planar) <$ c)

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
viewChunkCover :: Cosmic BoundsRectangle -> [[ViewChunk]]
viewChunkCover c@(Cosmic _ (ul, lr)) = chunksOf (fromIntegral cols) (map ViewChunk vcCoords)
  where
    ulvc = viewChunk (ul <$ c)
    lrvc = viewChunk (lr <$ c)
    getCol = snd . unCoords . view planar . unViewChunk
    cols = getCol lrvc - getCol ulvc + 1
    vcCoords :: [Cosmic Coords]
    vcCoords = traverse (curry range (unViewChunk ulvc ^. planar)) (unViewChunk lrvc)
