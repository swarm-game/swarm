{-# LANGUAGE PatternSynonyms #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- World coordinates.
module Swarm.Game.World.Coords (
  Coords (..),
  locToCoords,
  coordsToLoc,
  addTuple,
  mapCoords,
  BoundsRectangle,
)
where

import Control.Lens (Rewrapped, Wrapped)
import Data.Array.IArray (Ix)
import Data.Int (Int32)
import GHC.Generics (Generic)
import Swarm.Game.Location (Location, pattern Location)

------------------------------------------------------------
-- World coordinates
------------------------------------------------------------

-- | World coordinates use @(row,column)@ format, with the row
--   increasing as we move down the screen.  We use this format for
--   indexing worlds internally, since it plays nicely with things
--   like drawing the screen, and reading maps from configuration
--   files. The 'locToCoords' and 'coordsToLoc' functions convert back
--   and forth between this type and t'Location', which is used when
--   presenting coordinates externally to the player.
newtype Coords = Coords {unCoords :: (Int32, Int32)}
  deriving (Eq, Ord, Show, Read, Ix, Generic)

instance Rewrapped Coords t
instance Wrapped Coords

-- | Convert an external @(x,y)@ location to an internal 'Coords' value.
locToCoords :: Location -> Coords
locToCoords (Location x y) = Coords (-y, x)

-- | Convert an internal 'Coords' value to an external @(x,y)@ location.
coordsToLoc :: Coords -> Location
coordsToLoc (Coords (r, c)) = Location c (-r)

addTuple :: Coords -> (Int32, Int32) -> Coords
addTuple (Coords (r, c)) (addR, addC) = Coords (r + addR, c + addC)

mapCoords :: (Int32 -> Int32) -> Coords -> Coords
mapCoords f (Coords (r,c)) = Coords (f r, f c)

-- | Represents the top-left and bottom-right coordinates
-- of a bounding rectangle of cells in the world map
type BoundsRectangle = (Coords, Coords)
