{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Orphan JSON instances for Location and Heading

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Locations and headings
--
-- Locations and headings.
module Swarm.Game.Location (
  Location,
  pattern Location,
  HasLocation (..),

  -- ** Heading and Direction functions
  Heading,
  applyTurn,
  relativeTo,
  toDirection,
  toAbsDirection,
  nearestDirection,
  isCardinal,
  north,
  south,
  east,
  west,

  -- ** Utility functions
  manhattan,
  euclidean,
  asVector,
  getLocsInArea,
  getElemsInArea,

  -- ** Re-exports for convenience
  Affine (..),
  Point (..),
  origin,
  toHeading,
) where

import Control.Arrow ((&&&))
import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.Function (on, (&))
import Data.Int (Int32)
import Data.List (nub)
import Data.List.Extra (enumerate)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Yaml (FromJSON (parseJSON), ToJSON (toJSON))
import Linear (Additive (..), V2 (..), negated, norm, perp, unangle)
import Linear.Affine (Affine (..), Point (..), origin)
import Swarm.Language.Syntax.Direction (AbsoluteDir (..), Direction (..), PlanarRelativeDir (..), RelativeDir (..), isCardinal)
import Swarm.Util qualified as Util

-- $setup
-- >>> import qualified Data.Map as Map
-- >>> import Linear
-- >>> import Swarm.Language.Syntax.Direction

-- | A t'Location' is a pair of @(x,y)@ coordinates, both up to 32 bits.
--   The positive x-axis points east and the positive y-axis points
--   north.  These are the coordinates that are shown to players.
--
--   See also the 'Swarm.Game.World.Coords' type defined in "Swarm.Game.World", which
--   use a (row, column) format instead, which is more convenient for
--   internal use.  The "Swarm.Game.World" module also defines
--   conversions between 'Location' and 'Swarm.Game.World.Coords'.
type Location = Point V2 Int32

-- | A convenient way to pattern-match on 'Location' values.
pattern Location :: Int32 -> Int32 -> Location
pattern Location x y = P (V2 x y)

{-# COMPLETE Location #-}

instance FromJSON Location where
  parseJSON = fmap P . parseJSON

instance ToJSON Location where
  toJSON (P v) = toJSON v

-- | A @Heading@ is a 2D vector, with 32-bit coordinates.
--
--   t'Location' and 'Heading' are both represented using types from
--   the @linear@ package, so they can be manipulated using a large
--   number of operators from that package.  For example:
--
--   * Two headings can be added with '^+^'.
--   * The difference between two t'Location's is a 'Heading' (via '.-.').
--   * A t'Location' plus a 'Heading' is another t'Location' (via 'Linear.Affine..^+').
type Heading = V2 Int32

deriving instance ToJSON (V2 Int32)
deriving instance FromJSON (V2 Int32)

deriving instance FromJSONKey (V2 Int32)
deriving instance ToJSONKey (V2 Int32)

toHeading :: AbsoluteDir -> Heading
toHeading = \case
  DNorth -> north
  DSouth -> south
  DEast -> east
  DWest -> west

-- | The cardinal direction north = @V2 0 1@.
north :: Heading
north = V2 0 1

-- | The cardinal direction south = @V2 0 (-1)@.
south :: Heading
south = V2 0 (-1)

-- | The cardinal direction east = @V2 1 0@.
east :: Heading
east = V2 1 0

-- | The cardinal direction west = @V2 (-1) 0@.
west :: Heading
west = V2 (-1) 0

-- | The direction for viewing the current cell = @V2 0 0@.
down :: Heading
down = zero

-- | The 'applyTurn' function gives the meaning of each 'Direction' by
--   turning relative to the given heading or by turning to an absolute
--   heading.
--
--   >>> applyTurn (DRelative (DPlanar DLeft)) (V2 5 3)
--   V2 (-3) 5
--   >>> applyTurn (DAbsolute DWest) (V2 5 3)
--   V2 (-1) 0
applyTurn :: Direction -> Heading -> Heading
applyTurn d = case d of
  DRelative e -> case e of
    DPlanar DLeft -> perp
    DPlanar DRight -> negated . perp
    DPlanar DBack -> negated
    DPlanar DForward -> id
    DDown -> const down
  DAbsolute e -> const $ toHeading e

-- | Mapping from heading to their corresponding cardinal directions.
--   Only absolute directions are mapped.
cardinalDirs :: M.Map Heading AbsoluteDir
cardinalDirs =
  M.fromList $ map (toHeading &&& id) enumerate

-- | Possibly convert a heading into a 'Direction'---that is, if the
--   vector happens to be a unit vector in one of the cardinal
--   directions.
--
--   >>> toDirection (V2 0 (-1))
--   Just (DAbsolute DSouth)
--   >>> toDirection (V2 3 7)
--   Nothing
toDirection :: Heading -> Maybe Direction
toDirection = fmap DAbsolute . toAbsDirection

-- | Like 'toDirection', but preserve the type guarantee of an absolute direction
toAbsDirection :: Heading -> Maybe AbsoluteDir
toAbsDirection v = M.lookup v cardinalDirs

-- | Return the 'PlanarRelativeDir' which would result in turning to
--   the first (target) direction from the second (reference) direction.
--
--   >>> DWest `relativeTo` DSouth
--   DRight
--   >>> DWest `relativeTo` DWest
--   DForward
relativeTo :: AbsoluteDir -> AbsoluteDir -> PlanarRelativeDir
relativeTo targetDir referenceDir =
  toEnum indexDiff
 where
  enumCount = length (enumerate :: [AbsoluteDir])
  indexDiff = ((-) `on` fromEnum) targetDir referenceDir `mod` enumCount

-- | Compute the absolute direction nearest to a given 'Heading'.
--
--   Logic adapted from <https://gamedev.stackexchange.com/questions/49290/#comment213403_49300>.
nearestDirection :: Heading -> AbsoluteDir
nearestDirection coord =
  Util.indexWrapNonEmpty orderedDirs index
 where
  angle :: Double
  angle = unangle (fmap fromIntegral coord) / (2 * pi)

  index :: Int
  index = round $ fromIntegral (length orderedDirs) * angle
  orderedDirs = Util.enumerateNonEmpty

-- | Manhattan distance between world locations.
manhattan :: Location -> Location -> Int32
manhattan (Location x1 y1) (Location x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Euclidean distance between world locations.
euclidean :: Location -> Location -> Double
euclidean p1 p2 = norm (fromIntegral <$> (p2 .-. p1))

-- | Converts a 'Point' to a vector offset from the 'origin'.
asVector :: Location -> V2 Int32
asVector (P vec) = vec

-- | Get all the locations that are within a certain manhattan
--   distance from a given location.
--
-- >>> getLocsInArea (P (V2 0 0)) 1
-- [P (V2 0 0),P (V2 0 1),P (V2 0 (-1)),P (V2 1 0),P (V2 (-1) 0)]
-- >>> map (\i -> length (getLocsInArea origin i)) [0..8]
-- [1,5,13,25,41,61,85,113,145]
--
--   See also @Swarm.Game.Step.Const.genDiamondSides@.
getLocsInArea :: Location -> Int32 -> [Location]
getLocsInArea loc r =
  [loc .+^ V2 dx dy | x <- [0 .. r], y <- [0 .. r - x], dx <- nub [x, -x], dy <- nub [y, -y]]

-- | Get elements that are within a certain manhattan distance from location.
--
-- >>> v2s i = [(p, manhattan origin p) | x <- [-i..i], y <- [-i..i], let p = Location x y]
-- >>> v2s 0
-- [(P (V2 0 0),0)]
-- >>> map (\i -> length (getElemsInArea origin i (Map.fromList $ v2s i))) [0..8]
-- [1,5,13,25,41,61,85,113,145]
--
-- The last test is the sequence "Centered square numbers":
-- https://oeis.org/A001844
getElemsInArea :: Location -> Int32 -> Map Location e -> [e]
getElemsInArea o@(Location x y) d m = M.elems sm'
 where
  -- to be more efficient we basically split on first coordinate
  -- (which is logarithmic) and then we have to linearly filter
  -- the second coordinate to get a square - this is how it looks:
  --         ▲▲▲▲
  --         ││││    the arrows mark points that are greater then A
  --         ││s│                                 and lesser then B
  --         │sssB (2,1)
  --         ssoss   <-- o=(x=0,y=0) with d=2
  -- (-2,-1) Asss│
  --          │s││   the point o and all s are in manhattan
  --          ││││                  distance 2 from point o
  --          ▼▼▼▼
  sm =
    m
      & M.split (Location (x - d) (y - 1)) -- A
      & snd -- A<
      & M.split (Location (x + d) (y + 1)) -- B
      & fst -- B>
  sm' = M.filterWithKey (const . (<= d) . manhattan o) sm

-- * Locatable things

class HasLocation a where
  -- | Basically 'fmap' for the 'Location' field of a record
  modifyLoc :: (Location -> Location) -> a -> a

  -- | Translation by a vector
  offsetLoc :: V2 Int32 -> a -> a
  offsetLoc locOffset = modifyLoc (.+^ locOffset)
