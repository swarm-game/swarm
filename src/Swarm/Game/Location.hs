{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Orphan JSON instances for Location and Heading

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Locations and headings.
module Swarm.Game.Location (
  Location,
  pattern Location,

  -- ** Heading and Direction functions
  Heading,
  applyTurn,
  relativeTo,
  toDirection,
  nearestDirection,
  fromDirection,
  isCardinal,
  north,
  south,
  east,
  west,

  -- ** utility functions
  manhattan,
  euclidean,
  getElemsInArea,
  sigdir,

  -- ** reexports for convenience
  Affine (..),
  Point (..),
  origin,
) where

import Control.Arrow ((&&&))
import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.Function (on, (&))
import Data.Int (Int32)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Yaml (FromJSON (parseJSON), ToJSON (toJSON))
import Linear (Additive (..), V2 (..), negated, norm, perp, unangle)
import Linear.Affine (Affine (..), Point (..), origin)
import Swarm.Language.Syntax (AbsoluteDir (..), Direction (..), RelativeDir (..), isCardinal)
import Swarm.Util qualified as Util

-- $setup
-- >>> import qualified Data.Map as Map

-- | A Location is a pair of (x,y) coordinates, both up to 32 bits.
--   The positive x-axis points east and the positive y-axis points
--   north.  These are the coordinates that are shown to players.
--
--   See also the 'Coords' type defined in "Swarm.Game.World", which
--   use a (row, column) format instead, which is more convenient for
--   internal use.  The "Swarm.Game.World" module also defines
--   conversions between 'Location' and 'Coords'.
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
--   'Location' and 'Heading' are both represented using types from
--   the @linear@ package, so they can be manipulated using a large
--   number of operators from that package.  For example:
--
--   * Two headings can be added with '^+^'.
--   * The difference between two 'Location's is a 'Heading' (via '.-.').
--   * A 'Location' plus a 'Heading' is another 'Location' (via '.^+').
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
--   heading
applyTurn :: Direction -> Heading -> Heading
applyTurn d = case d of
  DRelative e -> case e of
    DLeft -> perp
    DRight -> negated . perp
    DBack -> negated
    DDown -> const down
    DForward -> id
  DAbsolute e -> const $ toHeading e

-- | Mapping from heading to their corresponding cardinal directions.
--   Only absolute directions are mapped.
cardinalDirs :: M.Map Heading Direction
cardinalDirs =
  M.fromList $ map (toHeading &&& DAbsolute) Util.listEnums

-- | Possibly convert a heading into a 'Direction'---that is, if the
--   vector happens to be a unit vector in one of the cardinal
--   directions.
toDirection :: Heading -> Maybe Direction
toDirection v = M.lookup v cardinalDirs

-- | Example:
--      DWest `relativeTo` DSouth == DRight
relativeTo :: AbsoluteDir -> AbsoluteDir -> RelativeDir
relativeTo targetDir referenceDir =
  [DForward, DLeft, DBack, DRight] !! indexDiff
 where
  enumCount = length (Util.listEnums :: [AbsoluteDir])
  indexDiff = ((-) `on` fromEnum) targetDir referenceDir `mod` enumCount

-- | Substitutes all nonzero values with one, preserving sign.
-- Compare to "signorm", which is constrained to class "Floating":
-- https://hackage.haskell.org/package/linear-1.22/docs/Linear-Metric.html#v:signorm
sigdir :: (Ord a, Num a) => V2 a -> V2 a
sigdir = fmap $ signOrdering . compare 0

signOrdering :: Num a => Ordering -> a
signOrdering = \case
  LT -> -1
  EQ -> 0
  GT -> 1

-- | Logic adapted from:
-- https://gamedev.stackexchange.com/questions/49290/#comment213403_49300
nearestDirection :: Heading -> AbsoluteDir
nearestDirection coord =
  orderedDirs !! index
 where
  angle :: Double
  angle = unangle (fmap fromIntegral coord) / (2 * pi)

  index = round (fromIntegral enumCount * angle) `mod` enumCount
  orderedDirs = Util.listEnums
  enumCount = length orderedDirs

-- | Convert a 'Direction' into a corresponding heading.  Note that
--   this only does something reasonable for 'DNorth', 'DSouth', 'DEast',
--   and 'DWest'---other 'Direction's return the zero vector.
fromDirection :: Direction -> Heading
fromDirection = \case
  DAbsolute x -> toHeading x
  _ -> zero

-- | Manhattan distance between world locations.
manhattan :: Location -> Location -> Int32
manhattan (Location x1 y1) (Location x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Euclidean distance between world locations.
euclidean :: Location -> Location -> Double
euclidean p1 p2 = norm (fromIntegral <$> (p2 .-. p1))

-- | Get elements that are in manhattan distance from location.
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
