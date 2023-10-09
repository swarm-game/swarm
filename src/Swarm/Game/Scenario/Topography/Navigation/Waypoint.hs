{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Landmarks that are used to specify portal locations
-- and can serve as navigation aids via the `waypoint` command.
--
-- = Waypoint ordering
--
-- The sequence of waypoints of a given name is dictated by criteria in the following order:
--
-- 1. Ordering of structure placements
--    (see implementation of 'Swarm.Game.Scenario.Topography.Structure.mergeStructures');
--    later placements are ordered first.
-- 2. Placement of cells within a map. Map locations go by row-major order
--    (compare to docs for 'Swarm.Game.State.genRobotTemplates').
--
-- TODO (#1366): May be useful to have a mechanism for more
-- precise control of ordering.
module Swarm.Game.Scenario.Topography.Navigation.Waypoint where

import Data.Int (Int32)
import Data.Text qualified as T
import Data.Yaml as Y
import GHC.Generics (Generic)
import Linear (V2 (..))
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Placement

-- | This type is isomorphic to 'Maybe'.
data Parentage a
  = WithParent a
  | Root
  deriving (Show, Eq)

-- | Indicates which structure something came from
-- for debugging purposes.
data Originated a = Originated
  { parent :: Parentage Placement
  , value :: a
  }
  deriving (Show, Eq, Functor)

newtype WaypointName = WaypointName T.Text
  deriving (Show, Eq, Ord, Generic, FromJSON)

-- | Metadata about a waypoint
data WaypointConfig = WaypointConfig
  { wpName :: WaypointName
  , wpUnique :: Bool
  -- ^ Enforce global uniqueness of this waypoint
  }
  deriving (Show, Eq)

parseWaypointConfig :: Object -> Parser WaypointConfig
parseWaypointConfig v =
  WaypointConfig
    <$> v .: "name"
    <*> v .:? "unique" .!= False

instance FromJSON WaypointConfig where
  parseJSON = withObject "Waypoint Config" parseWaypointConfig

-- |
-- A parent world shouldn't have to know the exact layout of a subworld
-- to specify where exactly a portal will deliver a robot to within the subworld.
-- Therefore, we define named waypoints in the subworld and the parent world
-- must reference them by name, rather than by coordinate.
data Waypoint = Waypoint
  { wpConfig :: WaypointConfig
  , wpLoc :: Location
  }
  deriving (Show, Eq)

-- | JSON representation is flattened; all keys are at the same level,
-- in contrast with the underlying record.
instance FromJSON Waypoint where
  parseJSON = withObject "Waypoint" $ \v ->
    Waypoint
      <$> parseWaypointConfig v
      <*> v .: "loc"

-- | Basically "fmap" for the "Location" field
modifyLocation ::
  (Location -> Location) ->
  Waypoint ->
  Waypoint
modifyLocation f (Waypoint cfg originalLoc) = Waypoint cfg $ f originalLoc

-- | Translation by a vector
offsetWaypoint ::
  V2 Int32 ->
  Waypoint ->
  Waypoint
offsetWaypoint locOffset = modifyLocation (.+^ locOffset)
