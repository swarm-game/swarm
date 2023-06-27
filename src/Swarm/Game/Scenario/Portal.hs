-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Portal where

import Swarm.Game.Scenario.Structure
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Note: The primary overworld shall use
-- the reserved name \"root\".
newtype SubworldName = SubworldName Text
  deriving (Show, Eq, Ord, Generic, FromJSON)

data PortalExit = PortalExit {
    exit :: WaypointName
    -- | Note: 'Nothing' indicates that references a waypoint within the same subworld.
  , subworldName :: Maybe SubworldName
} deriving (Show, Eq, Generic, FromJSON)

data Portal = Portal
  { entrance :: WaypointName
  , exitInfo :: PortalExit
  }
  deriving (Show, Eq, Generic, FromJSON)
