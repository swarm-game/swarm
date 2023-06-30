-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Portal where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Game.Scenario.Structure

-- | Note: The primary overworld shall use
-- the reserved name \"root\".
newtype SubworldName = SubworldName Text
  deriving (Show, Eq, Ord, Generic, FromJSON)

data PortalExit = PortalExit
  { exit :: WaypointName
  , subworldName :: Maybe SubworldName
  -- ^ Note: 'Nothing' indicates that references a waypoint within the same subworld.
  }
  deriving (Show, Eq, Generic, FromJSON)

data Portal = Portal
  { entrance :: WaypointName
  , exitInfo :: PortalExit
  }
  deriving (Show, Eq, Generic, FromJSON)
