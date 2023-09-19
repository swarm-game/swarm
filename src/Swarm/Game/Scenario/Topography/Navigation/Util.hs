-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Navigation.Util where

import Control.Lens (view)
import Data.Function (on)
import Data.Int (Int32)
import Linear (V2, zero)
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.Universe (Cosmic, planar)
import Swarm.Language.Direction

-- | Given a vector @(x, y)@
-- where:
--
-- * positive x-coordinate represents @east@
-- * negative x-coordinate represents @west@
-- * positive y-coordinate represents @north@
-- * negative y-coordinate represents @south@
--
-- re-interpret as relative to a given
-- orientation, where:
--
-- * positive x-coordinate represents @right@
-- * negative x-coordinate represents @left@
-- * positive y-coordinate represents @forward@
-- * negative y-coordinate represents @back@
orientationBasedRelativePosition :: Robot -> Cosmic Location -> V2 Int32
orientationBasedRelativePosition selfRobot otherLocation =
  maybe zero (`applyTurn` relativeCoords) maybeSelfDirRelativeToNorth
 where
  maybeSelfDirection = view robotOrientation selfRobot >>= toAbsDirection
  maybeSelfDirRelativeToNorth = DRelative . DPlanar . relativeTo DNorth <$> maybeSelfDirection

  relativeCoords = ((.-.) `on` view planar) otherLocation (view robotLocation selfRobot)
