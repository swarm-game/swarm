-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Navigation.Util where

import Control.Lens (view)
import Data.Function (on)
import Data.Int (Int32)
import Linear (V2)
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.Universe (Cosmic, planar)
import Swarm.Language.Syntax (Phase (..))
import Swarm.Language.Syntax.Direction

-- |
-- Computes the relative offset vector between a 'Robot' and a 'Location'
-- (presumed to be in the same subworld, though the contrary will
-- not result in failure), then re-interpret that vector based on the
-- 'Robot'\'s current orientation.
--
-- If the robot is not oriented in a cardinal direction, returns 'Nothing'.
--
-- = Re-orientation semantics
--
-- Given a displacement vector @(x, y)@ where:
--
-- * positive @x@-coordinate represents @east@
-- * negative @x@-coordinate represents @west@
-- * positive @y@-coordinate represents @north@
-- * negative @y@-coordinate represents @south@
--
-- the re-interpreted vector @(x', y')@ becomes:
--
-- * positive @x'@-coordinate represents @right@
-- * negative @x'@-coordinate represents @left@
-- * positive @y'@-coordinate represents @forward@
-- * negative @y'@-coordinate represents @back@
orientationBasedRelativePosition :: Robot Instantiated -> Cosmic Location -> Maybe (V2 Int32)
orientationBasedRelativePosition selfRobot otherLocation =
  (`applyTurn` relativeCoords) <$> maybeSelfDirRelativeToNorth
 where
  maybeSelfDirection = view robotOrientation selfRobot >>= toAbsDirection
  maybeSelfDirRelativeToNorth = DRelative . DPlanar . relativeTo DNorth <$> maybeSelfDirection

  relativeCoords = ((.-.) `on` view planar) otherLocation (view robotLocation selfRobot)
