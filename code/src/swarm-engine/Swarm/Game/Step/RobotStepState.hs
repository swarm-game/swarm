{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- This exists in its own module so that it can be
-- used by both "Swarm.Game.Step.Path.Cache" and
-- "Swarm.Game.Step.Util" without introducing an
-- import cycle.
module Swarm.Game.Step.RobotStepState where

import Control.Carrier.State.Lazy
import Control.Effect.Error
import Swarm.Game.Exception
import Swarm.Game.Robot
import Swarm.Game.State

-- | All functions that are used for robot step can access 'GameState' and the current 'Robot'.
--
-- They can also throw exception of our custom type, which is handled elsewhere.
-- Because of that the constraint is only 'Throw', but not 'Catch'/'Error'.
type HasRobotStepState sig m = (Has (State GameState) sig m, Has (State Robot) sig m, Has (Throw Exn) sig m)
