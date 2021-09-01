{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.Robot
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A data type to represent robots.
--
-----------------------------------------------------------------------------

module Swarm.Game.Robot
  ( -- * Robots

    Robot(..)

    -- ** Lenses
  , robotName, robotDisplay, robotLocation, robotOrientation, robotInventory
  , machine, tickSteps, static

    -- ** Create

  , mkRobot, baseRobot

    -- ** Query

  , isActive, getResult
  ) where

import           Control.Lens
import           Data.Maybe         (isNothing)
import           Data.Text          (Text)
import           Linear

import           Swarm.Game.CEK
import           Swarm.Game.Display
import           Swarm.Game.Entity
import           Swarm.Game.Value

-- | A value of type 'Robot' is a record representing the state of a
--   single robot.
data Robot = Robot
  { _robotEntity   :: Entity
    -- ^ An entity record storing the robot's name, display,
    --   inventory, and so on.

  , _robotLocation :: V2 Int
    -- ^ The location of the robot as (x,y).

  , _machine       :: CEK
    -- ^ The current state of the robot's CEK machine.

  , _tickSteps     :: Int
    -- ^ The need for 'tickSteps' is a bit technical, and I hope I can
    --   eventually find a different, better way to accomplish it.
    --   Ideally, we would want each robot to execute a single
    --   /command/ at every game tick, so that /e.g./ two robots
    --   executing @move;move;move@ and @repeat 3 move@ (given a
    --   suitable definition of @repeat@) will move in lockstep.
    --   However, the second robot actually has to do more computation
    --   than the first (it has to look up the definition of @repeat@,
    --   reduce its application to the number 3, etc.), so its CEK
    --   machine will take more steps.  It won't do to simply let each
    --   robot run until executing a command---because robot programs
    --   can involve arbitrary recursion, it is very easy to write a
    --   program that evaluates forever without ever executing a
    --   command, which in this scenario would completely freeze the
    --   UI. (It also wouldn't help to ensure all programs are
    --   terminating---it would still be possible to effectively do
    --   the same thing by making a program that takes a very, very
    --   long time to terminate.)  So instead, we allocate each robot
    --   a certain maximum number of computation steps per tick
    --   (defined in 'evalStepsPerTick'), and it suspends computation
    --   when it either executes a command or reaches the maximum
    --   number of steps, whichever comes first.
    --
    --   It seems like this really isn't something the robot should be
    --   keeping track of itself, but that seemed the most technically
    --   convenient way to do it at the time.  The robot needs some
    --   way to signal when it has executed a command, which it
    --   currently does by setting tickSteps to zero.  However, that
    --   has the disadvantage that when tickSteps becomes zero, we
    --   can't tell whether that happened because the robot ran out of
    --   steps, or because it executed a command and set it to zero
    --   manually.
    --
    --   Perhaps instead, each robot should keep a counter saying how
    --   many commands it has executed.  The loop stepping the robot
    --   can tell when the counter increments.

  , _static        :: Bool
    -- ^ Whether the robot is allowed to move, turn, or harvest.
    --   Currently this is a hack to prevent the "base" robot from
    --   moving; eventually this should go away, and the base will be
    --   prevented from moving simply because it does not have treads
    --   (or whatever the right device is for moving), and so on.
  }
  deriving (Eq, Ord, Show)

makeLenses ''Robot

robotName :: Lens' Robot Text
robotName = robotEntity . entityName

robotDisplay :: Lens' Robot Display
robotDisplay = robotEntity . entityDisplay

robotOrientation :: Lens' Robot (Maybe (V2 Int))
robotOrientation = robotEntity . entityOrientation

robotInventory :: Lens' Robot Inventory
robotInventory = robotEntity . entityInventory

-- | Create a robot.
mkRobot
  :: Text    -- ^ Name of the robot.  Precondition: it should not be the same as any
             --   other robot name.
  -> V2 Int  -- ^ Initial location.
  -> V2 Int  -- ^ Initial heading/direction.
  -> CEK     -- ^ Initial CEK machine.
  -> Robot
mkRobot name l d m = Robot
  { _robotEntity  = mkEntity
      defaultRobotDisplay
      name
      "A generic robot."
      (Just d)
      []
  , _robotLocation = l
  , _machine       = m
  , _tickSteps     = 0
  , _static        = False
  }

-- | The initial robot representing your "base".
baseRobot :: Robot
baseRobot = Robot
  { _robotEntity = mkEntity
      defaultRobotDisplay
      "base"
      "Your base of operations."
      Nothing
      []
  , _robotLocation = V2 0 0
  , _machine       = idleMachine
  , _tickSteps     = 0
  , _static        = True
  }

-- | Is the robot actively in the middle of a computation?
isActive :: Robot -> Bool
isActive = isNothing . getResult

-- | Get the result of the robot's computation if it is finished.
getResult :: Robot -> Maybe Value
getResult = finalValue . view machine

