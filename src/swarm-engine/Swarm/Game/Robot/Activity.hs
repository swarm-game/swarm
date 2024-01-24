{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Live activity monitoring for instantiated robots.
module Swarm.Game.Robot.Activity (
  ActivityCounts,
  tickStepBudget,
  tangibleCommandCount,
  commandsHistogram,
  lifetimeStepCount,
  activityWindow,
  emptyActivityCount,
) where

import Control.Lens hiding (Const, contains)
import Data.Aeson qualified as Ae (FromJSON (..), ToJSON (..))
import Data.Map (Map)
import GHC.Generics (Generic)
import Swarm.Game.Tick
import Swarm.Language.Syntax (Const)
import Swarm.Util.Lens (makeLensesNoSigs)
import Swarm.Util.WindowedCounter

data ActivityCounts = ActivityCounts
  { _tickStepBudget :: Int
  , _tangibleCommandCount :: Int
  , _commandsHistogram :: Map Const Int
  , _lifetimeStepCount :: Int
  , _activityWindow :: WindowedCounter TickNumber
  }
  deriving (Eq, Show, Generic, Ae.FromJSON, Ae.ToJSON)

emptyActivityCount :: ActivityCounts
emptyActivityCount =
  ActivityCounts
    { _tickStepBudget = 0
    , _tangibleCommandCount = 0
    , _commandsHistogram = mempty
    , _lifetimeStepCount = 0
    , -- NOTE: This value was chosen experimentally.
      -- TODO(#1341): Make this dynamic based on game speed.
      _activityWindow = mkWindow 64
    }

makeLensesNoSigs ''ActivityCounts

-- | A counter that is decremented upon each step of the robot within the
--   CESK machine. Initially set to 'Swarm.Game.State.robotStepsPerTick'
--   at each new tick.
--
--   The need for 'tickStepBudget' is a bit technical, and I hope I can
--   eventually find a different, better way to accomplish it.
--   Ideally, we would want each robot to execute a single
--   /command/ at every game tick, so that /e.g./ two robots
--   executing @move;move;move@ and @repeat 3 move@ (given a
--   suitable definition of @repeat@) will move in lockstep.
--   However, the second robot actually has to do more computation
--   than the first (it has to look up the definition of @repeat@,
--   reduce its application to the number 3, etc.), so its CESK
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
--   (defined in 'Swarm.Game.Step.evalStepsPerTick'), and it
--   suspends computation when it either executes a command or
--   reaches the maximum number of steps, whichever comes first.
--
--   It seems like this really isn't something the robot should be
--   keeping track of itself, but that seemed the most technically
--   convenient way to do it at the time.  The robot needs some
--   way to signal when it has executed a command, which it
--   currently does by setting tickStepBudget to zero.  However, that
--   has the disadvantage that when tickStepBudget becomes zero, we
--   can't tell whether that happened because the robot ran out of
--   steps, or because it executed a command and set it to zero
--   manually.
tickStepBudget :: Lens' ActivityCounts Int

-- | Total number of tangible commands executed over robot's lifetime
tangibleCommandCount :: Lens' ActivityCounts Int

-- | Histogram of commands executed over robot's lifetime
commandsHistogram :: Lens' ActivityCounts (Map Const Int)

-- | Total number of CESK steps executed over robot's lifetime.
-- This could be thought of as "CPU cycles" consumed, and is labeled
-- as "cycles" in the F2 dialog in the UI.
lifetimeStepCount :: Lens' ActivityCounts Int

-- | Sliding window over a span of ticks indicating ratio of activity
activityWindow :: Lens' ActivityCounts (WindowedCounter TickNumber)
