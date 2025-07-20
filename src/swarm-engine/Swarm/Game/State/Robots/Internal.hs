{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Robot-specific subrecords and utilities used by 'Swarm.Game.State.GameState'.
--
-- This module exports internal lenses that break invariants and should be imported qualified.
module Swarm.Game.State.Robots.Internal (
  Robots,
  initRobots,

  -- ** Lenses
  robotMap,
  currentTickWakeableBots,
  robotsByLocation,
  robotsWatching,
  robotNaming,
  viewCenterState,

  -- *** Internal lenses
  activeRobots,
  waitingRobots,
) where

import Control.Lens (Lens')
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.MonoidMap (MonoidMap)
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.State.Config
import Swarm.Game.State.RobotNaming
import Swarm.Game.State.ViewCenter.Internal (ViewCenter, defaultViewCenter)
import Swarm.Game.Tick
import Swarm.Game.Universe as U
import Swarm.Util.Lens (makeLensesNoSigs)

-- | Robots specific part of the game state.
data Robots = Robots
  { _robotMap :: IntMap Robot
  , _activeRobots :: IntSet
  , _waitingRobots :: MonoidMap TickNumber [RID]
  , _currentTickWakeableBots :: IntSet
  , _robotsByLocation :: MonoidMap SubworldName (MonoidMap Location IntSet)
  , _robotsWatching :: MonoidMap (Cosmic Location) IntSet
  , _robotNaming :: RobotNaming
  , _viewCenterState :: ViewCenter
  }

-- | Initialize the robots state from configuration.
initRobots :: GameStateConfig -> Robots
initRobots gsc =
  Robots
    { _robotMap = IM.empty
    , _activeRobots = IS.empty
    , _waitingRobots = mempty
    , _currentTickWakeableBots = mempty
    , _robotsByLocation = mempty
    , _robotsWatching = mempty
    , _robotNaming = initRobotNaming gsc
    , _viewCenterState = defaultViewCenter
    }

makeLensesNoSigs ''Robots

-- | All the robots that currently exist in the game, indexed by ID.
robotMap :: Lens' Robots (IntMap Robot)

-- | The names of the robots that are currently not sleeping.
--
-- Formally, it is a set of robots to consider for the next game tick.
-- It is guaranteed to be a subset of the keys of 'robotMap'.
--
-- It may contain waiting or idle robots. But robots that are present
-- in 'robotMap' and not in 'activeRobots' are guaranteed to be either
-- waiting or idle.
activeRobots :: Lens' Robots IntSet

-- | The names of the robots that are currently sleeping, indexed by wake up
--   time. Note that this may not include all sleeping robots, particularly
--   those that are only taking a short nap (e.g. @wait 1@).
--
-- Formally, it is a set of probably waiting robots, indexed by probable wake-up time.
-- It may contain robots that are in fact active or idle, as well as robots
-- that do not exist anymore. Its only guarantee is that once a robot name
-- with its wake up time is inserted in it, it will remain there until the
-- wake-up time is reached, at which point it is removed via
-- 'wakeUpRobotsDoneSleeping'.
--
-- Waiting robots for a given time are a list because it is cheaper to
-- prepend to a list than insert into a 'Set'.
waitingRobots :: Lens' Robots (MonoidMap TickNumber [RID])

-- | Get a set of all the robots that are about to wake.
currentTickWakeableBots :: Lens' Robots IntSet

-- | The names of all robots that currently exist in the game, indexed by
--   location (which we need both for /e.g./ the @salvage@ command as
--   well as for actually drawing the world).  Unfortunately there is
--   no good way to automatically keep this up to date, since we don't
--   just want to completely rebuild it every time the 'robotMap'
--   changes.  Instead, we just make sure to update it every time the
--   location of a robot changes, or a robot is created or destroyed.
--   Fortunately, there are relatively few ways for these things to
--   happen.
robotsByLocation :: Lens' Robots (MonoidMap SubworldName (MonoidMap Location IntSet))

-- | Get a list of all the robots that are \"watching\" by location.
--
-- This is an optimization so that we do not have to iterate over all
-- "waiting" robots, since there may be many.
robotsWatching :: Lens' Robots (MonoidMap (Cosmic Location) IntSet)

-- | State and data for assigning identifiers to robots
robotNaming :: Lens' Robots RobotNaming

-- | The current view center location, focused robot and the rule for which to follow.
viewCenterState :: Lens' Robots ViewCenter
