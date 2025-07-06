-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Robot-specific subrecords and utilities used by 'Swarm.Game.State.GameState'
module Swarm.Game.State.Robot (
  -- * Types
  ViewCenterRule (..),
  Robots,

  -- * Initialization
  initRobots,
  setRobotInfo,

  -- * Lenses
  robotMap,
  robotsByLocation,
  robotsWatchingForEntities,
  robotsWatchingForRobots,
  activeRobots,
  waitingRobots,
  currentTickWakeableBots,
  robotNaming,

  -- ** View center lenses
  viewCenterRule,
  viewCenter,
  focusedRobotID,
  focusedRobot,

  -- * Utilities
  wakeRobotsWatchingForEntities,
  wakeRobotsWatchingForRobots,
  sleepUntil,
  sleepForever,
  wakeUpRobotsDoneSleeping,
  deleteRobot,
  removeRobotFromLocationMap,
  activateRobot,
  addRobot,
  addRobotToLocation,
  addTRobot,
  addTRobot',

  -- ** View
  modifyViewCenter,
  unfocus,
  recalcViewCenter,

  -- ** Robot naming
  RobotNaming,
  nameGenerator,
  gensym,
) where

import Control.Arrow (Arrow ((&&&)))
import Control.Effect.Lens
import Control.Effect.State (State)
import Control.Effect.Throw (Has)
import Control.Lens hiding (Const, use, uses, view, (%=), (+=), (.=), (<+=), (<<.=))
import Control.Monad (forM_, void)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.IntSet.Lens (setOf)
import Data.List (partition)
import Data.Maybe (mapMaybe)
import Data.MonoidMap (MonoidMap)
import Data.MonoidMap qualified as MM
import Data.Set qualified as S
import Swarm.Game.CESK (CESK (Waiting))
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.Robot.Concrete
import Swarm.Game.State.RobotNaming
import Swarm.Game.State.Robots.Internal hiding (activeRobots, waitingRobots)
import Swarm.Game.State.Robots.Internal qualified as Internal (activeRobots, waitingRobots)
import Swarm.Game.State.ViewCenter.Internal (ViewCenterRule (..))
import Swarm.Game.State.ViewCenter.Internal qualified as VCInternal
import Swarm.Game.Tick
import Swarm.Game.Universe as U
import Swarm.Util ((<+=), (<<.=))

-- | The names of the robots that are currently not sleeping.
activeRobots :: Getter Robots IntSet
activeRobots = Internal.activeRobots

-- | The names of the robots that are currently sleeping, indexed by wake up
--   time. Note that this may not include all sleeping robots, particularly
--   those that are only taking a short nap (e.g. @wait 1@).
waitingRobots :: Getter Robots (MonoidMap TickNumber [RID])
waitingRobots = Internal.waitingRobots

-- | The current center of the world view. Note that this cannot be
--   modified directly, since it is calculated automatically from the
--   'viewCenterRule'.  To modify the view center, either set the
--   'viewCenterRule', or use 'modifyViewCenter'.
viewCenter :: Getter Robots (Cosmic Location)
viewCenter = viewCenterState . VCInternal.viewCenterLocation

-- | The current robot in focus.
--
-- It is only a 'Getter' because this value should be updated only when
-- the 'viewCenterRule' is specified to be a robot.
--
-- Technically it's the last robot ID specified by 'viewCenterRule',
-- but that robot may not be alive anymore - to be safe use 'focusedRobot'.
focusedRobotID :: Getter Robots RID
focusedRobotID = viewCenterState . VCInternal.viewRobotID

-- | Find out which robot has been last specified by the
--   'viewCenterRule', if any.
focusedRobot :: Getter Robots (Maybe Robot)
focusedRobot = to $ \r -> r ^. robotMap . at (r ^. focusedRobotID)

-- | The current rule for determining the center of the world view.
--   It also updates 'viewCenter' and 'focusedRobot' to keep
--   everything synchronized.
viewCenterRule :: Lens' Robots ViewCenterRule
viewCenterRule = lens getter setter
 where
  getter :: Robots -> ViewCenterRule
  getter = view $ viewCenterState . VCInternal.viewCenterRule
  setter :: Robots -> ViewCenterRule -> Robots
  setter rInfo rule =
    rInfo
      & viewCenterState . VCInternal.viewCenterRule .~ rule
      & viewCenterState %~ VCInternal.syncViewCenter (getLocationFromRID rInfo)

-- | Add a concrete instance of a robot template to the game state:
--   First, generate a unique ID number for it.  Then, add it to the
--   main robot map, the active robot set, and to to the index of
--   robots by location.
addTRobot :: (Has (State Robots) sig m) => CESK -> TRobot -> m ()
addTRobot m r = void $ addTRobot' m r

-- | Like addTRobot, but return the newly instantiated robot.
addTRobot' :: (Has (State Robots) sig m) => CESK -> TRobot -> m Robot
addTRobot' initialMachine r = do
  rid <- robotNaming . gensym <+= 1
  let newRobot = instantiateRobot (Just initialMachine) rid r
  addRobot newRobot
  return newRobot

-- | Add a robot to the game state, adding it to the main robot map,
--   the active robot set, and to to the index of robots by
--   location.
addRobot :: (Has (State Robots) sig m) => Robot -> m ()
addRobot r = do
  robotMap %= IM.insert rid r
  addRobotToLocation rid $ r ^. robotLocation
  Internal.activeRobots %= IS.insert rid
 where
  rid = r ^. robotID

-- | Helper function for updating the "robotsByLocation" bookkeeping
addRobotToLocation :: (Has (State Robots) sig m) => RID -> Cosmic Location -> m ()
addRobotToLocation rid rLoc =
  robotsByLocation
    %= MM.adjust (MM.adjust (IS.insert rid) (rLoc ^. planar)) (rLoc ^. subworld)

-- | Takes a robot out of the 'activeRobots' set and puts it in the 'waitingRobots'
--   queue.
sleepUntil :: (Has (State Robots) sig m) => RID -> TickNumber -> m ()
sleepUntil rid time = do
  Internal.activeRobots %= IS.delete rid
  Internal.waitingRobots %= MM.adjust (rid :) time

-- | Takes a robot out of the 'activeRobots' set.
sleepForever :: (Has (State Robots) sig m) => RID -> m ()
sleepForever rid = Internal.activeRobots %= IS.delete rid

-- | Adds a robot to the 'activeRobots' set.
activateRobot :: (Has (State Robots) sig m) => RID -> m ()
activateRobot rid = Internal.activeRobots %= IS.insert rid

-- | Removes robots whose wake up time matches the current game ticks count
--   from the 'waitingRobots' queue and put them back in the 'activeRobots' set
--   if they still exist in the keys of 'robotMap'.
--
-- = Mutations
--
-- This function modifies:
--
-- * 'wakeLog'
-- * 'robotsWatching'
-- * 'waitingRobots'
-- * 'activeRobots'
wakeUpRobotsDoneSleeping :: (Has (State Robots) sig m) => TickNumber -> m ()
wakeUpRobotsDoneSleeping time = do
  robotIdSet <- IM.keysSet <$> use robotMap
  wakeableRIDsSet <- IS.fromList . MM.get time <$> use Internal.waitingRobots
  Internal.waitingRobots %= MM.nullify time

  -- Limit ourselves to the robots that have not expired in their sleep
  let newlyAlive = IS.intersection robotIdSet wakeableRIDsSet

  Internal.activeRobots %= IS.union newlyAlive

  -- These robots' wake times may have been moved "forward"
  -- by 'wakeWatchingRobots'.
  clearWatchingRobots wakeableRIDsSet

-- | Clear the "watch" state of all of the
-- awakened robots
clearWatchingRobots ::
  (Has (State Robots) sig m) =>
  IntSet ->
  m ()
clearWatchingRobots rids = do
  robotsWatchingForEntities %= MM.map (`IS.difference` rids)
  robotsWatchingForRobots %= MM.map (`IS.difference` rids)

-- | Wake the robots watching for entity change in the location.
wakeRobotsWatchingForEntities :: (Has (State Robots) sig m) => RID -> TickNumber -> Cosmic Location -> m ()
wakeRobotsWatchingForEntities myID currentTick loc = do
  m <- use robotsWatchingForEntities
  wakeWatchingRobotsInternal m myID currentTick loc

-- | Wake the robots watching for robots entering or leaving the location.
wakeRobotsWatchingForRobots :: (Has (State Robots) sig m) => RID -> TickNumber -> Cosmic Location -> m ()
wakeRobotsWatchingForRobots myID currentTick loc = do
  m <- use robotsWatchingForRobots
  wakeWatchingRobotsInternal m myID currentTick loc

-- | Wake the robots watching given location.
--
-- NOTE: Clearing 'TickNumber' map entries from 'waitingRobots'
-- upon wakeup is handled by 'wakeUpRobotsDoneSleeping'
wakeWatchingRobotsInternal ::
  (Has (State Robots) sig m) =>
  MonoidMap (Cosmic Location) IntSet ->
  RID ->
  TickNumber ->
  Cosmic Location ->
  m ()
wakeWatchingRobotsInternal watchingMap myID currentTick loc = do
  waitingMap <- use waitingRobots
  rMap <- use robotMap

  -- The bookkeeping updates to robot waiting
  -- states are prepared in 4 steps...

  let -- Step 1: Identify the robots that are watching this location.
      botsWatchingThisLoc :: [Robot]
      botsWatchingThisLoc =
        mapMaybe (`IM.lookup` rMap) $
          IS.toList $
            MM.get loc watchingMap

      -- Step 2: Get the target wake time for each of these robots
      wakeTimes :: [(RID, TickNumber)]
      wakeTimes = mapMaybe (sequenceA . (view robotID &&& waitingUntil)) botsWatchingThisLoc

      wakeTimesToPurge :: MonoidMap TickNumber (S.Set RID)
      wakeTimesToPurge = foldr (uncurry (MM.adjust . S.insert)) mempty wakeTimes

      -- Step 3: Take these robots out of their time-indexed slot in "waitingRobots".
      -- To preserve performance, this should be done without iterating over the
      -- entire "waitingRobots" map.
      filteredWaiting :: MonoidMap TickNumber [RID]
      filteredWaiting = MM.foldrWithKey f waitingMap wakeTimesToPurge
       where
        f k botsToRemove = MM.adjust (filter (`S.notMember` botsToRemove)) k

      -- Step 4: Re-add the watching bots to be awakened ASAP:
      wakeableBotIds = map fst wakeTimes

      -- It is crucial that only robots with a larger RID than the current robot
      -- be scheduled for the *same* tick, since within a given tick we iterate over
      -- robots in increasing order of RID.
      -- See note in 'iterateRobots'.
      (currTickWakeable, nextTickWakeable) = partition (> myID) wakeableBotIds
      wakeTimeGroups =
        [ (currentTick, currTickWakeable)
        , (addTicks 1 currentTick, nextTickWakeable)
        ]
      newInsertions = MM.fromList wakeTimeGroups

  -- Contract: This must be emptied immediately
  -- in 'iterateRobots'
  currentTickWakeableBots .= currTickWakeable

  -- NOTE: There are two "sources of truth" for the waiting state of robots:
  -- 1. In the GameState via "Internal.waitingRobots"
  -- 2. In each robot, via the CESK machine state

  -- 1. Update the game state
  Internal.waitingRobots .= filteredWaiting <> newInsertions

  -- 2. Update the machine of each robot
  forM_ wakeTimeGroups $ \(newWakeTime, wakeableBots) ->
    forM_ wakeableBots $ \rid ->
      robotMap . at rid . _Just . machine %= \case
        Waiting _ c -> Waiting newWakeTime c
        x -> x

deleteRobot :: (Has (State Robots) sig m) => RID -> m (Maybe (Cosmic Location))
deleteRobot rn = do
  Internal.activeRobots %= IS.delete rn
  mrobot <- robotMap . at rn <<.= Nothing
  mrobot `forM_` \robot -> do
    -- Delete the robot from the index of robots by location.
    removeRobotFromLocationMap (robot ^. robotLocation) rn
  pure (view robotLocation <$> mrobot)

-- | Makes sure empty sets don't hang around in the
-- 'robotsByLocation' map.  We don't want a key with an
-- empty set at every location any robot has ever
-- visited!
removeRobotFromLocationMap ::
  (Has (State Robots) sig m) =>
  Cosmic Location ->
  RID ->
  m ()
removeRobotFromLocationMap (Cosmic oldSubworld oldPlanar) rid =
  robotsByLocation
    %= MM.adjust (MM.adjust (IS.delete rid) oldPlanar) oldSubworld

setRobotInfo :: RID -> [Robot] -> Robots -> Robots
setRobotInfo rid robotList rState =
  setRobotList robotList rState
    & viewCenterState . VCInternal.viewRobotID .~ rid
    & viewCenterRule .~ VCRobot rid

setRobotList :: [Robot] -> Robots -> Robots
setRobotList robotList rState =
  rState
    & robotMap .~ IM.fromList (map (view robotID &&& id) robotList)
    & robotsByLocation .~ groupRobotsByLocation robotList
    & Internal.activeRobots .~ setOf (traverse . robotID) robotList
    & robotNaming . gensym .~ initGensym
 where
  initGensym = length robotList - 1

  groupRobotsByLocation = foldr f mempty
   where
    f r = MM.adjust (g r) (r ^. (robotLocation . subworld))
    g r = MM.adjust (IS.insert (r ^. robotID)) (r ^. (robotLocation . planar))

-- | Helper function to get location of robot.
getLocationFromRID :: Robots -> RID -> Maybe (Cosmic Location)
getLocationFromRID rs rid = rs ^? robotMap . ix rid . robotLocation

-- | Modify the 'viewCenter' by applying an arbitrary function to the
--   current value.  Note that this also modifies the 'viewCenterRule'
--   to match.  After calling this function the 'viewCenterRule' will
--   specify a particular location, not a robot.
modifyViewCenter :: (Cosmic Location -> Cosmic Location) -> Robots -> Robots
modifyViewCenter update rInfo =
  rInfo
    & viewCenterState %~ VCInternal.modifyViewCenter (getLocationFromRID rInfo) update

-- | "Unfocus" by modifying the view center rule to look at the
--   current location instead of a specific robot, and also set the
--   focused robot ID to an invalid value.  In classic mode this
--   causes the map view to become nothing but static.
unfocus :: Robots -> Robots
unfocus rInfo =
  rInfo
    & viewCenterState %~ VCInternal.unfocus (getLocationFromRID rInfo)

-- | Recalculate the 'viewCenter'  based on the current 'viewCenterRule'.
--
-- If the 'viewCenterRule' specifies a robot which does not exist,
-- simply leave the current 'viewCenter' as it is.
recalcViewCenter :: Robots -> Robots
recalcViewCenter rInfo =
  rInfo
    & viewCenterState %~ VCInternal.syncViewCenter (getLocationFromRID rInfo)
