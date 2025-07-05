{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Robot-specific subrecords and utilities used by 'Swarm.Game.State.GameState'
module Swarm.Game.State.Robot (
  -- * Types
  ViewCenterRule (..),
  Robots,

  -- * Robot naming
  RobotNaming,
  nameGenerator,
  gensym,
  robotNaming,

  -- * Initialization
  initRobots,
  setRobotInfo,

  -- * Accessors
  robotMap,
  robotsByLocation,
  robotsWatching,
  activeRobots,
  waitingRobots,
  currentTickWakeableBots,
  viewCenterRule,
  viewCenter,
  focusedRobotID,
  focusedRobot,

  -- * Utilities
  wakeWatchingRobots,
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
) where

import Control.Arrow (Arrow ((&&&)))
import Control.Effect.Lens
import Control.Effect.State (State)
import Control.Effect.Throw (Has)
import Control.Lens hiding (Const, use, uses, view, (%=), (+=), (.=), (<+=), (<<.=))
import Control.Monad (forM_, void)
import Data.IntMap (IntMap)
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
import Swarm.Game.State.Config
import Swarm.Game.State.ViewCenter.Internal (ViewCenter, ViewCenterRule (..), defaultViewCenter)
import Swarm.Game.State.ViewCenter.Internal qualified as VCInternal
import Swarm.Game.Tick
import Swarm.Game.Universe as U
import Swarm.ResourceLoading (NameGenerator)
import Swarm.Util ((<+=), (<<.=))
import Swarm.Util.Lens (makeLensesExcluding)

data RobotNaming = RobotNaming
  { _nameGenerator :: NameGenerator
  , _gensym :: Int
  }

makeLensesExcluding ['_nameGenerator] ''RobotNaming

--- | Read-only list of words, for use in building random robot names.
nameGenerator :: Getter RobotNaming NameGenerator
nameGenerator = to _nameGenerator

-- | A counter used to generate globally unique IDs.
gensym :: Lens' RobotNaming Int

data Robots = Robots
  { _robotMap :: IntMap Robot
  , -- A set of robots to consider for the next game tick. It is guaranteed to
    -- be a subset of the keys of 'robotMap'. It may contain waiting or idle
    -- robots. But robots that are present in 'robotMap' and not in 'activeRobots'
    -- are guaranteed to be either waiting or idle.
    _activeRobots :: IntSet
  , -- A set of probably waiting robots, indexed by probable wake-up time. It
    -- may contain robots that are in fact active or idle, as well as robots
    -- that do not exist anymore. Its only guarantee is that once a robot name
    -- with its wake up time is inserted in it, it will remain there until the
    -- wake-up time is reached, at which point it is removed via
    -- 'wakeUpRobotsDoneSleeping'.
    -- Waiting robots for a given time are a list because it is cheaper to
    -- prepend to a list than insert into a 'Set'.
    _waitingRobots :: MonoidMap TickNumber [RID]
  , _currentTickWakeableBots :: [RID]
  , _robotsByLocation :: MonoidMap SubworldName (MonoidMap Location IntSet)
  , -- This member exists as an optimization so
    -- that we do not have to iterate over all "waiting" robots,
    -- since there may be many.
    _robotsWatching :: MonoidMap (Cosmic Location) IntSet
  , _robotNaming :: RobotNaming
  , _viewCenterState :: ViewCenter
  }

-- We want to access active and waiting robots via lenses inside
-- this module but to expose it as a Getter to protect invariants.
makeLensesFor
  [ ("_activeRobots", "internalActiveRobots")
  , ("_waitingRobots", "internalWaitingRobots")
  ]
  ''Robots

makeLensesExcluding ['_activeRobots, '_waitingRobots] ''Robots

-- | All the robots that currently exist in the game, indexed by ID.
robotMap :: Lens' Robots (IntMap Robot)

-- | The names of the robots that are currently not sleeping.
activeRobots :: Getter Robots IntSet
activeRobots = internalActiveRobots

-- | The names of the robots that are currently sleeping, indexed by wake up
--   time. Note that this may not include all sleeping robots, particularly
--   those that are only taking a short nap (e.g. @wait 1@).
waitingRobots :: Getter Robots (MonoidMap TickNumber [RID])
waitingRobots = internalWaitingRobots

-- | Get a list of all the robots that are \"watching\" by location.
currentTickWakeableBots :: Lens' Robots [RID]

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
robotsWatching :: Lens' Robots (MonoidMap (Cosmic Location) IntSet)

-- | State and data for assigning identifiers to robots
robotNaming :: Lens' Robots RobotNaming

-- | The current view center location, focused robot and the rule for which to follow.
viewCenterState :: Lens' Robots ViewCenter

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

-- * Utilities

initRobots :: GameStateConfig -> Robots
initRobots gsc =
  Robots
    { _robotMap = IM.empty
    , _activeRobots = IS.empty
    , _waitingRobots = mempty
    , _currentTickWakeableBots = mempty
    , _robotsByLocation = mempty
    , _robotsWatching = mempty
    , _robotNaming =
        RobotNaming
          { _nameGenerator = nameParts gsc
          , _gensym = 0
          }
    , _viewCenterState = defaultViewCenter
    }

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
  internalActiveRobots %= IS.insert rid
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
  internalActiveRobots %= IS.delete rid
  internalWaitingRobots %= MM.adjust (rid :) time

-- | Takes a robot out of the 'activeRobots' set.
sleepForever :: (Has (State Robots) sig m) => RID -> m ()
sleepForever rid = internalActiveRobots %= IS.delete rid

-- | Adds a robot to the 'activeRobots' set.
activateRobot :: (Has (State Robots) sig m) => RID -> m ()
activateRobot rid = internalActiveRobots %= IS.insert rid

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
-- * 'internalWaitingRobots'
-- * 'internalActiveRobots' (aka 'activeRobots')
wakeUpRobotsDoneSleeping :: (Has (State Robots) sig m) => TickNumber -> m ()
wakeUpRobotsDoneSleeping time = do
  robotIdSet <- IM.keysSet <$> use robotMap
  wakeableRIDsSet <- IS.fromList . MM.get time <$> use internalWaitingRobots
  internalWaitingRobots %= MM.nullify time

  -- Limit ourselves to the robots that have not expired in their sleep
  let newlyAlive = IS.intersection robotIdSet wakeableRIDsSet

  internalActiveRobots %= IS.union newlyAlive

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
  robotsWatching %= MM.map (`IS.difference` rids)

-- | Iterates through all of the currently @wait@-ing robots,
-- and moves forward the wake time of the ones that are @watch@-ing this location.
--
-- NOTE: Clearing 'TickNumber' map entries from 'internalWaitingRobots'
-- upon wakeup is handled by 'wakeUpRobotsDoneSleeping'
wakeWatchingRobots :: (Has (State Robots) sig m) => RID -> TickNumber -> Cosmic Location -> m ()
wakeWatchingRobots myID currentTick loc = do
  waitingMap <- use waitingRobots
  rMap <- use robotMap
  watchingMap <- use robotsWatching

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
  -- 1. In the GameState via "internalWaitingRobots"
  -- 2. In each robot, via the CESK machine state

  -- 1. Update the game state
  internalWaitingRobots .= filteredWaiting <> newInsertions

  -- 2. Update the machine of each robot
  forM_ wakeTimeGroups $ \(newWakeTime, wakeableBots) ->
    forM_ wakeableBots $ \rid ->
      robotMap . at rid . _Just . machine %= \case
        Waiting _ c -> Waiting newWakeTime c
        x -> x

deleteRobot :: (Has (State Robots) sig m) => RID -> m ()
deleteRobot rn = do
  internalActiveRobots %= IS.delete rn
  mrobot <- robotMap . at rn <<.= Nothing
  mrobot `forM_` \robot -> do
    -- Delete the robot from the index of robots by location.
    removeRobotFromLocationMap (robot ^. robotLocation) rn

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
    & internalActiveRobots .~ setOf (traverse . robotID) robotList
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
