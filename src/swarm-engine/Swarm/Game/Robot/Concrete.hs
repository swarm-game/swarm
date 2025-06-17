{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Support for instantiated robots.
module Swarm.Game.Robot.Concrete (
  -- * Lenses
  machine,
  activityCounts,
  robotLog,
  robotLogUpdated,

  -- * Query
  waitingUntil,
  getResult,
  isActive,
  wantsToStep,

  -- * Utilities
  instantiateRobot,
) where

import Control.Lens hiding (Const, contains)
import Data.Aeson qualified as Ae (Key, KeyValue, ToJSON (..), object, (.=))
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Linear
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD
import Swarm.Game.CESK qualified as C
import Swarm.Game.Cosmetic.Display (defaultRobotDisplay, invisible)
import Swarm.Game.Entity hiding (empty)
import Swarm.Game.Robot
import Swarm.Game.Robot.Activity
import Swarm.Game.Robot.Walk (emptyExceptions)
import Swarm.Game.Tick
import Swarm.Game.Universe
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Syntax (TSyntax)
import Swarm.Language.Value as V
import Swarm.Log

type instance RobotMachine 'ConcreteRobot = C.CESK
type instance RobotActivity 'ConcreteRobot = ActivityCounts
type instance RobotLogMember 'ConcreteRobot = Seq LogEntry
type instance RobotLogUpdatedMember 'ConcreteRobot = Bool

machine :: Lens' Robot C.CESK
machine = lens _machine (\r x -> r {_machine = x})

-- | Diagnostic and operational tracking of CESK steps or other activity
activityCounts :: Lens' Robot ActivityCounts
activityCounts = lens _activityCounts (\r x -> r {_activityCounts = x})

-- | The robot's own private message log, most recent message last.
--   Messages can be added both by explicit use of the 'Swarm.Language.Syntax.Log' command,
--   and by uncaught exceptions.  Stored as a 'Seq' so that
--   we can efficiently add to the end and also process from beginning
--   to end.  Note that updating via this lens will also set the
--   'robotLogUpdated'.
robotLog :: Lens' Robot (Seq LogEntry)
robotLog = lens _robotLog setLog
 where
  setLog r newLog =
    r
      { _robotLog = newLog
      , -- Flag the log as updated if (1) if already was, or (2) the new
        -- log is a different length than the old.  (This would not
        -- catch updates that merely modify an entry, but we don't want
        -- to have to compare the entire logs, and we only ever append
        -- to logs anyway.)
        _robotLogUpdated =
          _robotLogUpdated r || Seq.length (_robotLog r) /= Seq.length newLog
      }

-- | Has the 'robotLog' been updated since the last time it was
--   viewed?
robotLogUpdated :: Lens' Robot Bool
robotLogUpdated = lens _robotLogUpdated (\r x -> r {_robotLogUpdated = x})

instance ToSample Robot where
  toSamples _ = SD.singleSample sampleBase
   where
    sampleBase :: Robot
    sampleBase =
      instantiateRobot (Just $ C.initMachine [tmQ| move |]) 0 $
        mkRobot
          Nothing
          "base"
          "The starting robot."
          Nothing
          zero
          defaultRobotDisplay
          Nothing
          []
          []
          False
          False
          emptyExceptions
          0

mkMachine :: Maybe TSyntax -> C.CESK
mkMachine Nothing = C.Out VUnit C.emptyStore []
mkMachine (Just t) = C.initMachine t

-- | Instantiate a robot template to make it into a concrete robot, by
--    providing a robot ID. Concrete robots also require a location;
--    if the robot template didn't have a location already, just set
--    the location to (0,0) by default.  If you want a different location,
--    set it via 'trobotLocation' before calling 'instantiateRobot'.
--
-- If a machine is not supplied (i.e. 'Nothing'), will fallback to any
-- program specified in the template robot.
instantiateRobot :: Maybe C.CESK -> RID -> TRobot -> Robot
instantiateRobot maybeMachine i r =
  r
    { _robotID = i
    , _robotLocation = fromMaybe defaultCosmicLocation $ _robotLocation r
    , _activityCounts = emptyActivityCount
    , _machine = fromMaybe (mkMachine $ _machine r) maybeMachine
    , _robotLog = Seq.empty
    , _robotLogUpdated = False
    }

(.=?) :: (Ae.KeyValue e a, Ae.ToJSON v, Eq v) => Ae.Key -> v -> v -> Maybe a
(.=?) n v defaultVal = if defaultVal /= v then Just $ n Ae..= v else Nothing

(.==) :: (Ae.KeyValue e a, Ae.ToJSON v) => Ae.Key -> v -> Maybe a
(.==) n v = Just $ n Ae..= v

instance Ae.ToJSON Robot where
  toJSON r =
    Ae.object $
      catMaybes
        [ "id" .== (r ^. robotID)
        , "name" .== (r ^. robotEntity . entityName)
        , "description" .=? (r ^. robotEntity . entityDescription) $ mempty
        , "loc" .== (r ^. robotLocation)
        , "dir" .=? (r ^. robotEntity . entityOrientation) $ zero
        , "display" .=? (r ^. robotDisplay) $ (defaultRobotDisplay & invisible .~ sys)
        , "program" .== (r ^. machine)
        , "devices" .=? (map (^. _2 . entityName) . elems $ r ^. equippedDevices) $ []
        , "inventory" .=? (map (_2 %~ view entityName) . elems $ r ^. robotInventory) $ []
        , "system" .=? sys $ False
        , "heavy" .=? (r ^. robotHeavy) $ False
        , "log" .=? (r ^. robotLog) $ mempty
        , -- debug
          "capabilities" .=? (r ^. robotCapabilities) $ mempty
        , "logUpdated" .=? (r ^. robotLogUpdated) $ False
        , "parent" .=? (r ^. robotParentID) $ Nothing
        , "createdAt" .=? (r ^. robotCreatedAt) $ 0
        , "selfDestruct" .=? (r ^. selfDestruct) $ False
        , "activity" .=? (r ^. activityCounts) $ emptyActivityCount
        , "runningAtomic" .=? (r ^. runningAtomic) $ False
        ]
   where
    sys = r ^. systemRobot

-- | The time until which the robot is waiting, if any.
waitingUntil :: Robot -> Maybe TickNumber
waitingUntil robot =
  case _machine robot of
    C.Waiting time _ -> Just time
    _ -> Nothing

-- | Get the result of the robot's computation if it is finished.
getResult :: Robot -> Maybe Value
{-# INLINE getResult #-}
getResult = C.finalValue . view machine

-- | Is the robot actively in the middle of a computation?
isActive :: Robot -> Bool
{-# INLINE isActive #-}
isActive = isNothing . getResult

-- | "Active" robots include robots that are waiting; 'wantsToStep' is
--   true if the robot actually wants to take another step right now
--   (this is a /subset/ of active robots).
wantsToStep :: TickNumber -> Robot -> Bool
wantsToStep now robot
  | not (isActive robot) = False
  | otherwise = maybe True (now >=) (waitingUntil robot)
