{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Swarm.Game.Robot
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A data type to represent robots.
module Swarm.Game.Robot (
  -- * Robots data

  -- * Robot log entries
  LogEntry (..),
  leText,
  leRobotName,
  leTime,

  -- * Robots
  RID,
  Robot,

  -- * Robot context
  RobotContext,
  defTypes,
  defCaps,
  defVals,
  defStore,

  -- ** Lenses
  robotEntity,
  robotName,
  robotDisplay,
  robotLocation,
  robotOrientation,
  robotInventory,
  installedDevices,
  robotLog,
  robotLogUpdated,
  inventoryHash,
  robotCapabilities,
  robotContext,
  robotID,
  robotParentID,
  machine,
  systemRobot,
  selfDestruct,
  tickSteps,

  -- ** Create
  mkRobot,
  baseRobot,
  unsafeSetRobotID,

  -- ** Query
  robotKnows,
  isActive,
  waitingUntil,
  getResult,
) where

import Control.Lens hiding (contains)
import Data.Hashable (hashWithSalt)
import Data.Int (Int64)
import Data.Maybe (isNothing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import Data.Set.Lens (setOf)
import Data.Text (Text)
import Linear
import Witch (into)

import Data.Yaml ((.!=), (.:), (.:?))
import Swarm.Util.Yaml

import Swarm.Game.CESK
import Swarm.Game.Display
import Swarm.Game.Entity hiding (empty)
import Swarm.Game.Value as V
import Swarm.Language.Capability
import Swarm.Language.Context
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Syntax (Term (TString), east)
import Swarm.Language.Types (TCtx)

-- | A record that stores the information
--   for all defintions stored in a 'Robot'
data RobotContext = RobotContext
  { -- | Map definition names to their types.
    _defTypes :: TCtx
  , -- | Map defintion names to the capabilities
    --   required to evaluate/execute them.
    _defCaps :: CapCtx
  , -- | Map defintion names to their values. Note that since
    --   definitions are delayed, the values will just consist of
    --   'VRef's pointing into the store.
    _defVals :: Env
  , -- | A store containing memory cells allocated to hold
    --   definitions.
    _defStore :: Store
  }
  deriving (Show)

makeLenses ''RobotContext

-- | An entry in a robot's log.
data LogEntry = LogEntry
  { -- | The text of the log entry.
    _leText :: Text
  , -- | The name of the robot that generated the entry.
    _leRobotName :: Text
  , -- | The time at which the entry was created.
    _leTime :: Integer
  }
  deriving (Show)

makeLenses ''LogEntry

-- | A unique identifier for a robot.
type RID = Int

-- | A value of type 'Robot' is a record representing the state of a
--   single robot.
data Robot = Robot
  { _robotEntity :: Entity
  , _installedDevices :: Inventory
  , -- | A cached view of the capabilities this robot has.
    --   Automatically generated from '_installedDevices'.
    _robotCapabilities :: Set Capability
  , _robotLog :: Seq LogEntry
  , _robotLogUpdated :: Bool
  , _robotLocation :: V2 Int64
  , _robotContext :: RobotContext
  , _robotID :: RID
  , _robotParentID :: Maybe RID
  , _machine :: CESK
  , _systemRobot :: Bool
  , _selfDestruct :: Bool
  , _tickSteps :: Int
  }
  deriving (Show)

-- See https://byorgey.wordpress.com/2021/09/17/automatically-updated-cached-views-with-lens/
-- for the approach used here with lenses.

let exclude = ['_robotCapabilities, '_installedDevices, '_robotLog, '_robotID]
 in makeLensesWith
      ( lensRules
          & generateSignatures .~ False
          & lensField . mapped . mapped %~ \fn n ->
            if n `elem` exclude then [] else fn n
      )
      ''Robot

-- | Robots are not entities, but they have almost all the
--   characteristics of one (or perhaps we could think of robots as
--   very special sorts of entities), so for convenience each robot
--   carries an 'Entity' record to store all the information it has in
--   common with any 'Entity'.
--
--   Note there are various lenses provided for convenience that
--   directly reference fields inside this record; for example, one
--   can use 'robotName' instead of writing @'robotEntity'
--   . 'entityName'@.
robotEntity :: Lens' Robot Entity

-- | The name of a robot.  Note that unlike entities, robot names are
--   expected to be globally unique
robotName :: Lens' Robot Text
robotName = robotEntity . entityName

-- | The 'Display' of a robot.
robotDisplay :: Lens' Robot Display
robotDisplay = robotEntity . entityDisplay

-- | The robot's current location, represented as (x,y).
robotLocation :: Lens' Robot (V2 Int64)

-- | Which way the robot is currently facing.
robotOrientation :: Lens' Robot (Maybe (V2 Int64))
robotOrientation = robotEntity . entityOrientation

-- | The robot's inventory.
robotInventory :: Lens' Robot Inventory
robotInventory = robotEntity . entityInventory

-- | The robot's context
robotContext :: Lens' Robot RobotContext

-- | The (unique) ID number of the robot.  This is only a Getter since
--   the robot ID is immutable.
robotID :: Getter Robot RID
robotID = to _robotID

-- | Set the ID number of a robot.  This is "unsafe" since robots
--   should be uniquely identified by their ID, and are stored using
--   the ID as a key, etc.  The only place this is ever needed is when
--   reading robots from a `.yaml` file (*e.g.* in a challenge
--   description), we cannot fill in a unique ID at parse time since
--   we don't have access to a `State Game` effect; when later adding
--   such robots to the world we generate and fill in a unique ID.
--   Otherwise, all robots are created via 'mkRobot', which requires
--   an ID number up front.
unsafeSetRobotID :: RID -> Robot -> Robot
unsafeSetRobotID i r = r {_robotID = i}

-- | The ID number of the robot's parent, that is, the robot that
--   built (or most recently reprogrammed) this robot, if there is
--   one.
robotParentID :: Lens' Robot (Maybe RID)

-- | A separate inventory for "installed devices", which provide the
--   robot with certain capabilities.
--
--   Note that every time the inventory of installed devices is
--   modified, this lens recomputes a cached set of the capabilities
--   the installed devices provide, to speed up subsequent lookups to
--   see whether the robot has a certain capability (see 'robotCapabilities')
installedDevices :: Lens' Robot Inventory
installedDevices = lens _installedDevices setInstalled
 where
  setInstalled r inst =
    r
      { _installedDevices = inst
      , _robotCapabilities = inventoryCapabilities inst
      }

-- | The robot's own private message log, most recent message last.
--   Messages can be added both by explicit use of the 'Log' command,
--   and by uncaught exceptions.  Stored as a "Data.Sequence" so that
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

-- | A hash of a robot's entity record and installed devices, to
--   facilitate quickly deciding whether we need to redraw the robot
--   info panel.
inventoryHash :: Getter Robot Int
inventoryHash = to (\r -> 17 `hashWithSalt` (r ^. (robotEntity . entityHash)) `hashWithSalt` (r ^. installedDevices))

-- | Recompute the set of capabilities provided by the inventory of
--   installed devices.
inventoryCapabilities :: Inventory -> Set Capability
inventoryCapabilities = setOf (to elems . traverse . _2 . entityCapabilities . traverse)

-- | Does the robot know of the entity's existence.
robotKnows :: Robot -> Entity -> Bool
robotKnows r e = contains0plus e (r ^. robotInventory) || contains0plus e (r ^. installedDevices)

-- | Get the set of capabilities this robot possesses.  This is only a
--   getter, not a lens, because it is automatically generated from
--   the 'installedDevices'.  The only way to change a robot's
--   capabilities is to modify its 'installedDevices'.
robotCapabilities :: Getter Robot (Set Capability)
robotCapabilities = to _robotCapabilities

-- | The robot's current CEK machine state.
machine :: Lens' Robot CESK

-- | Is this robot a "system robot"?  System robots are generated by
--   the system (as opposed to created by the user) and are not
--   subject to the usual capability restrictions.
systemRobot :: Lens' Robot Bool

-- | Does this robot wish to self destruct?
selfDestruct :: Lens' Robot Bool

-- | The need for 'tickSteps' is a bit technical, and I hope I can
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
--   currently does by setting tickSteps to zero.  However, that
--   has the disadvantage that when tickSteps becomes zero, we
--   can't tell whether that happened because the robot ran out of
--   steps, or because it executed a command and set it to zero
--   manually.
--
--   Perhaps instead, each robot should keep a counter saying how
--   many commands it has executed.  The loop stepping the robot
--   can tell when the counter increments.
tickSteps :: Lens' Robot Int

-- | A general function for creating robots.
mkRobot ::
  -- | ID number of the robot.
  Int ->
  -- | ID number of the robot's parent, if it has one.
  Maybe Int ->
  -- | Name of the robot.
  Text ->
  -- | Description of the robot.
  [Text] ->
  -- | Initial location.
  V2 Int64 ->
  -- | Initial heading/direction.
  V2 Int64 ->
  -- | Robot display.
  Display ->
  -- | Initial CESK machine.
  CESK ->
  -- | Installed devices.
  [Entity] ->
  -- | Initial inventory.
  [(Count, Entity)] ->
  -- | Should this be a system robot?
  Bool ->
  Robot
mkRobot rid pid name descr loc dir disp m devs inv sys =
  Robot
    { _robotEntity =
        mkEntity disp name descr []
          & entityOrientation ?~ dir
          & entityInventory .~ fromElems inv
    , _installedDevices = inst
    , _robotCapabilities = inventoryCapabilities inst
    , _robotLog = Seq.empty
    , _robotLogUpdated = False
    , _robotLocation = loc
    , _robotContext = RobotContext empty empty empty emptyStore
    , _robotID = rid
    , _robotParentID = pid
    , _machine = m
    , _systemRobot = sys
    , _selfDestruct = False
    , _tickSteps = 0
    }
 where
  inst = fromList devs

-- | The initial robot representing your "base".
baseRobot :: [Entity] -> Maybe FilePath -> Robot
baseRobot devs toRun =
  Robot
    { _robotEntity =
        mkEntity
          defaultRobotDisplay
          "base"
          ["Your base of operations."]
          []
          & entityOrientation ?~ east
          & entityDisplay . orientationMap .~ Empty
    , _installedDevices = inst
    , _robotCapabilities = inventoryCapabilities inst
    , _robotLog = Seq.empty
    , _robotLogUpdated = False
    , _robotLocation = V2 0 0
    , _robotContext = RobotContext empty empty empty emptyStore
    , _robotID = 0
    , _robotParentID = Nothing
    , _machine = case toRun of
        Nothing -> idleMachine
        Just (into @Text -> f) -> initMachine [tmQ| run($str:f) |] empty emptyStore
    , _systemRobot = False
    , _selfDestruct = False
    , _tickSteps = 0
    }
 where
  inst = fromList devs

-- | We can parse a robot from a YAML file if we have access to an
--   'EntityMap' in which we can look up the names of entities.
instance FromJSONE EntityMap Robot where
  parseJSONE = withObjectE "robot" $ \v ->
    -- Note we can't generate a unique ID here since we don't have
    -- access to a 'State GameState' effect; a unique ID will be
    -- filled in later when adding the robot to the world.
    mkRobot (-1) Nothing
      <$> liftE (v .: "name")
      <*> liftE (v .:? "description" .!= [])
      <*> liftE (v .: "loc")
      <*> liftE (v .: "dir")
      <*> liftE (v .:? "display" .!= defaultRobotDisplay)
      <*> liftE (mkMachine <$> (v .:? "program"))
      <*> v ..:? "devices" ..!= []
      <*> v ..:? "inventory" ..!= []
      <*> liftE (v .:? "system" .!= False)
   where
    mkMachine Nothing = idleMachine
    mkMachine (Just pt) = initMachine pt mempty emptyStore

-- | Is the robot actively in the middle of a computation?
isActive :: Robot -> Bool
{-# INLINE isActive #-}
isActive = isNothing . getResult

-- | The time until which the robot is waiting, if any.
waitingUntil :: Robot -> Maybe Integer
waitingUntil robot =
  case _machine robot of
    Waiting time _ -> Just time
    _ -> Nothing

-- | Get the result of the robot's computation if it is finished.
getResult :: Robot -> Maybe (Value, Store)
{-# INLINE getResult #-}
getResult = finalValue . view machine
