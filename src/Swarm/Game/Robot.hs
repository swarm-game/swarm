{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
  module Swarm.Game.Log,

  -- * Robots
  RobotPhase (..),
  RID,
  RobotR,
  Robot,
  TRobot,

  -- ** Runtime robot update
  RobotUpdate (..),

  -- * Robot context
  RobotContext,
  defTypes,
  defReqs,
  defVals,
  defStore,
  emptyRobotContext,

  -- ** Lenses
  robotEntity,
  robotName,
  trobotName,
  robotCreatedAt,
  robotDisplay,
  robotLocation,
  unsafeSetRobotLocation,
  trobotLocation,
  robotOrientation,
  robotInventory,
  equippedDevices,
  robotLog,
  robotLogUpdated,
  inventoryHash,
  robotCapabilities,
  robotContext,
  trobotContext,
  robotID,
  robotParentID,
  robotHeavy,
  machine,
  systemRobot,
  selfDestruct,
  tickSteps,
  runningAtomic,

  -- ** Creation & instantiation
  mkRobot,
  instantiateRobot,

  -- ** Query
  robotKnows,
  isActive,
  waitingUntil,
  getResult,

  -- ** Constants
  hearingDistance,
) where

import Control.Lens hiding (contains)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (hashWithSalt)
import Data.Maybe (fromMaybe, isNothing)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Text (Text)
import Data.Yaml ((.!=), (.:), (.:?))
import GHC.Generics (Generic)
import Linear
import Swarm.Game.CESK
import Swarm.Game.Display (Display, curOrientation, defaultRobotDisplay, invisible)
import Swarm.Game.Entity hiding (empty)
import Swarm.Game.Log
import Swarm.Language.Capability (Capability)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Requirement (ReqCtx)
import Swarm.Language.Syntax (toDirection)
import Swarm.Language.Typed (Typed (..))
import Swarm.Language.Types (TCtx)
import Swarm.Language.Value as V
import Swarm.Util.Location
import Swarm.Util.Yaml
import System.Clock (TimeSpec)

-- | A record that stores the information
--   for all definitions stored in a 'Robot'
data RobotContext = RobotContext
  { _defTypes :: TCtx
  -- ^ Map definition names to their types.
  , _defReqs :: ReqCtx
  -- ^ Map definition names to the capabilities
  --   required to evaluate/execute them.
  , _defVals :: Env
  -- ^ Map definition names to their values. Note that since
  --   definitions are delayed, the values will just consist of
  --   'VRef's pointing into the store.
  , _defStore :: Store
  -- ^ A store containing memory cells allocated to hold
  --   definitions.
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''RobotContext

emptyRobotContext :: RobotContext
emptyRobotContext = RobotContext Ctx.empty Ctx.empty Ctx.empty emptyStore

type instance Index RobotContext = Ctx.Var
type instance IxValue RobotContext = Typed Value

instance Ixed RobotContext
instance At RobotContext where
  at name = lens getter setter
   where
    getter ctx =
      do
        typ <- Ctx.lookup name (ctx ^. defTypes)
        val <- Ctx.lookup name (ctx ^. defVals)
        req <- Ctx.lookup name (ctx ^. defReqs)
        return $ Typed val typ req
    setter ctx Nothing =
      ctx
        & defTypes %~ Ctx.delete name
        & defVals %~ Ctx.delete name
        & defReqs %~ Ctx.delete name
    setter ctx (Just (Typed val typ req)) =
      ctx
        & defTypes %~ Ctx.addBinding name typ
        & defVals %~ Ctx.addBinding name val
        & defReqs %~ Ctx.addBinding name req

-- | A unique identifier for a robot.
type RID = Int

-- | The phase of a robot description record.
data RobotPhase
  = -- | The robot record has just been read in from a scenario
    --   description; it represents a /template/ that may later be
    --   instantiated as one or more concrete robots.
    TemplateRobot
  | -- | The robot record represents a concrete robot in the world.
    ConcreteRobot

-- | With a robot template, we may or may not have a location.  With a
--   concrete robot we must have a location.
type family RobotLocation (phase :: RobotPhase) :: * where
  RobotLocation 'TemplateRobot = Maybe Location
  RobotLocation 'ConcreteRobot = Location

-- | Robot templates have no ID; concrete robots definitely do.
type family RobotID (phase :: RobotPhase) :: * where
  RobotID 'TemplateRobot = ()
  RobotID 'ConcreteRobot = RID

-- | A value of type 'RobotR' is a record representing the state of a
--   single robot.  The @f@ parameter is for tracking whether or not
--   the robot has been assigned a unique ID.
data RobotR (phase :: RobotPhase) = RobotR
  { _robotEntity :: Entity
  , _equippedDevices :: Inventory
  , _robotCapabilities :: Set Capability
  -- ^ A cached view of the capabilities this robot has.
  --   Automatically generated from '_equippedDevices'.
  , _robotLog :: Seq LogEntry
  , _robotLogUpdated :: Bool
  , _robotLocation :: RobotLocation phase
  , _robotContext :: RobotContext
  , _robotID :: RobotID phase
  , _robotParentID :: Maybe RID
  , _robotHeavy :: Bool
  , _machine :: CESK
  , _systemRobot :: Bool
  , _selfDestruct :: Bool
  , _tickSteps :: Int
  , _runningAtomic :: Bool
  , _robotCreatedAt :: TimeSpec
  }
  deriving (Generic)

deriving instance (Show (RobotLocation phase), Show (RobotID phase)) => Show (RobotR phase)
deriving instance (Eq (RobotLocation phase), Eq (RobotID phase)) => Eq (RobotR phase)

deriving instance (ToJSON (RobotLocation phase), ToJSON (RobotID phase)) => ToJSON (RobotR phase)

-- See https://byorgey.wordpress.com/2021/09/17/automatically-updated-cached-views-with-lens/
-- for the approach used here with lenses.

let exclude = ['_robotCapabilities, '_equippedDevices, '_robotLog]
 in makeLensesWith
      ( lensRules
          & generateSignatures .~ False
          & lensField . mapped . mapped %~ \fn n ->
            if n `elem` exclude then [] else fn n
      )
      ''RobotR

-- | A template robot, i.e. a template robot record without a unique ID number,
--   and possibly without a location.
type TRobot = RobotR 'TemplateRobot

-- | A concrete robot, with a unique ID number and a specific location.
type Robot = RobotR 'ConcreteRobot

-- In theory we could make all these lenses over (RobotR phase), but
-- that leads to lots of type ambiguity problems later.  In practice
-- we only need lenses for Robots.

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
robotEntity :: Lens' (RobotR phase) Entity

-- | The creation date of the robot.
robotCreatedAt :: Lens' Robot TimeSpec

-- robotName and trobotName could be generalized to robotName' ::
-- Lens' (RobotR phase) Text.  However, type inference does not work
-- very well with the polymorphic version, so we export both
-- monomorphic versions instead.

-- | The name of a robot.
robotName :: Lens' Robot Text
robotName = robotEntity . entityName

-- | The name of a robot template.
trobotName :: Lens' TRobot Text
trobotName = robotEntity . entityName

-- | The 'Display' of a robot.  This is a special lens that
--   automatically sets the 'curOrientation' to the orientation of the
--   robot every time you do a @get@ operation.  Technically this does
--   not satisfy the lens laws---in particular, the get/put law does
--   not hold.  But we should think of the 'curOrientation' as being
--   simply a cache of the displayed entity's direction.
robotDisplay :: Lens' Robot Display
robotDisplay = lens getDisplay setDisplay
 where
  getDisplay r =
    (r ^. robotEntity . entityDisplay)
      & curOrientation .~ ((r ^. robotOrientation) >>= toDirection)
  setDisplay r d = r & robotEntity . entityDisplay .~ d

-- | The robot's current location, represented as (x,y).  This is only
--   a getter, since when changing a robot's location we must remember
--   to update the 'robotsByLocation' map as well.  You can use the
--   'updateRobotLocation' function for this purpose.
robotLocation :: Getter Robot Location

-- | Set a robot's location.  This is unsafe and should never be
--   called directly except by the 'updateRobotLocation' function.
--   The reason is that we need to make sure the 'robotsByLocation'
--   map stays in sync.
unsafeSetRobotLocation :: Location -> Robot -> Robot
unsafeSetRobotLocation loc r = r {_robotLocation = loc}

-- | A template robot's location.  Unlike 'robotLocation', this is a
--   lens, since when dealing with robot templates there is as yet no
--   'robotsByLocation' map to keep up-to-date.
trobotLocation :: Lens' TRobot (Maybe Location)
trobotLocation = lens _robotLocation (\r l -> r {_robotLocation = l})

-- | Which way the robot is currently facing.
robotOrientation :: Lens' Robot (Maybe Heading)
robotOrientation = robotEntity . entityOrientation

-- | The robot's inventory.
robotInventory :: Lens' Robot Inventory
robotInventory = robotEntity . entityInventory

-- | The robot's context.
robotContext :: Lens' Robot RobotContext

-- | The robot's context.
trobotContext :: Lens' TRobot RobotContext
trobotContext = lens _robotContext (\r c -> r {_robotContext = c})

-- | The (unique) ID number of the robot.  This is only a Getter since
--   the robot ID is immutable.
robotID :: Getter Robot RID

-- | Instantiate a robot template to make it into a concrete robot, by
--    providing a robot ID. Concrete robots also require a location;
--    if the robot template didn't have a location already, just set
--    the location to (0,0) by default.  If you want a different location,
--    set it via 'trobotLocation' before calling 'instantiateRobot'.
instantiateRobot :: RID -> TRobot -> Robot
instantiateRobot i r =
  r
    { _robotID = i
    , _robotLocation = fromMaybe zero (_robotLocation r)
    }

-- | The ID number of the robot's parent, that is, the robot that
--   built (or most recently reprogrammed) this robot, if there is
--   one.
robotParentID :: Lens' Robot (Maybe RID)

-- | Is this robot extra heavy (thus requiring tank treads to move)?
robotHeavy :: Lens' Robot Bool

-- | A separate inventory for equipped devices, which provide the
--   robot with certain capabilities.
--
--   Note that every time the inventory of equipped devices is
--   modified, this lens recomputes a cached set of the capabilities
--   the equipped devices provide, to speed up subsequent lookups to
--   see whether the robot has a certain capability (see 'robotCapabilities')
equippedDevices :: Lens' Robot Inventory
equippedDevices = lens _equippedDevices setEquipped
 where
  setEquipped r inst =
    r
      { _equippedDevices = inst
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

-- | A hash of a robot's entity record and equipped devices, to
--   facilitate quickly deciding whether we need to redraw the robot
--   info panel.
inventoryHash :: Getter Robot Int
inventoryHash = to (\r -> 17 `hashWithSalt` (r ^. (robotEntity . entityHash)) `hashWithSalt` (r ^. equippedDevices))

-- | Does a robot know of an entity's existence?
robotKnows :: Robot -> Entity -> Bool
robotKnows r e = contains0plus e (r ^. robotInventory) || contains0plus e (r ^. equippedDevices)

-- | Get the set of capabilities this robot possesses.  This is only a
--   getter, not a lens, because it is automatically generated from
--   the 'equippedDevices'.  The only way to change a robot's
--   capabilities is to modify its 'equippedDevices'.
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

-- | Is the robot currently running an atomic block?
runningAtomic :: Lens' Robot Bool

-- | A general function for creating robots.
mkRobot ::
  -- | ID number of the robot.
  RobotID phase ->
  -- | ID number of the robot's parent, if it has one.
  Maybe Int ->
  -- | Name of the robot.
  Text ->
  -- | Description of the robot.
  [Text] ->
  -- | Initial location.
  RobotLocation phase ->
  -- | Initial heading/direction.
  Heading ->
  -- | Robot display.
  Display ->
  -- | Initial CESK machine.
  CESK ->
  -- | Equipped devices.
  [Entity] ->
  -- | Initial inventory.
  [(Count, Entity)] ->
  -- | Should this be a system robot?
  Bool ->
  -- | Is this robot heavy?
  Bool ->
  -- | Creation date
  TimeSpec ->
  RobotR phase
mkRobot rid pid name descr loc dir disp m devs inv sys heavy ts =
  RobotR
    { _robotEntity =
        mkEntity disp name descr [] []
          & entityOrientation ?~ dir
          & entityInventory .~ fromElems inv
    , _equippedDevices = inst
    , _robotCapabilities = inventoryCapabilities inst
    , _robotLog = Seq.empty
    , _robotLogUpdated = False
    , _robotLocation = loc
    , _robotContext = emptyRobotContext
    , _robotID = rid
    , _robotParentID = pid
    , _robotHeavy = heavy
    , _robotCreatedAt = ts
    , _machine = m
    , _systemRobot = sys
    , _selfDestruct = False
    , _tickSteps = 0
    , _runningAtomic = False
    }
 where
  inst = fromList devs

-- | We can parse a robot from a YAML file if we have access to an
--   'EntityMap' in which we can look up the names of entities.
instance FromJSONE EntityMap TRobot where
  parseJSONE = withObjectE "robot" $ \v -> do
    -- Note we can't generate a unique ID here since we don't have
    -- access to a 'State GameState' effect; a unique ID will be
    -- filled in later when adding the robot to the world.
    sys <- liftE $ v .:? "system" .!= False
    let defDisplay = defaultRobotDisplay & invisible .~ sys

    mkRobot () Nothing
      <$> liftE (v .: "name")
      <*> liftE (v .:? "description" .!= [])
      <*> liftE (v .:? "loc")
      <*> liftE (v .:? "dir" .!= zero)
      <*> localE (const defDisplay) (v ..:? "display" ..!= defDisplay)
      <*> liftE (mkMachine <$> (v .:? "program"))
      <*> v ..:? "devices" ..!= []
      <*> v ..:? "inventory" ..!= []
      <*> pure sys
      <*> liftE (v .:? "heavy" .!= False)
      <*> pure 0
   where
    mkMachine Nothing = Out VUnit emptyStore []
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

hearingDistance :: Num i => i
hearingDistance = 32
