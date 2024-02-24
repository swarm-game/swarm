{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A data type to represent robots.
module Swarm.Game.Robot (
  -- * Robots data
  RobotContextMember,
  _robotID,
  _robotLocation,
  _robotContext,
  RobotMachine,
  _machine,
  RobotActivity,
  _activityCounts,
  RobotLogMember,
  _robotLog,
  RobotLogUpdatedMember,
  _robotLogUpdated,

  -- * Robots
  RobotPhase (..),
  RID,
  RobotR,
  Robot,
  TRobot,

  -- * Robot context
  WalkabilityContext (..),

  -- ** Lenses
  robotEntity,
  robotName,
  trobotName,
  unwalkableEntities,
  robotCreatedAt,
  robotDisplay,
  robotLocation,
  unsafeSetRobotLocation,
  trobotLocation,
  robotOrientation,
  robotInventory,
  equippedDevices,
  inventoryHash,
  robotCapabilities,
  walkabilityContext,
  robotID,
  robotParentID,
  robotHeavy,
  systemRobot,
  selfDestruct,
  runningAtomic,

  -- ** Creation & instantiation
  mkRobot,

  -- ** Query
  robotKnows,

  -- ** Constants
  hearingDistance,
) where

import Control.Applicative ((<|>))
import Control.Lens hiding (Const, contains)
import Data.Aeson qualified as Ae (ToJSON (..))
import Data.Hashable (hashWithSalt)
import Data.Kind qualified
import Data.Set (Set)
import Data.Text (Text)
import Data.Yaml (FromJSON (parseJSON), (.!=), (.:), (.:?))
import GHC.Generics (Generic)
import Linear
import Swarm.Game.Display (Display, curOrientation, defaultRobotDisplay, invisible)
import Swarm.Game.Entity hiding (empty)
import Swarm.Game.Ingredients
import Swarm.Game.Location (Heading, Location, toDirection, toHeading)
import Swarm.Game.Universe
import Swarm.Language.Capability (Capability)
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Language.Syntax (Syntax)
import Swarm.Language.Text.Markdown (Document)
import Swarm.Util.Lens (makeLensesExcluding)
import Swarm.Util.Yaml
import System.Clock (TimeSpec)

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
type family RobotLocation (phase :: RobotPhase) :: Data.Kind.Type where
  RobotLocation 'TemplateRobot = Maybe (Cosmic Location)
  RobotLocation 'ConcreteRobot = Cosmic Location

-- | Robot templates have no ID; concrete robots definitely do.
type family RobotID (phase :: RobotPhase) :: Data.Kind.Type where
  RobotID 'TemplateRobot = ()
  RobotID 'ConcreteRobot = RID

type family RobotMachine (phase :: RobotPhase) :: Data.Kind.Type
type instance RobotMachine 'TemplateRobot = Maybe ProcessedTerm

type family RobotContextMember (phase :: RobotPhase) :: Data.Kind.Type
type instance RobotContextMember 'TemplateRobot = ()

type family RobotActivity (phase :: RobotPhase) :: Data.Kind.Type
type instance RobotActivity 'TemplateRobot = ()

type family RobotLogMember (phase :: RobotPhase) :: Data.Kind.Type
type instance RobotLogMember 'TemplateRobot = ()

type family RobotLogUpdatedMember (phase :: RobotPhase) :: Data.Kind.Type
type instance RobotLogUpdatedMember 'TemplateRobot = ()

-- | A value of type 'RobotR' is a record representing the state of a
--   single robot.  The @f@ parameter is for tracking whether or not
--   the robot has been assigned a unique ID.
data RobotR (phase :: RobotPhase) = RobotR
  { _robotEntity :: Entity
  , _equippedDevices :: Inventory
  , _robotCapabilities :: Set Capability
  -- ^ A cached view of the capabilities this robot has.
  --   Automatically generated from '_equippedDevices'.
  , _robotLog :: RobotLogMember phase
  , _robotLogUpdated :: RobotLogUpdatedMember phase
  , _robotLocation :: RobotLocation phase
  , _robotContext :: RobotContextMember phase
  , _robotID :: RobotID phase
  , _robotParentID :: Maybe RID
  , _robotHeavy :: Bool
  , _machine :: RobotMachine phase
  , _systemRobot :: Bool
  , _selfDestruct :: Bool
  , _activityCounts :: RobotActivity phase
  , _runningAtomic :: Bool
  , _unwalkableEntities :: Set EntityName
  , _robotCreatedAt :: TimeSpec
  }
  deriving (Generic)

deriving instance (Show (RobotLocation phase), Show (RobotID phase), Show (RobotMachine phase), Show (RobotContextMember phase), Show (RobotActivity phase), Show (RobotLogMember phase), Show (RobotLogUpdatedMember phase)) => Show (RobotR phase)
deriving instance (Eq (RobotLocation phase), Eq (RobotID phase), Eq (RobotMachine phase), Eq (RobotContextMember phase), Eq (RobotActivity phase), Eq (RobotLogMember phase), Eq (RobotLogUpdatedMember phase)) => Eq (RobotR phase)

-- See https://byorgey.wordpress.com/2021/09/17/automatically-updated-cached-views-with-lens/
-- for the approach used here with lenses.

makeLensesExcluding ['_robotCapabilities, '_equippedDevices, '_robotLog, '_robotLogUpdated, '_robotContext, '_machine, '_activityCounts] ''RobotR

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

-- | Entities that the robot cannot move onto
unwalkableEntities :: Lens' Robot (Set EntityName)

-- | The creation date of the robot.
robotCreatedAt :: Lens' Robot TimeSpec

-- robotName and trobotName could be generalized to
-- @robotName' :: Lens' (RobotR phase) Text@.
-- However, type inference does not work
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

-- | The robot's current location, represented as @(x,y)@.  This is only
--   a getter, since when changing a robot's location we must remember
--   to update the 'Swarm.Game.State.robotsByLocation' map as well.  You can use the
--   'Swarm.Game.Step.updateRobotLocation' function for this purpose.
robotLocation :: Getter Robot (Cosmic Location)

-- | Set a robot's location.  This is unsafe and should never be
--   called directly except by the 'Swarm.Game.Step.updateRobotLocation' function.
--   The reason is that we need to make sure the 'Swarm.Game.State.robotsByLocation'
--   map stays in sync.
unsafeSetRobotLocation :: Cosmic Location -> Robot -> Robot
unsafeSetRobotLocation loc r = r {_robotLocation = loc}

-- | A template robot's location.  Unlike 'robotLocation', this is a
--   lens, since when dealing with robot templates there is as yet no
--   'Swarm.Game.State.robotsByLocation' map to keep up-to-date.
trobotLocation :: Lens' TRobot (Maybe (Cosmic Location))
trobotLocation = lens _robotLocation (\r l -> r {_robotLocation = l})

-- | Which way the robot is currently facing.
robotOrientation :: Lens' Robot (Maybe Heading)
robotOrientation = robotEntity . entityOrientation

-- | The robot's inventory.
robotInventory :: Lens' Robot Inventory
robotInventory = robotEntity . entityInventory

-- | The (unique) ID number of the robot.  This is only a Getter since
--   the robot ID is immutable.
robotID :: Getter Robot RID

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

-- | Is this robot a "system robot"?  System robots are generated by
--   the system (as opposed to created by the user) and are not
--   subject to the usual capability restrictions.
systemRobot :: Lens' Robot Bool

-- | Does this robot wish to self destruct?
selfDestruct :: Lens' Robot Bool

-- | Is the robot currently running an atomic block?
runningAtomic :: Lens' Robot Bool

-- | Properties of a robot used to determine whether an entity is walkable
data WalkabilityContext
  = WalkabilityContext
      (Set Capability)
      -- | which entities are unwalkable by this robot
      (Set EntityName)
  deriving (Show, Eq, Generic, Ae.ToJSON)

walkabilityContext :: Getter Robot WalkabilityContext
walkabilityContext = to $
  \x -> WalkabilityContext (_robotCapabilities x) (_unwalkableEntities x)

-- | A general function for creating robots.
mkRobot ::
  Maybe Int ->
  -- | Name of the robot.
  Text ->
  -- | Description of the robot.
  Document Syntax ->
  -- | Initial location.
  Maybe (Cosmic Location) ->
  -- | Initial heading/direction.
  Heading ->
  -- | Robot display.
  Display ->
  -- | Initial CESK machine.
  Maybe ProcessedTerm ->
  -- | Equipped devices.
  [Entity] ->
  -- | Initial inventory.
  [(Count, Entity)] ->
  -- | Should this be a system robot?
  Bool ->
  -- | Is this robot heavy?
  Bool ->
  -- | Unwalkable entities
  Set EntityName ->
  -- | Creation date
  TimeSpec ->
  TRobot
mkRobot pid name descr loc dir disp m devs inv sys heavy unwalkables ts =
  RobotR
    { _robotEntity =
        mkEntity disp name descr [] mempty
          & entityOrientation ?~ dir
          & entityInventory .~ fromElems inv
    , _equippedDevices = inst
    , _robotCapabilities = inventoryCapabilities inst
    , _robotLog = ()
    , _robotLogUpdated = ()
    , _robotLocation = loc
    , _robotContext = ()
    , _robotID = ()
    , _robotParentID = pid
    , _robotHeavy = heavy
    , _robotCreatedAt = ts
    , _machine = m
    , _systemRobot = sys
    , _selfDestruct = False
    , _activityCounts = ()
    , _runningAtomic = False
    , _unwalkableEntities = unwalkables
    }
 where
  inst = fromList devs

newtype HeadingSpec = HeadingSpec
  { getHeading :: Heading
  }

instance FromJSON HeadingSpec where
  parseJSON x = fmap HeadingSpec $ (toHeading <$> parseJSON x) <|> parseJSON x

-- | We can parse a robot from a YAML file if we have access to an
--   'EntityMap' in which we can look up the names of entities.
instance FromJSONE EntityMap TRobot where
  parseJSONE = withObjectE "robot" $ \v -> do
    -- Note we can't generate a unique ID here since we don't have
    -- access to a 'State GameState' effect; a unique ID will be
    -- filled in later when adding the robot to the world.
    sys <- liftE $ v .:? "system" .!= False
    let defDisplay = defaultRobotDisplay & invisible .~ sys

    mkRobot Nothing
      <$> liftE (v .: "name")
      <*> liftE (v .:? "description" .!= mempty)
      <*> liftE (v .:? "loc")
      <*> liftE (fmap getHeading $ v .:? "dir" .!= HeadingSpec zero)
      <*> localE (const defDisplay) (v ..:? "display" ..!= defDisplay)
      <*> liftE (v .:? "program")
      <*> v ..:? "devices" ..!= []
      <*> v ..:? "inventory" ..!= []
      <*> pure sys
      <*> liftE (v .:? "heavy" .!= False)
      <*> liftE (v .:? "unwalkable" ..!= mempty)
      <*> pure 0

hearingDistance :: (Num i) => i
hearingDistance = 32
