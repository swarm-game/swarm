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
  _robotID,
  _robotLocation,
  RobotMachine,
  _machine,
  RobotActivity,
  _activityCounts,
  RobotLogMember,
  _robotLog,
  RobotLogUpdatedMember,
  _robotLogUpdated,

  -- * Robots
  RID,
  Robot,

  -- ** Lenses
  robotEntity,
  robotName,
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
  isInteractive,

  -- ** Constants
  hearingDistance,

  -- ** Rendering
  renderRobot,
) where

import Control.Applicative ((<|>))
import Control.Lens hiding (Const, contains)
import Data.Hashable (hashWithSalt)
import Data.Kind qualified
import Data.Text (Text)
import Data.Yaml (FromJSON (parseJSON), (.!=), (.:), (.:?))
import GHC.Generics (Generic)
import Linear
import Swarm.Game.Cosmetic.Color (AttributeMap, TrueColor)
import Swarm.Game.Cosmetic.Display (Display, defaultRobotDisplay, invisible)
import Swarm.Game.Cosmetic.Texel (Texel)
import Swarm.Game.Device
import Swarm.Game.Entity hiding (empty)
import Swarm.Game.Ingredients
import Swarm.Game.Land
import Swarm.Game.Location (Heading, Location, toHeading)
import Swarm.Game.Robot.Walk
import Swarm.Game.Universe
import Swarm.Language.JSON ()
import Swarm.Language.Pipeline (Processable (..))
import Swarm.Language.Syntax (Phase (..), Syntax)
import Swarm.Language.Text.Markdown (Document)
import Swarm.Util.Lens (makeLensesExcluding)
import Swarm.Util.Yaml
import System.Clock (TimeSpec)

-- | A unique identifier for a robot.
type RID = Int

-- | With a robot template, we may or may not have a location.  With a
--   concrete robot we must have a location.
type family RobotLocation (phase :: Phase) :: Data.Kind.Type where
  RobotLocation Raw = Maybe (Cosmic Location)
  RobotLocation Resolved = Maybe (Cosmic Location)
  RobotLocation Inferred = Maybe (Cosmic Location)
  RobotLocation Typed = Maybe (Cosmic Location)
  RobotLocation Elaborated = Maybe (Cosmic Location)
  RobotLocation Instantiated = Cosmic Location

-- | Robot templates have no ID; concrete robots definitely do.
type family RobotID (phase :: Phase) :: Data.Kind.Type where
  RobotID Raw = ()
  RobotID Resolved = ()
  RobotID Inferred = ()
  RobotID Typed = ()
  RobotID Elaborated = ()
  RobotID Instantiated = RID

type family RobotActivity (phase :: Phase) :: Data.Kind.Type
type instance RobotActivity Raw = ()
type instance RobotActivity Resolved = ()
type instance RobotActivity Inferred = ()
type instance RobotActivity Typed = ()
type instance RobotActivity Elaborated = ()

type family RobotLogMember (phase :: Phase) :: Data.Kind.Type
type instance RobotLogMember Raw = ()
type instance RobotLogMember Resolved = ()
type instance RobotLogMember Inferred = ()
type instance RobotLogMember Typed = ()
type instance RobotLogMember Elaborated = ()

type family RobotLogUpdatedMember (phase :: Phase) :: Data.Kind.Type
type instance RobotLogUpdatedMember Raw = ()
type instance RobotLogUpdatedMember Resolved = ()
type instance RobotLogUpdatedMember Inferred = ()
type instance RobotLogUpdatedMember Typed = ()
type instance RobotLogUpdatedMember Elaborated = ()

type family RobotMachine (phase :: Phase) :: Data.Kind.Type
type instance RobotMachine Raw = Maybe (Syntax Raw)
type instance RobotMachine Resolved = Maybe (Syntax Resolved)
type instance RobotMachine Inferred = Maybe (Syntax Inferred)
type instance RobotMachine Typed = Maybe (Syntax Typed)
type instance RobotMachine Elaborated = Maybe (Syntax Elaborated)

-- | A value of type 'Robot' is a record representing the state of a
--   single robot.
data Robot (phase :: Phase) = Robot
  { _robotEntity :: Entity
  , _equippedDevices :: Inventory
  , _robotCapabilities :: MultiEntityCapabilities Entity EntityName
  -- ^ A cached view of the capabilities this robot has.
  --   Automatically generated from '_equippedDevices'.
  , _robotLog :: RobotLogMember phase
  , _robotLogUpdated :: RobotLogUpdatedMember phase
  , _robotLocation :: RobotLocation phase
  , _robotID :: RobotID phase
  , _robotParentID :: Maybe RID
  , _robotHeavy :: Bool
  , _machine :: RobotMachine phase
  , _systemRobot :: Bool
  , _selfDestruct :: Bool
  , _activityCounts :: RobotActivity phase
  , _runningAtomic :: Bool
  , _unwalkableEntities :: WalkabilityExceptions EntityName
  , _robotCreatedAt :: TimeSpec
  }
  deriving (Generic)

instance Processable Robot where
  process (Robot e d c l upd loc i p h m s sd a ra u ca) =
    Robot e d c
      <$> pure l
      <*> pure upd
      <*> pure loc
      <*> pure i
      <*> pure p
      <*> pure h
      <*> traverse process m
      <*> pure s
      <*> pure sd
      <*> pure a
      <*> pure ra
      <*> pure u
      <*> pure ca

-- See https://byorgey.wordpress.com/2021/09/17/automatically-updated-cached-views-with-lens/
-- for the approach used here with lenses.

makeLensesExcluding ['_robotCapabilities, '_equippedDevices, '_robotLog, '_robotLogUpdated, '_machine, '_activityCounts] ''Robot

-- ~~~~ Note [Robot lens types]
--
-- Many lenses are used exclusively at type 'Robot Instantiated', so
-- we give them that more specific type to cut down on the number of
-- explicit type arguments we need to use.

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
--
--   This lens is actually used at multiple types; see Note [Robot
--   lens types].
robotEntity :: Lens' (Robot phase) Entity

-- | Entities that the robot cannot move onto
unwalkableEntities :: Lens' (Robot Instantiated) (WalkabilityExceptions EntityName)

-- | The creation date of the robot.
robotCreatedAt :: Lens' (Robot Instantiated) TimeSpec

-- | The name of a robot.
--
--   This lens is actually used at multiple types; see Note [Robot
--   lens types].
robotName :: Lens' (Robot phase) Text
robotName = robotEntity . entityName

-- | The 'Display' of a robot.
--
--   This lens is actually used at multiple types; see Note [Robot
--   lens types].
robotDisplay :: Lens' (Robot phase) Display
robotDisplay = robotEntity . entityDisplay

-- | The robot's current location, represented as @(x,y)@.  This is only
--   a getter, since when changing a robot's location we must remember
--   to update the 'Swarm.Game.State.robotsByLocation' map as well.  You can use the
--   'Swarm.Game.Step.updateRobotLocation' function for this purpose.
--
--   This lens is actually used at multiple types; see Note [Robot
--   lens types].
robotLocation :: Getter (Robot phase) (RobotLocation phase)

-- | Set a robot's location.  This is unsafe and should never be
--   called directly except by the 'Swarm.Game.Step.updateRobotLocation' function.
--   The reason is that we need to make sure the 'Swarm.Game.State.robotsByLocation'
--   map stays in sync.
unsafeSetRobotLocation :: Cosmic Location -> Robot Instantiated -> Robot Instantiated
unsafeSetRobotLocation loc r = r {_robotLocation = loc}

-- | A template robot's location.  Unlike 'robotLocation', this is a
--   lens, since when dealing with robot templates there is as yet no
--   'Swarm.Game.State.robotsByLocation' map to keep up-to-date.
trobotLocation :: Lens' (Robot Elaborated) (Maybe (Cosmic Location))
trobotLocation = lens _robotLocation (\r l -> r {_robotLocation = l})

-- | Which way the robot is currently facing.
robotOrientation :: Lens' (Robot Instantiated) (Maybe Heading)
robotOrientation = robotEntity . entityOrientation

-- | The robot's inventory.
--
--   This lens is actually used at multiple types; see Note [Robot
--   lens types].
robotInventory :: Lens' (Robot phase) Inventory
robotInventory = robotEntity . entityInventory

-- | The (unique) ID number of the robot.  This is only a Getter since
--   the robot ID is immutable.
robotID :: Getter (Robot Instantiated) RID

-- | The ID number of the robot's parent, that is, the robot that
--   built (or most recently reprogrammed) this robot, if there is
--   one.
robotParentID :: Lens' (Robot Instantiated) (Maybe RID)

-- | Is this robot extra heavy (thus requiring tank treads to move)?
robotHeavy :: Lens' (Robot Instantiated) Bool

-- | A separate inventory for equipped devices, which provide the
--   robot with certain capabilities.
--
--   Note that every time the inventory of equipped devices is
--   modified, this lens recomputes a cached set of the capabilities
--   the equipped devices provide, to speed up subsequent lookups to
--   see whether the robot has a certain capability (see 'robotCapabilities')
--
--   This lens is actually used at multiple types; see Note [Robot
--   lens types].
equippedDevices :: Lens' (Robot phase) Inventory
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
inventoryHash :: Getter (Robot Instantiated) Int
inventoryHash = to (\r -> 17 `hashWithSalt` (r ^. (robotEntity . entityHash)) `hashWithSalt` (r ^. equippedDevices))

-- | Does a robot know of an entity's existence?
robotKnows :: Robot Instantiated -> Entity -> Bool
robotKnows r e = contains0plus e (r ^. robotInventory) || contains0plus e (r ^. equippedDevices)

isInteractive :: Robot Instantiated -> Bool
isInteractive r = not $ r ^. robotDisplay . invisible && r ^. systemRobot

-- | Get the set of capabilities this robot possesses.  This is only a
--   getter, not a lens, because it is automatically generated from
--   the 'equippedDevices'.  The only way to change a robot's
--   capabilities is to modify its 'equippedDevices'.
robotCapabilities :: Getter (Robot Instantiated) (MultiEntityCapabilities Entity EntityName)
robotCapabilities = to _robotCapabilities

-- | Is this robot a "system robot"?  System robots are generated by
--   the system (as opposed to created by the user) and are not
--   subject to the usual capability restrictions.
systemRobot :: Lens' (Robot Instantiated) Bool

-- | Does this robot wish to self destruct?
selfDestruct :: Lens' (Robot Instantiated) Bool

-- | Is the robot currently running an atomic block?
runningAtomic :: Lens' (Robot Instantiated) Bool
walkabilityContext :: Getter (Robot Instantiated) WalkabilityContext
walkabilityContext = to $
  \x -> WalkabilityContext (getCapabilitySet $ _robotCapabilities x) (_unwalkableEntities x)

-- | A general function for creating raw or template robot records.
mkRobot ::
  ( RobotActivity phase ~ ()
  , RobotID phase ~ ()
  , RobotLocation phase ~ Maybe (Cosmic Location)
  , RobotLogMember phase ~ ()
  , RobotLogUpdatedMember phase ~ ()
  , RobotMachine phase ~ Maybe (Syntax phase)
  ) =>
  Maybe Int ->
  -- | Name of the robot.
  Text ->
  -- | Description of the robot.
  Document (Syntax Raw) ->
  -- | Initial location.
  Maybe (Cosmic Location) ->
  -- | Initial heading/direction.
  Heading ->
  -- | Robot display.
  Display ->
  -- | Initial CESK machine.
  Maybe (Syntax phase) ->
  -- | Equipped devices.
  [Entity] ->
  -- | Initial inventory.
  [(Count, Entity)] ->
  -- | Should this be a system robot?
  Bool ->
  -- | Is this robot heavy?
  Bool ->
  -- | Unwalkable entities
  WalkabilityExceptions EntityName ->
  -- | Creation date
  TimeSpec ->
  Robot phase
mkRobot pid name descr loc dir disp m devs inv sys heavy unwalkables ts =
  Robot
    { _robotEntity =
        mkEntity disp name descr [] mempty
          & entityOrientation ?~ dir
          & entityInventory .~ fromElems inv
    , _equippedDevices = inst
    , _robotCapabilities = inventoryCapabilities inst
    , _robotLog = ()
    , _robotLogUpdated = ()
    , _robotLocation = loc
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

-- | We can parse a raw robot record from a YAML file if we have
--   access to an 'EntityMap' in which we can look up the names of
--   entities.
instance FromJSONE TerrainEntityMaps (Robot Raw) where
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
      <*> localE (view entityMap) (v ..:? "devices" ..!= [])
      <*> localE (view entityMap) (v ..:? "inventory" ..!= [])
      <*> pure sys
      <*> liftE (v .:? "heavy" .!= False)
      <*> liftE (v .:? "walkable" ..!= emptyExceptions)
      <*> pure 0

hearingDistance :: (Num i) => i
hearingDistance = 32

-- | Render a robot to a texel.
renderRobot :: AttributeMap -> Robot phase -> Texel TrueColor
renderRobot aMap r = renderEntity aMap (const False) True (r ^. robotEntity)
