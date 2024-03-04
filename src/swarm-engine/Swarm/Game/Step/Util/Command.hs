{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Helper functions for "Swarm.Game.Step.Const" commands
module Swarm.Game.Step.Util.Command where

import Control.Applicative (Applicative (..))
import Control.Carrier.State.Lazy
import Control.Carrier.Throw.Either (ThrowC, runThrow)
import Control.Effect.Error
import Control.Effect.Lens
import Control.Effect.Lift
import Control.Lens as Lens hiding (Const, distrib, from, parts, use, uses, view, (%=), (+=), (.=), (<+=), (<>=))
import Control.Monad (forM_, unless, when)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getZonedTime)
import Linear (zero)
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Achievement.Description (getValidityRequirements)
import Swarm.Game.CESK
import Swarm.Game.Display
import Swarm.Game.Entity hiding (empty, lookup, singleton, union)
import Swarm.Game.Entity qualified as E
import Swarm.Game.Exception
import Swarm.Game.Location
import Swarm.Game.Recipe
import Swarm.Game.Robot
import Swarm.Game.Robot.Concrete
import Swarm.Game.Robot.Walk (emptyExceptions)
import Swarm.Game.Scenario.Topography.Navigation.Portal (Navigation (..), destination, reorientation)
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Robot
import Swarm.Game.State.Substate
import Swarm.Game.Step.RobotStepState
import Swarm.Game.Step.Util
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.Language.Capability
import Swarm.Language.Context hiding (delete)
import Swarm.Language.Pipeline
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Requirement qualified as R
import Swarm.Language.Syntax
import Swarm.Language.Text.Markdown qualified as Markdown
import Swarm.Log
import Swarm.Util hiding (both)
import System.Clock (TimeSpec)
import Prelude hiding (Applicative (..), lookup)

data GrabbingCmd = Grab' | Harvest' | Swap' | Push' deriving (Eq, Show)

-- | Ensure that a robot is capable of executing a certain constant
--   (either because it has a device which gives it that capability,
--   or it is a system robot, or we are in creative mode).
ensureCanExecute :: (Has (State Robot) sig m, Has (State GameState) sig m, Has (Throw Exn) sig m) => Const -> m ()
ensureCanExecute c =
  gets @Robot (constCapsFor c) >>= \case
    Nothing -> pure ()
    Just cap -> do
      isPrivileged <- isPrivilegedBot
      robotCaps <- use robotCapabilities
      let hasCaps = cap `S.member` robotCaps
      (isPrivileged || hasCaps)
        `holdsOr` Incapable FixByEquip (R.singletonCap cap) (TConst c)

-- | Clear watches that are out of range
purgeFarAwayWatches ::
  HasRobotStepState sig m => m ()
purgeFarAwayWatches = do
  privileged <- isPrivilegedBot
  myLoc <- use robotLocation
  rid <- use robotID

  let isNearby = isNearbyOrExempt privileged myLoc
      f loc =
        if not $ isNearby loc
          then S.delete rid
          else id

  robotInfo . robotsWatching %= M.filter (not . null) . M.mapWithKey f

verbedGrabbingCmd :: GrabbingCmd -> Text
verbedGrabbingCmd = \case
  Harvest' -> "harvested"
  Grab' -> "grabbed"
  Swap' -> "swapped"
  Push' -> "pushed"

-- | Update the location of a robot, and simultaneously update the
--   'robotsByLocation' map, so we can always look up robots by
--   location.  This should be the /only/ way to update the location
--   of a robot.
-- Also implements teleportation by portals.
updateRobotLocation ::
  (HasRobotStepState sig m) =>
  Cosmic Location ->
  Cosmic Location ->
  m ()
updateRobotLocation oldLoc newLoc
  | oldLoc == newLoc = return ()
  | otherwise = do
      newlocWithPortal <- applyPortal newLoc
      rid <- use robotID
      zoomRobots $ do
        removeRobotFromLocationMap oldLoc rid
        addRobotToLocation rid newlocWithPortal
      modify (unsafeSetRobotLocation newlocWithPortal)
      flagRedraw
 where
  applyPortal loc = do
    lms <- use $ landscape . worldNavigation
    let maybePortalInfo = M.lookup loc $ portals lms
        updatedLoc = maybe loc destination maybePortalInfo
        maybeTurn = reorientation <$> maybePortalInfo
    forM_ maybeTurn $ \d ->
      robotOrientation . _Just %= applyTurn d
    return updatedLoc

-- | Execute a stateful action on a target robot --- whether the
--   current one or another.
onTarget ::
  (HasRobotStepState sig m, Has (Lift IO) sig m) =>
  RID ->
  (forall sig' m'. (HasRobotStepState sig' m', Has (Lift IO) sig' m') => m' ()) ->
  m ()
onTarget rid act = do
  myID <- use robotID
  case myID == rid of
    True -> act
    False -> do
      mtgt <- use (robotInfo . robotMap . at rid)
      case mtgt of
        Nothing -> return ()
        Just tgt -> do
          tgt' <- execState @Robot tgt act
          zoomRobots $
            if tgt' ^. selfDestruct
              then deleteRobot rid
              else robotMap . ix rid .= tgt'

-- | Enforces validity of the robot's privileged status to receive
-- an achievement.
grantAchievementForRobot ::
  (HasRobotStepState sig m, Has (Lift IO) sig m) =>
  GameplayAchievement ->
  m ()
grantAchievementForRobot a = do
  sys <- use systemRobot
  let isValidRobotType = not sys || robotTypeRequired == ValidForSystemRobot
  when isValidRobotType $
    grantAchievement a
 where
  ValidityConditions robotTypeRequired _ = getValidityRequirements a

checkGameModeAchievementValidity ::
  Has (State GameState) sig m =>
  GameplayAchievement ->
  m Bool
checkGameModeAchievementValidity a = do
  creative <- use creativeMode
  return $ not creative || gameplayModeRequired == ValidInCreativeMode
 where
  ValidityConditions _ gameplayModeRequired = getValidityRequirements a

-- | NOTE: When possible, one should use the
-- 'grantAchievementForRobot' function instead of this one.
grantAchievement ::
  (Has (State GameState) sig m, Has (Lift IO) sig m) =>
  GameplayAchievement ->
  m ()
grantAchievement a = do
  isGameModeValid <- checkGameModeAchievementValidity a
  when isGameModeValid $ do
    currentTime <- sendIO getZonedTime
    scenarioPath <- use currentScenarioPath
    discovery . gameAchievements
      %= M.insertWith
        (<>)
        a
        (Attainment (GameplayAchievement a) scenarioPath currentTime)

-- | Capabilities needed for a specific robot to evaluate or execute a
--   constant.  Right now, the only difference is whether the robot is
--   heavy or not when executing the 'Swarm.Language.Syntax.Move' command, but there might
--   be other exceptions added in the future.
constCapsFor :: Const -> Robot -> Maybe Capability
constCapsFor Move r
  | r ^. robotHeavy = Just CMoveheavy
constCapsFor Backup r
  | r ^. robotHeavy = Just CMoveheavy
constCapsFor Stride r
  | r ^. robotHeavy = Just CMoveheavy
constCapsFor c _ = constCaps c

-- | Requires that the target location is within one cell.
-- Requirement is waived if the bot is privileged.
isNearbyOrExempt :: Bool -> Cosmic Location -> Cosmic Location -> Bool
isNearbyOrExempt privileged myLoc otherLoc =
  privileged || case cosmoMeasure manhattan myLoc otherLoc of
    InfinitelyFar -> False
    Measurable x -> x <= 1

------------------------------------------------------------
-- Updating discovered entities, recipes, and commands
------------------------------------------------------------

-- | Update the global list of discovered entities, and check for new recipes.
updateDiscoveredEntities :: (HasRobotStepState sig m) => Entity -> m ()
updateDiscoveredEntities e = do
  allDiscovered <- use $ discovery . allDiscoveredEntities
  if E.contains0plus e allDiscovered
    then pure ()
    else do
      let newAllDiscovered = E.insertCount 1 e allDiscovered
      updateAvailableRecipes (newAllDiscovered, newAllDiscovered) e
      updateAvailableCommands e
      discovery . allDiscoveredEntities .= newAllDiscovered

-- | Update the availableRecipes list.
-- This implementation is not efficient:
-- * Every time we discover a new entity, we iterate through the entire list of recipes to see which ones we can make.
--   Trying to do something more clever seems like it would definitely be a case of premature optimization.
--   One doesn't discover new entities all that often.
-- * For each usable recipe, we do a linear search through the list of known recipes to see if we already know it.
--   This is a little more troubling, since it's quadratic in the number of recipes.
--   But it probably doesn't really make that much difference until we get up to thousands of recipes.
updateAvailableRecipes :: Has (State GameState) sig m => (Inventory, Inventory) -> Entity -> m ()
updateAvailableRecipes invs e = do
  allInRecipes <- use $ recipesInfo . recipesIn
  let entityRecipes = recipesFor allInRecipes e
      usableRecipes = filter (knowsIngredientsFor invs) entityRecipes
  knownRecipes <- use $ discovery . availableRecipes . notificationsContent
  let newRecipes = filter (`notElem` knownRecipes) usableRecipes
      newCount = length newRecipes
  discovery . availableRecipes %= mappend (Notifications newCount newRecipes)
  updateAvailableCommands e

updateAvailableCommands :: Has (State GameState) sig m => Entity -> m ()
updateAvailableCommands e = do
  let newCaps = e ^. entityCapabilities
      keepConsts = \case
        Just cap -> cap `S.member` newCaps
        Nothing -> False
      entityConsts = filter (keepConsts . constCaps) allConst
  knownCommands <- use $ discovery . availableCommands . notificationsContent
  let newCommands = filter (`notElem` knownCommands) entityConsts
      newCount = length newCommands
  discovery . availableCommands %= mappend (Notifications newCount newCommands)

------------------------------------------------------------
-- The "watch" command
------------------------------------------------------------

addWatchedLocation ::
  HasRobotStepState sig m =>
  Cosmic Location ->
  m ()
addWatchedLocation loc = do
  rid <- use robotID
  robotInfo . robotsWatching %= M.insertWith (<>) loc (S.singleton rid)

-- | Give some entities from a parent robot (the robot represented by
--   the ambient @State Robot@ effect) to a child robot (represented
--   by the given 'RID') as part of a 'Swarm.Language.Syntax.Build'
--   or 'Swarm.Language.Syntax.Reprogram' command.
--   The first 'Inventory' is devices to be equipped, and the second
--   is entities to be transferred.
--
--   In classic mode, the entities will be /transferred/ (that is,
--   removed from the parent robot's inventory); in creative mode, the
--   entities will be copied/created, that is, no entities will be
--   removed from the parent robot.
provisionChild ::
  (HasRobotStepState sig m) =>
  RID ->
  Inventory ->
  Inventory ->
  m ()
provisionChild childID toEquip toGive = do
  -- Equip and give devices to child
  zoomRobots $ do
    robotMap . ix childID . equippedDevices %= E.union toEquip
    robotMap . ix childID . robotInventory %= E.union toGive

  -- Delete all items from parent in classic mode
  creative <- use creativeMode
  unless creative $
    robotInventory %= (`E.difference` (toEquip `E.union` toGive))

------------------------------------------------------------
-- Exceptions and validation
------------------------------------------------------------

-- | Create an exception about a command failing, with an achievement
cmdExnWithAchievement :: Const -> [Text] -> GameplayAchievement -> Exn
cmdExnWithAchievement c parts a = CmdFailed c (T.unwords parts) $ Just a

-- | Raise an exception about a command failing with a formatted error message.
raise :: (Has (Throw Exn) sig m) => Const -> [Text] -> m a
raise c parts = throwError (cmdExn c parts)

-- | Run a subcomputation that might throw an exception in a context
--   where we are returning a CESK machine; any exception will be
--   turned into an 'Up' state.
withExceptions :: Monad m => Store -> Cont -> ThrowC Exn m CESK -> m CESK
withExceptions s k m = do
  res <- runThrow m
  case res of
    Left exn -> return $ Up exn s k
    Right a -> return a

-- | Print some text via the robot's log.
traceLog :: (Has (State GameState) sig m, Has (State Robot) sig m) => RobotLogSource -> Severity -> Text -> m LogEntry
traceLog source sev msg = do
  m <- createLogEntry source sev msg
  robotLog %= (Seq.|> m)
  return m

updateWorldAndRobots ::
  (HasRobotStepState sig m) =>
  Const ->
  [WorldUpdate Entity] ->
  [RobotUpdate] ->
  m ()
updateWorldAndRobots cmd wf rf = do
  mapM_ (updateWorld cmd) wf
  applyRobotUpdates rf
  flagRedraw

-- | Format a set of suggested devices for use in an error message,
--   in the format @device1 or device2 or ... or deviceN@.
formatDevices :: Set Entity -> Text
formatDevices = T.intercalate " or " . map (^. entityName) . S.toList

------------------------------------------------------------
-- Debugging
------------------------------------------------------------

-- | Create a log entry given current robot and game time in ticks
--   noting whether it has been said.
--
--   This is the more generic version used both for (recorded) said
--   messages and normal logs.
createLogEntry ::
  (Has (State GameState) sig m, Has (State Robot) sig m) =>
  RobotLogSource ->
  Severity ->
  Text ->
  m LogEntry
createLogEntry source sev msg = do
  rid <- use robotID
  rn <- use robotName
  time <- use $ temporal . ticks
  loc <- use robotLocation
  pure $ LogEntry time (RobotLog source rid loc) sev rn msg

-- | replace some entity in the world with another entity
updateWorld ::
  (Has (State Robot) sig m, Has (State GameState) sig m, Has (Throw Exn) sig m) =>
  Const ->
  WorldUpdate Entity ->
  m ()
updateWorld c (ReplaceEntity loc eThen down) = do
  w <- use $ landscape . multiWorld
  let eNow = W.lookupCosmicEntity (fmap W.locToCoords loc) w
  -- Can fail if a robot started a multi-tick "drill" operation on some entity
  -- and meanwhile another entity swaps it out from under them.
  if Just eThen /= eNow
    then throwError $ cmdExn c ["The", eThen ^. entityName, "is not there."]
    else updateEntityAt loc $ const down

applyRobotUpdates ::
  (Has (State GameState) sig m, Has (State Robot) sig m) =>
  [RobotUpdate] ->
  m ()
applyRobotUpdates =
  mapM_ \case
    AddEntity c e -> robotInventory %= E.insertCount c e
    LearnEntity e -> robotInventory %= E.insertCount 0 e

-- | Construct a "seed robot" from entity, time range and position,
--   and add it to the world.  It has low priority and will be covered
--   by placed entities.
addSeedBot ::
  Has (State GameState) sig m =>
  Entity ->
  (Integer, Integer) ->
  Cosmic Location ->
  TimeSpec ->
  m ()
addSeedBot e (minT, maxT) loc ts =
  zoomRobots
    . addTRobot (initMachine (seedProgram minT (maxT - minT) (e ^. entityName)) empty emptyStore)
    $ mkRobot
      Nothing
      "seed"
      (Markdown.fromText $ T.unwords ["A growing", e ^. entityName, "seed."])
      (Just loc)
      zero
      ( defaultEntityDisplay '.'
          & displayAttr .~ (e ^. entityDisplay . displayAttr)
          & displayPriority .~ 0
      )
      Nothing
      []
      [(1, e)]
      True
      False
      emptyExceptions
      ts

-- | A system program for a "seed robot", to regrow a growable entity
--   after it is harvested.
seedProgram :: Integer -> Integer -> Text -> ProcessedTerm
seedProgram minTime randTime thing =
  [tmQ|
    try {
      r <- random (1 + $int:randTime);
      wait (r + $int:minTime);
      appear "|";
      r <- random (1 + $int:randTime);
      wait (r + $int:minTime);
      place $str:thing;
    } {};
    selfdestruct
  |]

------------------------------------------------------------
-- Some utility functions
------------------------------------------------------------

verbGrabbingCmd :: GrabbingCmd -> Text
verbGrabbingCmd = \case
  Harvest' -> "harvest"
  Grab' -> "grab"
  Swap' -> "swap"
  Push' -> "push"
