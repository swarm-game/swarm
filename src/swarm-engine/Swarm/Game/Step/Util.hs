{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for implementing robot commands.
module Swarm.Game.Step.Util where

import Control.Monad (forM_, guard, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.Array (bounds, (!))
import Data.IntMap qualified as IM
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Linear (zero)
import Swarm.Game.Device
import Swarm.Game.Entity hiding (empty, lookup, singleton, union)
import Swarm.Game.Exception
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.Scenario.Topography.Modify qualified as WM
import Swarm.Game.Scenario.Topography.Structure.Recognition.Tracking qualified as SRT
import Swarm.Game.State
import Swarm.Game.State.Landscape (recognizerAutomatons)
import Swarm.Game.State.Robot
import Swarm.Game.State.Substate
import Swarm.Game.Step.Path.Cache
import Swarm.Game.Step.Path.Type
import Swarm.Game.Step.Path.Walkability
import Swarm.Game.Step.RobotStepState
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.Game.World.Coords
import Swarm.Language.Capability
import Swarm.Language.Requirements.Type qualified as R
import Swarm.Language.Syntax
import Swarm.Language.Syntax.Direction (Direction)
import Swarm.ResourceLoading (NameGenerator (..))
import Swarm.Util hiding (both)
import Swarm.Util.Lens
import System.Random (UniformRange, uniformR)
import Prelude hiding (lookup)

deriveHeading :: HasRobotStepState es => Direction -> Eff es Heading
deriveHeading d = do
  orient <- use robotOrientation
  when (isCardinal d) $ hasCapabilityFor COrient $ TDir d
  return $ applyTurn d $ orient ? zero

lookInDirection :: HasRobotStepState es => Direction -> Eff es (Cosmic Location, Maybe Entity)
lookInDirection d = do
  newHeading <- deriveHeading d
  loc <- use robotLocation
  let nextLoc = loc `offsetBy` newHeading
  (nextLoc,) <$> entityAt nextLoc

-- | Modify the entity (if any) at a given location, and mark the cell
--   dirty (i.e. needing to be redrawn) if anything changes.
updateEntityAt ::
  HasRobotStepState es =>
  Cosmic Location ->
  (Maybe Entity -> Maybe Entity) ->
  Eff es ()
updateEntityAt cLoc@(Cosmic subworldName loc) upd = do
  someChange <-
    zoomWorld subworldName $ \wMetric ->
      W.updateM @Int wMetric (locToCoords loc) upd

  forM_ (WM.getModification =<< someChange) $ \modType -> do
    currentTick <- use $ temporal . ticks
    myID <- use robotID
    zoomRobots $ wakeWatchingRobots myID currentTick cLoc

    structureRecognizer <- use $ landscape . recognizerAutomatons
    oldRecognition <- use $ discovery . structureRecognition
    newRecognition <- SRT.entityModified entityAt modType cLoc structureRecognizer oldRecognition
    discovery . structureRecognition .= newRecognition

    pcr <- use $ pathCaching . pathCachingRobots
    mapM_ (revalidatePathCache cLoc modType) $ IM.toList pcr

    markDirty cLoc

-- * Capabilities

-- | Exempts the robot from various command constraints
-- when it is either a system robot or playing in creative mode
isPrivilegedBot :: (State GameState :> es, State (Robot Instantiated) :> es) => Eff es Bool
isPrivilegedBot = (||) <$> use systemRobot <*> use creativeMode

-- | Test whether the current robot has a given capability (either
--   because it has a device which gives it that capability, or it is a
--   system robot, or we are in creative mode).
hasCapability :: (State (Robot Instantiated) :> es, State GameState :> es) => Capability -> Eff es Bool
hasCapability cap = do
  isPrivileged <- isPrivilegedBot
  caps <- use robotCapabilities
  return (isPrivileged || cap `S.member` getCapabilitySet caps)

-- | Ensure that either a robot has a given capability, OR we are in creative
--   mode.
hasCapabilityFor ::
  (State (Robot Instantiated) :> es, State GameState :> es, Error Exn :> es) => Capability -> Term Resolved -> Eff es ()
hasCapabilityFor cap term = do
  h <- hasCapability cap
  h `holdsOr` Incapable FixByEquip (R.singletonCap cap) term

-- * Exceptions

holdsOrFail' :: (Error Exn :> es) => Const -> Bool -> [Text] -> Eff es ()
holdsOrFail' c a ts = a `holdsOr` cmdExn c ts

isJustOrFail' :: (Error Exn :> es) => Const -> Maybe a -> [Text] -> Eff es a
isJustOrFail' c a ts = a `isJustOr` cmdExn c ts

-- | Create an exception about a command failing.
cmdExn :: Const -> [Text] -> Exn
cmdExn c parts = CmdFailed c (T.unwords parts) Nothing

-- * Randomness

-- | Generate a uniformly random number using the random generator in
--   the game state.
uniform :: (State GameState :> es, UniformRange a) => (a, a) -> Eff es a
uniform bnds = do
  rand <- use $ randomness . randGen
  let (n, g) = uniformR bnds rand
  randomness . randGen .= g
  return n

-- | Given a weighting function and a list of values, choose one of
--   the values randomly (using the random generator in the game
--   state), with the probability of each being proportional to its
--   weight.  Return @Nothing@ if the list is empty.
weightedChoice :: State GameState :> es => (a -> Integer) -> [a] -> Eff es (Maybe a)
weightedChoice weight as = do
  r <- uniform (0, total - 1)
  return $ go r as
 where
  total = sum (map weight as)

  go _ [] = Nothing
  go !k (x : xs)
    | k < w = Just x
    | otherwise = go (k - w) xs
   where
    w = weight x

-- | Generate a random robot name in the form @adjective_name@.
randomName :: State GameState :> es => Eff es Text
randomName = do
  NameGenerator adjs names <- use $ robotInfo . robotNaming . nameGenerator
  i <- uniform (bounds adjs)
  j <- uniform (bounds names)
  return $ T.concat [adjs ! i, "_", names ! j]

-- * Moving

-- | Raw check whether moving to the given location causes any kind of
--   failure, with no special checks for system robots (see also
--   'checkMoveFailure').
checkMoveFailureUnprivileged ::
  HasRobotStepState es =>
  Cosmic Location ->
  Eff es (Maybe MoveFailureMode)
checkMoveFailureUnprivileged nextLoc = do
  me <- entityAt nextLoc
  wc <- use walkabilityContext
  return $ checkUnwalkable wc me

-- | Check whether moving to the given location causes any kind of
--   failure.  Note that system robots have unrestricted movement and
--   never fail, but non-system robots have restricted movement even
--   in creative mode.
checkMoveFailure :: HasRobotStepState es => Cosmic Location -> Eff es (Maybe MoveFailureMode)
checkMoveFailure nextLoc = do
  systemRob <- use systemRobot
  runMaybeT $ do
    guard $ not systemRob
    maybeMoveFailure <- lift $ checkMoveFailureUnprivileged nextLoc
    hoistMaybe maybeMoveFailure
