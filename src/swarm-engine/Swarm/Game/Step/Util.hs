{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for implementing robot commands.
module Swarm.Game.Step.Util where

import Control.Carrier.State.Lazy
import Control.Effect.Error
import Control.Effect.Lens
import Control.Monad (forM_, guard, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.State.Strict qualified as TS
import Data.Array (bounds, (!))
import Data.IntMap qualified as IM
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
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
import System.Random (UniformRange, uniformR)
import Prelude hiding (lookup)

deriveHeading :: HasRobotStepState sig m => Direction -> m Heading
deriveHeading d = do
  orient <- use robotOrientation
  when (isCardinal d) $ hasCapabilityFor COrient $ TDir d
  return $ applyTurn d $ orient ? zero

lookInDirection :: HasRobotStepState sig m => Direction -> m (Cosmic Location, Maybe Entity)
lookInDirection d = do
  newHeading <- deriveHeading d
  loc <- use robotLocation
  let nextLoc = loc `offsetBy` newHeading
  (nextLoc,) <$> entityAt nextLoc

adaptGameState ::
  Has (State GameState) sig m =>
  TS.State GameState b ->
  m b
adaptGameState f = do
  (newRecognizer, newGS) <- TS.runState f <$> get
  put newGS
  return newRecognizer

-- | Modify the entity (if any) at a given location.
updateEntityAt ::
  (Has (State Robot) sig m, Has (State GameState) sig m) =>
  Cosmic Location ->
  (Maybe Entity -> Maybe Entity) ->
  m ()
updateEntityAt cLoc@(Cosmic subworldName loc) upd = do
  someChange <-
    zoomWorld subworldName $
      W.updateM @Int (locToCoords loc) upd

  forM_ (WM.getModification =<< someChange) $ \modType -> do
    currentTick <- use $ temporal . ticks
    myID <- use robotID
    zoomRobots $ wakeWatchingRobots myID currentTick cLoc

    structureRecognizer <- use $ landscape . recognizerAutomatons
    oldRecognition <- use $ discovery . structureRecognition
    newRecognition <- adaptGameState $ SRT.entityModified mtlEntityAt modType cLoc structureRecognizer oldRecognition
    discovery . structureRecognition .= newRecognition

    pcr <- use $ pathCaching . pathCachingRobots
    mapM_ (revalidatePathCache cLoc modType) $ IM.toList pcr

-- * Capabilities

-- | Exempts the robot from various command constraints
-- when it is either a system robot or playing in creative mode
isPrivilegedBot :: (Has (State GameState) sig m, Has (State Robot) sig m) => m Bool
isPrivilegedBot = (||) <$> use systemRobot <*> use creativeMode

-- | Test whether the current robot has a given capability (either
--   because it has a device which gives it that capability, or it is a
--   system robot, or we are in creative mode).
hasCapability :: (Has (State Robot) sig m, Has (State GameState) sig m) => Capability -> m Bool
hasCapability cap = do
  isPrivileged <- isPrivilegedBot
  caps <- use robotCapabilities
  return (isPrivileged || cap `S.member` getCapabilitySet caps)

-- | Ensure that either a robot has a given capability, OR we are in creative
--   mode.
hasCapabilityFor ::
  (Has (State Robot) sig m, Has (State GameState) sig m, Has (Throw Exn) sig m) => Capability -> Term -> m ()
hasCapabilityFor cap term = do
  h <- hasCapability cap
  h `holdsOr` Incapable FixByEquip (R.singletonCap cap) term

-- * Exceptions

holdsOrFail' :: (Has (Throw Exn) sig m) => Const -> Bool -> [Text] -> m ()
holdsOrFail' c a ts = a `holdsOr` cmdExn c ts

isJustOrFail' :: (Has (Throw Exn) sig m) => Const -> Maybe a -> [Text] -> m a
isJustOrFail' c a ts = a `isJustOr` cmdExn c ts

-- | Create an exception about a command failing.
cmdExn :: Const -> [Text] -> Exn
cmdExn c parts = CmdFailed c (T.unwords parts) Nothing

-- * Some utility functions

-- | Set a flag telling the UI that the world needs to be redrawn.
flagRedraw :: (Has (State GameState) sig m) => m ()
flagRedraw = needsRedraw .= True

-- * Randomness

-- | Generate a uniformly random number using the random generator in
--   the game state.
uniform :: (Has (State GameState) sig m, UniformRange a) => (a, a) -> m a
uniform bnds = do
  rand <- use $ randomness . randGen
  let (n, g) = uniformR bnds rand
  randomness . randGen .= g
  return n

-- | Given a weighting function and a list of values, choose one of
--   the values randomly (using the random generator in the game
--   state), with the probability of each being proportional to its
--   weight.  Return @Nothing@ if the list is empty.
weightedChoice :: Has (State GameState) sig m => (a -> Integer) -> [a] -> m (Maybe a)
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
randomName :: Has (State GameState) sig m => m Text
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
  HasRobotStepState sig m =>
  Cosmic Location ->
  m (Maybe MoveFailureMode)
checkMoveFailureUnprivileged nextLoc = do
  me <- entityAt nextLoc
  wc <- use walkabilityContext
  return $ checkUnwalkable wc me

-- | Check whether moving to the given location causes any kind of
--   failure.  Note that system robots have unrestricted movement and
--   never fail, but non-system robots have restricted movement even
--   in creative mode.
checkMoveFailure :: HasRobotStepState sig m => Cosmic Location -> m (Maybe MoveFailureMode)
checkMoveFailure nextLoc = do
  systemRob <- use systemRobot
  runMaybeT $ do
    guard $ not systemRob
    maybeMoveFailure <- lift $ checkMoveFailureUnprivileged nextLoc
    hoistMaybe maybeMoveFailure
