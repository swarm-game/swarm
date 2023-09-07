{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Step.Util where

import Control.Applicative (Applicative (..))
import Control.Carrier.State.Lazy
import Control.Effect.Error
import Control.Effect.Lens
import Control.Effect.Lift
import Control.Lens as Lens hiding (Const, distrib, from, parts, use, uses, view, (%=), (+=), (.=), (<+=), (<>=))
import Control.Monad (forM, join, when)
import Data.Array (bounds, (!))
import Data.IntMap qualified as IM
import Data.List (find)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Linear (zero)
import Swarm.Game.Entity hiding (empty, lookup, singleton, union)
import Swarm.Game.Exception
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.Language.Capability
import Swarm.Language.Requirement qualified as R
import Swarm.Language.Syntax
import Swarm.Util hiding (both)
import System.Clock (TimeSpec)
import System.Clock qualified
import System.Random (UniformRange, uniformR)
import Prelude hiding (Applicative (..), lookup)

-- | All functions that are used for robot step can access 'GameState' and the current 'Robot'.
--
-- They can also throw exception of our custom type, which is handled elsewhere.
-- Because of that the constraint is only 'Throw', but not 'Catch'/'Error'.
type HasRobotStepState sig m = (Has (State GameState) sig m, Has (State Robot) sig m, Has (Throw Exn) sig m)

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

-- | Modify the entity (if any) at a given location.
updateEntityAt ::
  (Has (State GameState) sig m) =>
  Cosmic Location ->
  (Maybe Entity -> Maybe Entity) ->
  m ()
updateEntityAt cLoc@(Cosmic subworldName loc) upd = do
  didChange <-
    fmap (fromMaybe False) $
      zoomWorld subworldName $
        W.updateM @Int (W.locToCoords loc) upd
  when didChange $
    wakeWatchingRobots cLoc

-- | Exempts the robot from various command constraints
-- when it is either a system robot or playing in creative mode
isPrivilegedBot :: (Has (State GameState) sig m, Has (State Robot) sig m) => m Bool
isPrivilegedBot = (||) <$> use systemRobot <*> use creativeMode

-- * Exceptions

-- | Test whether the current robot has a given capability (either
--   because it has a device which gives it that capability, or it is a
--   system robot, or we are in creative mode).
hasCapability :: (Has (State Robot) sig m, Has (State GameState) sig m) => Capability -> m Bool
hasCapability cap = do
  isPrivileged <- isPrivilegedBot
  caps <- use robotCapabilities
  return (isPrivileged || cap `S.member` caps)

-- | Ensure that either a robot has a given capability, OR we are in creative
--   mode.
hasCapabilityFor ::
  (Has (State Robot) sig m, Has (State GameState) sig m, Has (Throw Exn) sig m) => Capability -> Term -> m ()
hasCapabilityFor cap term = do
  h <- hasCapability cap
  h `holdsOr` Incapable FixByEquip (R.singletonCap cap) term

holdsOrFail' :: (Has (Throw Exn) sig m) => Const -> Bool -> [Text] -> m ()
holdsOrFail' c a ts = a `holdsOr` cmdExn c ts

isJustOrFail' :: (Has (Throw Exn) sig m) => Const -> Maybe a -> [Text] -> m a
isJustOrFail' c a ts = a `isJustOr` cmdExn c ts

-- | Create an exception about a command failing.
cmdExn :: Const -> [Text] -> Exn
cmdExn c parts = CmdFailed c (T.unwords parts) Nothing

getNow :: Has (Lift IO) sig m => m TimeSpec
getNow = sendIO $ System.Clock.getTime System.Clock.Monotonic

------------------------------------------------------------
-- Some utility functions
------------------------------------------------------------

-- | Set a flag telling the UI that the world needs to be redrawn.
flagRedraw :: (Has (State GameState) sig m) => m ()
flagRedraw = needsRedraw .= True

-- | Perform an action requiring a 'W.World' state component in a
--   larger context with a 'GameState'.
zoomWorld ::
  (Has (State GameState) sig m) =>
  SubworldName ->
  StateC (W.World Int Entity) Identity b ->
  m (Maybe b)
zoomWorld swName n = do
  mw <- use multiWorld
  forM (M.lookup swName mw) $ \w -> do
    let (w', a) = run (runState w n)
    multiWorld %= M.insert swName w'
    return a

-- | Get the entity (if any) at a given location.
entityAt :: (Has (State GameState) sig m) => Cosmic Location -> m (Maybe Entity)
entityAt (Cosmic subworldName loc) =
  join <$> zoomWorld subworldName (W.lookupEntityM @Int (W.locToCoords loc))

-- | Get the robot with a given ID.
robotWithID :: (Has (State GameState) sig m) => RID -> m (Maybe Robot)
robotWithID rid = use (robotMap . at rid)

-- | Get the robot with a given name.
robotWithName :: (Has (State GameState) sig m) => Text -> m (Maybe Robot)
robotWithName rname = use (robotMap . to IM.elems . to (find $ \r -> r ^. robotName == rname))

-- | Generate a uniformly random number using the random generator in
--   the game state.
uniform :: (Has (State GameState) sig m, UniformRange a) => (a, a) -> m a
uniform bnds = do
  rand <- use randGen
  let (n, g) = uniformR bnds rand
  randGen .= g
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
  adjs <- use @GameState adjList
  names <- use @GameState nameList
  i <- uniform (bounds adjs)
  j <- uniform (bounds names)
  return $ T.concat [adjs ! i, "_", names ! j]
