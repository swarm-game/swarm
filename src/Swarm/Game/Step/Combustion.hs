{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Some entities are "combustible". A command, `ignite`, will
-- initiate combustion on such an entity.
-- Furthermore, combustion can spread to (4-)adjacent entities, depending
-- on the 'ignition' property of that entity.
--
-- Short-lived robots are used to illustrate the combusting entity as
-- well as to initiate the delayed combustion of its neighbors.
module Swarm.Game.Step.Combustion where

import Control.Lens as Lens hiding (Const, distrib, from, parts, use, uses, view, (%=), (+=), (.=), (<+=), (<>=))
import Swarm.Language.Context (empty)
import Swarm.Game.CESK (emptyStore, initMachine)
import Swarm.Game.Display
import Swarm.Game.State
import Swarm.Game.Step.Util
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Game.Universe
import Swarm.Game.Entity qualified as E
import Swarm.Game.Location
import Swarm.Game.Entity hiding (empty, lookup, singleton, union)
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Text.Markdown qualified as Markdown
import Control.Applicative (Applicative (..))
import Control.Carrier.State.Lazy
import Control.Effect.Lens
import Control.Effect.Lift
import Control.Monad (when, void, forM_)
import Data.Text qualified as T
import Linear (zero)
import Swarm.Game.Robot
import Swarm.Language.Syntax
import Swarm.Util hiding (both)
import System.Clock (TimeSpec)
import Prelude hiding (Applicative (..), lookup)

igniteCommand :: (HasRobotStepState sig m, Has (Lift IO) sig m) => Const -> Direction -> m ()
igniteCommand c d = do
  (loc, me) <- lookInDirection d
  -- Ensure there is an entity here.
  e <-
    me `isJustOrFail` ["There is nothing here to", verb <> "."]

  -- Ensure it can be ignited.
  (e `hasProperty` Combustible)
    `holdsOrFail` ["The", e ^. entityName, "here can't be", verbed <> "."]

  -- Remove the entity from the world.
  updateEntityAt loc (const Nothing)
  flagRedraw

  -- Start burning process
  let selfCombustibility = (e ^. entityCombustion) ? defaultCombustibility
  createdAt <- getNow
  combustionDurationRand <- addCombustionBot e selfCombustibility createdAt loc

  let neighborLocs = map (offsetBy loc . flip applyTurn north . DRelative . DPlanar) listEnums
  forM_ neighborLocs $ igniteNeighbor createdAt combustionDurationRand

  where
    verb = "ignite"
    verbed = "ignited"

    holdsOrFail = holdsOrFail' c
    isJustOrFail = isJustOrFail' c


-- | Construct a "combustion robot" from entity and position
--   and add it to the world.
--   It has low priority and will be covered
--   by placed entities.
--   The "combustion bot" represents the burning of a single
--   entity; propagating the fire to neighbors is handled upstream,
--   within the `ignite` command.
addCombustionBot ::
  Has (State GameState) sig m =>
  Entity ->
  Combustibility ->
  TimeSpec ->
  Cosmic Location ->
  m Integer
addCombustionBot inputEntity combustibility ts loc = do
  botInventory <- case maybeCombustionProduct of
    Nothing -> return []
    Just n -> do
      maybeE <- uses entityMap (lookupEntityName n)
      return $ maybe [] (pure . (1,)) maybeE
  combustionDurationRand <- uniform durationRange
  let combustionProg = combustionProgram combustionDurationRand combustibility
  void $
    addTRobot $
      mkRobot
        ()
        Nothing
        "fire"
        (Markdown.fromText $ T.unwords ["A burning", (inputEntity ^. entityName) <> "."])
        (Just loc)
        zero
        ( defaultEntityDisplay '*'
            & displayAttr .~ AWorld "fire"
            & displayPriority .~ 0
        )
        (initMachine combustionProg empty emptyStore)
        []
        botInventory
        True
        False
        ts
  return combustionDurationRand
 where
  Combustibility _ durationRange maybeCombustionProduct = combustibility

-- Triggers the ignition of the entity underfoot with some delay.
ignitionProgram :: Integer -> ProcessedTerm
ignitionProgram waitTime =
  [tmQ|
    wait $int:waitTime;
    try {
      ignite down;
      noop;
    } {};
    selfdestruct
  |]

-- | A system program for a "combustion robot", to burn an entity
--   after it is ignited.
--
-- For efficiency, we determine a priori (i.e. the instant
-- the combustion robot is spawned) whether any neighbors will eventually
-- be burned, based on probabilities.
--
-- Note that it is possible that new neighbors may be introduced while
-- combustion is in progress. Although it may be more realistic to subject
-- these to possible combustion as well, we do not bother.
--
-- Though if we did actually want to do that, some options are:
--
-- 1. Create sub-partitions (of say, 10-tick duration) of the combustion duration
--    to re-evaluate opportunities to light adjacent entities on fire.
-- 2. Use the `watch` command to observe for changes to adjacent entities.
--    Note that if we "wake" from our `wait` due to the `watch` being triggered,
--    we would need to maintain bookkeeping of how much time is left.
-- 3. Spawn more robots whose sole purpose is to observe for changes to neighbor
--    cells. This would avoid polluting the logic of the currently burning cell
--    with logic to manage probabilities of combustion propagation.
combustionProgram :: Integer -> Combustibility -> ProcessedTerm
combustionProgram combustionDuration (Combustibility _ _ maybeCombustionProduct) =
  [tmQ|
    wait $int:combustionDuration;
    if ($int:invQuantity > 0) {
      try {
        place $str:combustionProduct;
      } {};
    } {};
    selfdestruct
  |]
 where
  (invQuantity, combustionProduct) = case maybeCombustionProduct of
    Nothing -> (0, "")
    Just p -> (1, p)

-- | We treat the 'ignition' field in the 'Combustion' record
-- as a /rate/ in a Poisson distribution.
-- Ignition of neighbors depends on that particular neighbor entity's
-- combustion /rate/, but also on the duration
-- that the current entity will burn.
igniteNeighbor ::
  Has (State GameState) sig m =>
  TimeSpec ->
  Integer ->
  Cosmic Location ->
  m ()
igniteNeighbor creationTime sourceDuration loc = do
  maybeEnt <- entityAt loc
  forM_ maybeEnt igniteEntity
 where
  igniteEntity e =
    when (e `hasProperty` Combustible) $ do
      threshold <- uniform (0, 1)
      when (probabilityOfIgnition >= threshold) $ do
        ignitionDelayRand <- uniform (0, 1)
        let ignitionDelay =
              floor
                . min (fromIntegral sourceDuration)
                . negate
                $ log ignitionDelayRand / rate
        addIgnitionBot ignitionDelay e creationTime loc
   where
    neighborCombustibility = (e ^. entityCombustion) ? defaultCombustibility
    rate = E.ignition neighborCombustibility
    probabilityOfIgnition = 1 - exp (negate $ rate * fromIntegral sourceDuration)

-- | Construct an invisible "ignition robot" and add it to the world.
--   Its sole purpose is to delay the `ignite` command for a neighbor
--   that has been a priori determined that it shall be ignited.
addIgnitionBot ::
  Has (State GameState) sig m =>
  Integer ->
  Entity ->
  TimeSpec ->
  Cosmic Location ->
  m ()
addIgnitionBot ignitionDelay inputEntity ts loc =
  void $
    addTRobot $
      mkRobot
        ()
        Nothing
        "firestarter"
        (Markdown.fromText $ T.unwords ["Delayed ignition of", (inputEntity ^. entityName) <> "."])
        (Just loc)
        zero
        ( defaultEntityDisplay '*'
            & invisible .~ True
        )
        (initMachine (ignitionProgram ignitionDelay) empty emptyStore)
        []
        []
        True
        False
        ts
