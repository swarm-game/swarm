-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Walkability logic
module Swarm.Game.Step.Path.Walkability where

import Control.Lens
import Data.Set qualified as S
import Swarm.Game.Entity hiding (empty, lookup, singleton, union)
import Swarm.Game.Robot.Walk
import Swarm.Language.Capability

data MoveFailureMode
  = -- | Can't move due to something blocking the path.  Note that if
    --   the robot has a path Whitelist, then the /absence/ of an entity
    --   could block the path (represented by `PathBlockedBy
    --   Nothing`).
    PathBlockedBy (Maybe Entity)
    -- | Some liquid entity is in the path.
  | PathLiquid Entity

-- | Pure logic used inside of
--   'Swarm.Game.Step.Util.checkMoveFailureUnprivileged'.  Given a
--   (possibly empty) walkable entity whitelist or blacklist, and the
--   entity (or lack thereof) in the cell we are trying to move to,
--   determine whether there is some kind of movement failure.
checkUnwalkable ::
  WalkabilityContext ->
  Maybe Entity ->
  Maybe MoveFailureMode
checkUnwalkable (WalkabilityContext _ walkExceptions) Nothing =
  -- If there's no entity in the path, we are blocked only if a
  -- whitelist of walkable entities is specified
  case walkExceptions of
    Whitelist _ -> Just $ PathBlockedBy Nothing
    Blacklist _ -> Nothing
checkUnwalkable (WalkabilityContext caps walkExceptions) (Just e)
  | isUnwalkableEntity =
      Just $ PathBlockedBy $ Just e
  -- Robots drown if they walk over liquid without the Float capability
  | e `hasProperty` Liquid && CFloat `S.notMember` caps =
      Just $ PathLiquid e
  | otherwise = Nothing
 where
  eName = e ^. entityName
  -- An entity blocks a robot if...
  isUnwalkableEntity = case walkExceptions of
    -- ...it's not one of the whitelisted entities...
    Whitelist onlyWalkables -> eName `S.notMember` onlyWalkables
    -- ...OR if it is inherently unwalkable, or is blacklisted.
    Blacklist unwalkables -> e `hasProperty` Unwalkable || eName `S.member` unwalkables
