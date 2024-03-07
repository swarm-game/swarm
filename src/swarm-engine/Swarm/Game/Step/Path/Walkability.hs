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
  = -- | If the robot has a path Whitelist,
    -- then the absence of an entity prevents walkability.
    PathBlockedBy (Maybe Entity)
  | PathLiquid Entity

-- | Pure logic used inside of
-- 'Swarm.Game.Step.Util.checkMoveFailureUnprivileged'
checkUnwalkable ::
  WalkabilityContext ->
  Maybe Entity ->
  Maybe MoveFailureMode
checkUnwalkable (WalkabilityContext caps walkExceptions) (Just e)
  -- robots can not walk through walls
  | isUnwalkableEntity =
      Just $ PathBlockedBy $ Just e
  -- robots drown if they walk over liquid without boat
  | e `hasProperty` Liquid && CFloat `S.notMember` caps =
      Just $ PathLiquid e
  | otherwise = Nothing
 where
  eName = e ^. entityName
  isUnwalkableEntity = case walkExceptions of
    Whitelist onlyWalkables -> eName `S.notMember` onlyWalkables
    Blacklist unwalkables -> e `hasProperty` Unwalkable || eName `S.member` unwalkables
checkUnwalkable (WalkabilityContext _ walkExceptions) Nothing =
  case walkExceptions of
    Whitelist _ -> Just $ PathBlockedBy Nothing
    Blacklist _ -> Nothing
