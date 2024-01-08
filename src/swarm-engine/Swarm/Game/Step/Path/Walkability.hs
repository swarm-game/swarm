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

data MoveFailureMode = PathBlocked | PathLiquid

data MoveFailureDetails
  = MoveFailureDetails
      -- | Occupies the destination cell
      Entity
      MoveFailureMode

-- | Pure logic used inside of
-- 'Swarm.Game.Step.Util.checkMoveFailureUnprivileged'
checkUnwalkable ::
  WalkabilityContext ->
  -- TODO: Accept a (Maybe Entity)
  Entity ->
  Maybe MoveFailureDetails
checkUnwalkable (WalkabilityContext caps walkExceptions) e
  -- robots can not walk through walls
  | isUnwalkableEntity =
      Just $ MoveFailureDetails e PathBlocked
  -- robots drown if they walk over liquid without boat
  | e `hasProperty` Liquid && CFloat `S.notMember` caps =
      Just $ MoveFailureDetails e PathLiquid
  | otherwise = Nothing
 where
  eName = e ^. entityName
  isUnwalkableEntity = case walkExceptions of
    Whitelist onlyWalkables -> eName `S.notMember` onlyWalkables
    Blacklist unwalkables -> e `hasProperty` Unwalkable || eName `S.member` unwalkables
