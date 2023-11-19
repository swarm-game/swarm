-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Walkability logic
module Swarm.Game.Step.Path.Walkability where

import Control.Lens
import Data.Set qualified as S
import Swarm.Game.Entity hiding (empty, lookup, singleton, union)
import Swarm.Game.Robot
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
  Entity ->
  Maybe MoveFailureDetails
checkUnwalkable (WalkabilityContext caps unwalkables) e
  -- robots can not walk through walls
  | e `hasProperty` Unwalkable || (e ^. entityName) `S.member` unwalkables =
      Just $ MoveFailureDetails e PathBlocked
  -- robots drown if they walk over liquid without boat
  | e `hasProperty` Liquid && CFloat `S.notMember` caps =
      Just $ MoveFailureDetails e PathLiquid
  | otherwise = Nothing
