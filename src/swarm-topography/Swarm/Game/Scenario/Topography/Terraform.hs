-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Terraform where

data CellModification e
  = -- | Fields represent what existed in the cell "before" and "after", in that order.
    -- The values are guaranteed to be different.
    Swap e e
  | Remove e
  | Add e
