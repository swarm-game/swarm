{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Grid on which the game takes place
--
-- A /world/ refers to the grid on which the game takes place, and the
-- things in it (besides robots). A world has a base, immutable
-- /terrain/ layer, where each cell contains a terrain type, and a
-- mutable /entity/ layer, with at most one entity per cell.
--
-- A world is technically finite but practically infinite (worlds are
-- indexed by 32-bit signed integers, so they correspond to a
-- \( 2^{32} \times 2^{32} \) torus).
module Swarm.Game.World (
  -- * Worlds
  WorldFun (..),
  runWF,
  worldFunFromArray,
  World,
  MultiWorld,

  -- ** Tile management
  loadCell,
  loadRegion,

  -- ** World functions
  newWorld,
  lookupCosmicTerrain,
  lookupTerrain,
  lookupCosmicEntity,
  lookupEntity,
  update,

  -- ** Monadic variants
  lookupTerrainM,
  lookupEntityM,
  lookupContentM,
  updateM,
  loadRegionM,

  -- ** Runtime updates
  WorldUpdate (..),

  -- * Re-Exports
  module Coords,
) where

import Swarm.Game.World.Coords as Coords
import Swarm.Game.World.Function
import Swarm.Game.World.Multi
import Swarm.Game.World.Pure
import Swarm.Game.World.Stateful
import Swarm.Game.World.Update
