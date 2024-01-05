-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Config data required by initializers of the
-- 'Swarm.Game.State.GameState' record and its subrecords.
module Swarm.Game.State.Config where

import Swarm.Game.Entity (Entity, EntityMap)
import Swarm.Game.Recipe (Recipe)
import Swarm.Game.ResourceLoading (NameGenerator)
import Swarm.Game.World.Typecheck (WorldMap)

-- | Record to pass information needed to create an initial
--   'GameState' record when starting a scenario.
data GameStateConfig = GameStateConfig
  { initNameParts :: NameGenerator
  , initEntities :: EntityMap
  , initRecipes :: [Recipe Entity]
  , initWorldMap :: WorldMap
  }
