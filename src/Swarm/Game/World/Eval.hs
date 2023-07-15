-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Evaluation for the Swarm world description DSL.
module Swarm.Game.World.Eval where

import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Swarm.Game.Entity (Entity)
import Swarm.Game.Terrain (TerrainType (..))
import Swarm.Game.World (WorldFun (..))
import Swarm.Game.World.Compile
import Swarm.Game.World.Coords (Coords)
import Swarm.Game.World.Syntax
import Swarm.Game.World.Typecheck
import Swarm.Game.WorldGen (Seed)

runWExp :: WExp -> Seed -> Maybe (WorldFun TerrainType Entity)
runWExp wexp seed = convert . runCTerm . compile . bracket <$> check CNil (TTyWorld TTyCell) wexp

convert :: (Coords -> FilledCellVal) -> WorldFun TerrainType Entity
convert f = WF ((\(CellVal (Last t) (Last e) _) -> (fromMaybe BlankT t, e)) . f)
