-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Evaluation for the Swarm world description DSL.
module Swarm.Game.World.Eval where

import Control.Carrier.Reader (runReader)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Swarm.Game.Entity (Entity, EntityMap)
import Swarm.Game.Terrain (TerrainType (..))
import Swarm.Game.World (WorldFun (..))
import Swarm.Game.World.Compile
import Swarm.Game.World.Coords (Coords)
import Swarm.Game.World.Syntax
import Swarm.Game.World.Typecheck
import Swarm.Game.WorldGen (Seed)

runWExp :: EntityMap -> WExp -> Seed -> Either CheckErr (WorldFun TerrainType Entity)
runWExp em wexp seed = convert . runCTerm . compile seed . bracket <$> runReader em (check CNil (TTyWorld TTyCell) wexp)

convert :: (Coords -> CellVal) -> WorldFun TerrainType Entity
convert f = WF ((\(CellVal (Last t) (Last e) _) -> (fromMaybe BlankT t, e)) . f)