{-# LANGUAGE DataKinds #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Evaluation for the Swarm world description DSL.
module Swarm.Game.World.Eval (
  runWorld,
) where

import Swarm.Game.Entity (Entity)
import Swarm.Game.Terrain (TerrainType (..))
import Swarm.Game.World (WorldFun (..))
import Swarm.Game.World.Abstract (bracket)
import Swarm.Game.World.Coords (Coords)
import Swarm.Game.World.Gen (Seed)
import Swarm.Game.World.Interpret (interpBTerm)
import Swarm.Game.World.Syntax
import Swarm.Game.World.Typecheck

-- | Run a typechecked world description DSL term to produce a
--   'WorldFun'.
runWorld :: TTerm '[] (World CellVal) -> Seed -> WorldFun TerrainType Entity
runWorld t seed = convertWF . interpBTerm seed . bracket $ t

-- Currently we run a DSL term by performing bracket abstraction,
-- producing a 'BTerm', then directly interpreting the 'BTerm' with
-- 'interpBTerm'.  We could also compile the 'BTerm' to a 'CTerm' and
-- run it, i.e.
--
--   convertWF . runCTerm . compile seed . bracket $ t
--
-- which can supposedly give a performance boost, but it is unclear
-- whether this actually makes a difference in our case.

-- | Simple adapter function to convert a plain @Coords -> CellVal@
--   function into a 'WorldFun' value.
convertWF :: (Coords -> CellVal) -> WorldFun TerrainType Entity
convertWF f = WF ((\(CellVal t e _) -> (t, e)) . f)
