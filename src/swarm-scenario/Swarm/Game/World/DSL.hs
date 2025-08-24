{-# LANGUAGE PatternSynonyms #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Grid on which the game takes place
--
-- DSL for programming worlds.
module Swarm.Game.World.DSL (
  -- ** Syntax
  World,
  Seed,
  CellVal,
  TTerm (..),

  -- ** Typechecking
  check,
  WorldMap,
  CheckErr (..),
  Ctx (..),
  TTy (..),
  pattern TTyBool,
  pattern TTyInt,
  pattern TTyFloat,
  pattern TTyCell,

  -- ** World function conversion
  runWorld,
  extractEntities,

  -- ** Loading files
  loadWorld,
  loadWorlds,
) where

import Swarm.Game.World.DSL.Eval (runWorld)
import Swarm.Game.World.DSL.Gen (Seed, extractEntities)
import Swarm.Game.World.DSL.Load (loadWorld, loadWorlds)
import Swarm.Game.World.DSL.Syntax (CellVal, World)
import Swarm.Game.World.DSL.Typecheck (
  CheckErr (..),
  Ctx (..),
  TTerm (..),
  TTy (..),
  WorldMap,
  check,
  pattern TTyBool,
  pattern TTyCell,
  pattern TTyFloat,
  pattern TTyInt,
 )
