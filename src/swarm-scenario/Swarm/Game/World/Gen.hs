{-# LANGUAGE GADTs #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for working with procedurally generated worlds.
module Swarm.Game.World.Gen where

import Data.Semigroup (Last (..))
import Data.Set qualified as S
import Swarm.Game.Entity
import Swarm.Game.World.Syntax (CellVal (..))
import Swarm.Game.World.Typecheck (Const (CCell), TTerm (..))
import Swarm.Util.Erasable

type Seed = Int

-- | Extract a list of all entities mentioned in a given world DSL term.
extractEntities :: TTerm g a -> S.Set Entity
extractEntities (TLam t) = extractEntities t
extractEntities (TApp t1 t2) = extractEntities t1 <> extractEntities t2
extractEntities (TConst (CCell (CellVal _ ee _))) = getEntity ee
 where
  getEntity (EJust (Last e)) = S.singleton e
  getEntity _ = S.empty
extractEntities _ = S.empty
