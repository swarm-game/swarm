{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Unification effects
module Swarm.Effect.Unify where

import Control.Algebra
import Data.Kind (Type)
import Data.Set (Set)
import Swarm.Language.Types hiding (Type)

data Unification (m :: Type -> Type) k where
  Unify :: UType -> UType -> Unification m UType
  ApplyBindings :: UType -> Unification m UType
  FreshIntVar :: Unification m IntVar
  FreeUVars :: UType -> Unification m (Set IntVar)

-- | Unify two types, returning a type equal to both.
(=:=) :: Has Unification sig m => UType -> UType -> m UType
t1 =:= t2 = send (Unify t1 t2)

-- | Substitute for as many unification variables as are currently
--   bound.  It is guaranteed that we currently have no information
--   about any unification variables remaining in the result.
applyBindings :: Has Unification sig m => UType -> m UType
applyBindings = send . ApplyBindings

-- | Compute the set of free unification variables of a type (after
--   substituting away any which are already bound).
freeUVars :: Has Unification sig m => UType -> m (Set IntVar)
freeUVars = send . FreeUVars

-- | Generate a fresh unification variable.
freshIntVar :: Has Unification sig m => m IntVar
freshIntVar = send FreshIntVar

-- | An error that occurred while running the unifier.
data UnificationError where
  -- | Occurs check failure, i.e. the solution to some unification
  --   equations was an infinite term.
  Infinite :: IntVar -> UType -> UnificationError
  -- | Mismatch error between the given terms.
  UnifyErr :: TypeF UType -> TypeF UType -> UnificationError
  deriving (Show)
