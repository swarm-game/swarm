{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

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

-- | XXX
(=:=) :: Has Unification sig m => UType -> UType -> m UType
t1 =:= t2 = send (Unify t1 t2)

-- | XXX
applyBindings :: Has Unification sig m => UType -> m UType
applyBindings = send . ApplyBindings

-- | XXX
freeUVars :: Has Unification sig m => UType -> m (Set IntVar)
freeUVars = send . FreeUVars

-- | XXX
freshIntVar :: Has Unification sig m => m IntVar
freshIntVar = send FreshIntVar

-- | XXX
data UnificationError
  = Infinite IntVar UType
  | -- | ^ Occurs check failure
    UnifyErr
      (TypeF UType)
      -- | Mismatch
      (TypeF UType)
  deriving (Show)
