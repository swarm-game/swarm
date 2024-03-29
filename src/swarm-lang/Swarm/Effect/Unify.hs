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
import Control.Applicative (Alternative)
import Control.Effect.State (State, get, gets, modify)
import Control.Effect.Throw (Throw)
import Control.Monad.Trans (MonadIO)
import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as S
import Swarm.Language.Typecheck.Unify (UnificationError, unify, fvs)
import Swarm.Language.Typecheck.Unify.Subst (Subst, subst, (@@))
import Swarm.Language.Types (IntVar (..), UType)

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

newtype UnificationC m a = UnificationC {runUnification :: m a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO)

newtype FreshVarCounter = FreshVarCounter {getFreshVarCounter :: Int}
  deriving (Eq, Ord, Enum)

instance
  ( Has (State (Subst IntVar UType)) sig m
  , Has (State FreshVarCounter) sig m
  , Has (Throw UnificationError) sig m
  , Algebra sig m
  ) =>
  Algebra (Unification :+: sig) (UnificationC m)
  where
  alg hdl sig ctx = UnificationC $ case sig of
    L (Unify t1 t2) -> do
      s1 <- get @(Subst IntVar UType)
      let t1' = subst s1 t1
          t2' = subst s1 t2
      s2 <- unify t1' t2'
      modify (@@ s2)
      return $ (subst s2 t1' <$ ctx)
    L (ApplyBindings t) -> do
      s <- get @(Subst IntVar UType)
      return $ subst s t <$ ctx
    L FreshIntVar -> do
      v <- IntVar <$> gets getFreshVarCounter
      modify @FreshVarCounter succ
      return $ v <$ ctx
    L (FreeUVars t) -> do
      s <- get @(Subst IntVar UType)
      return $ fvs (subst s t) <$ ctx

    R other -> alg (runUnification . hdl) other ctx
