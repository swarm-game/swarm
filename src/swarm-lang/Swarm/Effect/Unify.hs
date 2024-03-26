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
import Control.Effect.State (State, get, modify)
import Control.Effect.Throw (Throw)
import Control.Monad.Trans (MonadIO)
import Data.Kind (Type)
import Swarm.Language.Typecheck.Unify (UnificationError, unify)
import Swarm.Language.Typecheck.Unify.Subst (Subst, subst, (@@))
import Swarm.Language.Types (IntVar, UType)

data Unification (m :: Type -> Type) k where
  Unify :: UType -> UType -> Unification m UType

(=:=) :: Has Unification sig m => UType -> UType -> m UType
t1 =:= t2 = send (Unify t1 t2)

newtype Unifier m a = Unifier {runUnification :: m a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO)

instance
  ( Has (State (Subst IntVar UType)) sig m
  , Has (Throw UnificationError) sig m
  , Algebra sig m
  ) =>
  Algebra (Unification :+: sig) (Unifier m)
  where
  alg hdl sig ctx = case sig of
    L (Unify t1 t2) -> Unifier $ do
      s1 <- get @(Subst IntVar UType)
      let t1' = subst s1 t1
          t2' = subst s1 t2
      s2 <- unify t1' t2'
      modify (@@ s2)
      return $ (subst s2 t1' <$ ctx)
    R other -> Unifier (alg (runUnification . hdl) other ctx)
