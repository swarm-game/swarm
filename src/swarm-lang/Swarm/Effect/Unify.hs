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
import Control.Effect.State (State)
import Control.Monad.Free (Free)
import Control.Monad.Trans (MonadIO)
import Data.Kind (Type)
import Swarm.Language.Typecheck.Unify.Subst (Subst)

class Unifiable t where
  zipMatch :: t a -> t a -> Maybe (t (Either a (a, a)))

data Unification t v (m :: Type -> Type) k where
  Unify :: Unifiable t => Free t v -> Free t v -> Unification t v m (Free t v)

unify :: (Has (Unification t v) sig m, Unifiable t) => Free t v -> Free t v -> m (Free t v)
unify t1 t2 = send (Unify t1 t2)

newtype Unifier (t :: Type -> Type) v m a = Unifier {runUnifier :: m a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO)

instance
  ( Has (State (Subst v (Free t v))) sig m
  , Algebra sig m
  ) =>
  Algebra (Unification t v :+: sig) (Unifier t v m)
  where
  alg hdl sig ctx = case sig of
    L (Unify t1 t2) -> undefined
    R other -> Unifier (alg (runUnifier . hdl) other ctx)
