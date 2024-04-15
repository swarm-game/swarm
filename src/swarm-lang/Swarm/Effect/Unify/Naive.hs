{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Description: Naive (slow) substitution-based implementation of
-- unification.
module Swarm.Effect.Unify.Naive where

import Swarm.Effect.Unify

newtype UnificationC m a = UnificationC
  {unUnificationC :: StateC (Subst IntVar UType) (StateC FreshVarCounter (ThrowC UnificationError m)) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO)

newtype FreshVarCounter = FreshVarCounter {getFreshVarCounter :: Int}
  deriving (Eq, Ord, Enum)

instance Algebra sig m => Algebra (Unification :+: sig) (UnificationC m) where
  alg hdl sig ctx = UnificationC $ case sig of
    L (Unify t1 t2) -> do
      s1 <- get @(Subst IntVar UType)
      let t1' = subst s1 t1
          t2' = subst s1 t2
      s2 <- unify t1' t2'
      modify (s2 @@)
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
    R other -> alg (unUnificationC . hdl) (R (R (R other))) ctx

runUnification :: Algebra sig m => UnificationC m a -> m (Either UnificationError a)
runUnification = unUnificationC >>> evalState idS >>> evalState (FreshVarCounter 0) >>> runThrow
<
