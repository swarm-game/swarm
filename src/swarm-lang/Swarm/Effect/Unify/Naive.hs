{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Description: Naive (slow) substitution-based implementation of
-- unification.  Uses a simple but expensive-to-maintain invariant on
-- substitutions, and returns a substitution from unification which
-- must then be composed with the substitution being tracked.
--
-- Not used in Swarm, and also unmaintained
-- (e.g. "Swarm.Effect.Unify.Fast" now supports expanding type
-- aliases + recursive types; this module does not). It's still here just for
-- testing/comparison.
module Swarm.Effect.Unify.Naive where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.State.Strict (StateC, evalState)
import Control.Carrier.Throw.Either (ThrowC, runThrow)
import Control.Category ((>>>))
import Control.Effect.State (get, gets, modify)
import Control.Effect.Throw (Throw, throwError)
import Control.Monad (zipWithM)
import Control.Monad.Free
import Control.Monad.Trans (MonadIO)
import Data.Function (on)
import Data.Map ((!?))
import Data.Map qualified as M
import Data.Map.Merge.Lazy qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Swarm.Effect.Unify
import Swarm.Effect.Unify.Common
import Swarm.Language.Types hiding (Type)

------------------------------------------------------------
-- Substitutions

-- | Class of things supporting substitution.  @Substitutes n b a@ means
--   that we can apply a substitution of type @Subst n b@ to a
--   value of type @a@, replacing all the free names of type @n@
--   inside the @a@ with values of type @b@, resulting in a new value
--   of type @a@.
class Substitutes n b a where
  subst :: Subst n b -> a -> a

-- | We can perform substitution on terms built up as the free monad
--   over a structure functor @f@.
instance (Show n, Ord n, Functor f) => Substitutes n (Free f n) (Free f n) where
  subst s f = f >>= \n -> fromMaybe (Pure n) (getSubst s !? n)

-- | Compose two substitutions.  Applying @s1 \@\@ s2@ is the same as
--   applying first @s2@, then @s1@; that is, semantically,
--   composition of substitutions corresponds exactly to function
--   composition when they are considered as functions on terms.
--
--   As one would expect, composition is associative and has 'idS' as
--   its identity.
(@@) :: (Ord n, Substitutes n a a) => Subst n a -> Subst n a -> Subst n a
(Subst s1) @@ (Subst s2) = Subst (M.map (subst (Subst s1)) s2 `M.union` s1)

-- | Compose a whole container of substitutions.  For example,
--   @compose [s1, s2, s3] = s1 \@\@ s2 \@\@ s3@.
compose :: (Ord n, Substitutes n a a, Foldable t) => t (Subst n a) -> Subst n a
compose = foldr (@@) idS

------------------------------------------------------------
-- Carrier type

-- Note: this carrier type and the runUnification function are
-- identical between this module and Swarm.Effect.Unify.Fast, but it
-- seemed best to duplicate it, so we can modify the carriers
-- independently in the future if we want.

-- | Carrier type for unification: we maintain a current substitution,
--   a counter for generating fresh unification variables, and can
--   throw unification errors.
newtype UnificationC m a = UnificationC
  { unUnificationC ::
      StateC (Subst IntVar UType) (StateC FreshVarCounter (ThrowC UnificationError m)) a
  }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO)

-- | Counter for generating fresh unification variables.
newtype FreshVarCounter = FreshVarCounter {getFreshVarCounter :: Int}
  deriving (Eq, Ord, Enum)

-- | Run a 'Unification' effect via the 'UnificationC' carrier.
runUnification :: Algebra sig m => UnificationC m a -> m (Either UnificationError a)
runUnification =
  unUnificationC >>> evalState idS >>> evalState (FreshVarCounter 0) >>> runThrow

------------------------------------------------------------
-- Unification

-- | Naive implementation of the 'Unification' effect in terms of the
--   'UnificationC' carrier.
--
--   We maintain an invariant on the current @Subst@ that map keys
--   never show up in any of the values.  For example, we could have
--   @{x -> a+5, y -> 5}@ but not @{x -> a+y, y -> 5}@.
instance Algebra sig m => Algebra (Unification :+: sig) (UnificationC m) where
  alg hdl sig ctx = UnificationC $ case sig of
    L (Unify t1 t2) -> do
      s1 <- get @(Subst IntVar UType)
      let t1' = subst s1 t1
          t2' = subst s1 t2
      s2 <- unify t1' t2'
      modify (s2 @@)
      return $ Right (subst s2 t1') <$ ctx
    L (ApplyBindings t) -> do
      s <- get @(Subst IntVar UType)
      return $ subst s t <$ ctx
    L FreshIntVar -> do
      v <- IntVar <$> gets getFreshVarCounter
      modify @FreshVarCounter succ
      return $ v <$ ctx
    L (FreeUVars t) -> do
      s <- get @(Subst IntVar UType)
      return $ fuvs (subst s t) <$ ctx
    R other -> alg (unUnificationC . hdl) (R (R (R other))) ctx

-- | Unify two types and return the mgu, i.e. the smallest
--   substitution which makes them equal.
unify ::
  Has (Throw UnificationError) sig m =>
  UType ->
  UType ->
  m (Subst IntVar UType)
unify ty1 ty2 = case (ty1, ty2) of
  (Pure x, Pure y)
    | x == y -> return idS
    | otherwise -> return $ x |-> Pure y
  (Pure x, y)
    | x `S.member` fuvs y -> throwError $ Infinite x y
    | otherwise -> return $ x |-> y
  (y, Pure x)
    | x `S.member` fuvs y -> throwError $ Infinite x y
    | otherwise -> return $ x |-> y
  (Free t1, Free t2) -> unifyF t1 t2

-- | Unify two non-variable terms and return an mgu, i.e. the smallest
--   substitution which makes them equal.
unifyF ::
  Has (Throw UnificationError) sig m =>
  TypeF UType ->
  TypeF UType ->
  m (Subst IntVar UType)
unifyF t1 t2 = case (t1, t2) of
  (TyConF c1 ts1, TyConF c2 ts2) -> case c1 == c2 of
    True -> compose <$> zipWithM unify ts1 ts2
    False -> unifyErr
  (TyConF {}, _) -> unifyErr
  (TyVarF v1, TyVarF v2) -> case v1 == v2 of
    True -> return idS
    False -> unifyErr
  (TyVarF {}, _) -> unifyErr
  (TyRcdF m1, TyRcdF m2) ->
    case ((==) `on` M.keysSet) m1 m2 of
      False -> unifyErr
      _ -> (fmap compose . sequence) (M.merge M.dropMissing M.dropMissing (M.zipWithMatched (const unify)) m1 m2)
  (TyRcdF {}, _) -> unifyErr
  -- Don't support any extra features (e.g. recursive types), so just
  -- add a catch-all failure case
  (_, _) -> unifyErr
 where
  unifyErr = throwError $ UnifyErr t1 t2
