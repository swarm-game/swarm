{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Description: Fast yet purely functional implementation of
-- unification, using a map as a lazy substitution, i.e. a
-- manually-maintained "functional shared memory".
--
-- See Dijkstra, Middelkoop, & Swierstra, "Efficient Functional
-- Unification and Substitution", Utrecht University tech report
-- UU-CS-2008-027 (section 5) for the basic idea, and Peyton Jones et
-- al, "Practical type inference for arbitrary-rank types"
-- (pp. 74--75) for a correct implementation of unification via
-- references.
module Swarm.Effect.Unify.Fast where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.State.Strict (StateC, evalState)
import Control.Carrier.Throw.Either (ThrowC, runThrow)
import Control.Category ((>>>))
import Control.Effect.State (State, get, gets, modify)
import Control.Effect.Throw (Throw, throwError)
import Control.Monad.Free
import Control.Monad.Trans (MonadIO)
import Data.Function (on)
import Data.Map qualified as M
import Data.Map.Merge.Lazy qualified as M
import Data.Set qualified as S
import Swarm.Effect.Unify
import Swarm.Effect.Unify.Common
import Swarm.Language.Types hiding (Type)
import Prelude hiding (lookup)

------------------------------------------------------------
-- Substitutions

-- | Compose two substitutions.  Applying @s1 \@\@ s2@ is the same as
--   applying first @s2@, then @s1@; that is, semantically,
--   composition of substitutions corresponds exactly to function
--   composition when they are considered as functions on terms.
--
--   As one would expect, composition is associative and has 'idS' as
--   its identity.
--
--   Note that we do /not/ apply @s1@ to all the values in @s2@, since
--   the substitution is maintained lazily; we do not need to maintain
--   the invariant that values in the mapping do not contain any of
--   the keys.  This makes composition much faster, at the cost of
--   making application more complex.
(@@) :: (Ord n, Substitutes n a a) => Subst n a -> Subst n a -> Subst n a
(Subst s1) @@ (Subst s2) = Subst (s2 `M.union` s1)

-- | Class of things supporting substitution.  @Substitutes n b a@ means
--   that we can apply a substitution of type @Subst n b@ to a
--   value of type @a@, replacing all the free names of type @n@
--   inside the @a@ with values of type @b@, resulting in a new value
--   of type @a@.
--
--   We also do a lazy occurs-check during substitution application,
--   so we need the ability to throw a unification error.
class Substitutes n b a where
  subst :: Has (Throw UnificationError) sig m => Subst n b -> a -> m a

-- | We can perform substitution on terms built up as the free monad
--   over a structure functor @f@.
instance Substitutes IntVar UType UType where
  subst s = go S.empty
   where
    go seen (Pure x) = case lookup x s of
      Nothing -> pure $ Pure x
      Just t
        | S.member x seen -> throwError $ Infinite x t
        | otherwise -> go (S.insert x seen) t
    go seen (Free t) = Free <$> goF seen t

    goF _ t@(TyBaseF {}) = pure t
    goF _ t@(TyVarF {}) = pure t
    goF seen (TySumF t1 t2) = TySumF <$> go seen t1 <*> go seen t2
    goF seen (TyProdF t1 t2) = TyProdF <$> go seen t1 <*> go seen t2
    goF seen (TyRcdF m) = TyRcdF <$> mapM (go seen) m
    goF seen (TyCmdF c) = TyCmdF <$> go seen c
    goF seen (TyDelayF c) = TyDelayF <$> go seen c
    goF seen (TyFunF t1 t2) = TyFunF <$> go seen t1 <*> go seen t2

------------------------------------------------------------
-- Carrier type

-- Note: this carrier type and the runUnification function are
-- identical between this module and Swarm.Effect.Unify.Naive, but it
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

-- The idea here (using an explicit substitution as a sort of
-- "functional shared memory", instead of directly using IORefs), is
-- based on Dijkstra et al. Unfortunately, their implementation of
-- unification is subtly wrong; fortunately, a single integration test
-- in the Swarm test suite failed, leading to discovering the bug.
-- The basic issue is that when unifying an equation between two
-- variables @x = y@, we must look up *both* to see whether they are
-- already mapped by the substitution (and if so, replace them by
-- their referent and keep recursing).  Dijkstra et al. only look up
-- @x@ and simply map @x |-> y@ if x is not in the substitution, but
-- this can lead to cycles where e.g. x is mapped to y, and later we
-- unify @y = x@ resulting in both @x |-> y@ and @y |-> x@ in the
-- substitution, which at best leads to a spurious infinite type
-- error, and at worst leads to infinite recursion in the unify function.
--
-- Peyton Jones et al. show how to do it correctly: when unifying x = y and
-- x is not mapped in the substitution, we must also look up y.

-- | Implementation of the 'Unification' effect in terms of the
--   'UnificationC' carrier.
instance Algebra sig m => Algebra (Unification :+: sig) (UnificationC m) where
  alg hdl sig ctx = UnificationC $ case sig of
    L (Unify t1 t2) -> (<$ ctx) <$> runThrow (unify t1 t2)
    L (ApplyBindings t) -> do
      s <- get @(Subst IntVar UType)
      (<$ ctx) <$> subst s t
    L FreshIntVar -> do
      v <- IntVar <$> gets getFreshVarCounter
      modify @FreshVarCounter succ
      return $ v <$ ctx
    L (FreeUVars t) -> do
      s <- get @(Subst IntVar UType)
      (<$ ctx) . fuvs <$> subst s t
    R other -> alg (unUnificationC . hdl) (R (R (R other))) ctx

-- | Unify two types, returning a unified type equal to both.  Note
--   that for efficiency we /don't/ do an occurs check here, but
--   instead lazily during substitution.
unify ::
  ( Has (Throw UnificationError) sig m
  , Has (State (Subst IntVar UType)) sig m
  ) =>
  UType ->
  UType ->
  m UType
unify ty1 ty2 = case (ty1, ty2) of
  (Pure x, Pure y) | x == y -> pure (Pure x)
  (Pure x, y) -> do
    mxv <- lookupS x
    case mxv of
      Nothing -> unifyVar x y
      Just xv -> unify xv y
  (x, Pure y) -> unify (Pure y) x
  (Free t1, Free t2) -> Free <$> unifyF t1 t2

-- | Unify a unification variable which /is not/ bound by the current
--   substitution with another term.  If the other term is also a
--   variable, we must look it up as well to see if it is bound.
unifyVar ::
  ( Has (Throw UnificationError) sig m
  , Has (State (Subst IntVar UType)) sig m
  ) =>
  IntVar ->
  UType ->
  m UType
unifyVar x (Pure y) = do
  myv <- lookupS y
  case myv of
    -- x = y but the variable y is not bound: just add (x |-> y) to
    -- the current Subst
    --
    -- Note, as an optimization we just call e.g. insert x (Pure y)
    -- instead of building a singleton Subst with @(|->)@ and then
    -- composing, since composition doesn't need to apply the newly
    -- created binding to all the other values bound in the Subst.
    Nothing -> modify @(Subst IntVar UType) (insert x (Pure y)) >> pure (Pure y)
    -- x = y  and y is bound to v: recurse on x = v.
    Just yv -> unify (Pure x) yv

-- x = t for a non-variable t: just add (x |-> t) to the Subst.
unifyVar x t = modify (insert x t) >> pure t

-- | Perform unification on two non-variable terms: check that they
--   have the same top-level constructor and recurse on their
--   contents.
unifyF ::
  ( Has (Throw UnificationError) sig m
  , Has (State (Subst IntVar UType)) sig m
  ) =>
  TypeF UType ->
  TypeF UType ->
  m (TypeF UType)
unifyF t1 t2 = case (t1, t2) of
  (TyBaseF b1, TyBaseF b2) -> case b1 == b2 of
    True -> pure t1
    False -> unifyErr
  (TyBaseF {}, _) -> unifyErr
  -- Note that *type variables* are not the same as *unification variables*.
  -- Type variables must match exactly.
  (TyVarF v1, TyVarF v2) -> case v1 == v2 of
    True -> pure t1
    False -> unifyErr
  (TyVarF {}, _) -> unifyErr
  (TySumF t11 t12, TySumF t21 t22) -> TySumF <$> unify t11 t21 <*> unify t12 t22
  (TySumF {}, _) -> unifyErr
  (TyProdF t11 t12, TyProdF t21 t22) -> TyProdF <$> unify t11 t21 <*> unify t12 t22
  (TyProdF {}, _) -> unifyErr
  (TyRcdF m1, TyRcdF m2) ->
    case ((==) `on` M.keysSet) m1 m2 of
      False -> unifyErr
      _ -> fmap TyRcdF . sequence $ M.merge M.dropMissing M.dropMissing (M.zipWithMatched (const unify)) m1 m2
  (TyRcdF {}, _) -> unifyErr
  (TyCmdF c1, TyCmdF c2) -> TyCmdF <$> unify c1 c2
  (TyCmdF {}, _) -> unifyErr
  (TyDelayF c1, TyDelayF c2) -> TyDelayF <$> unify c1 c2
  (TyDelayF {}, _) -> unifyErr
  (TyFunF t11 t12, TyFunF t21 t22) -> TyFunF <$> unify t11 t21 <*> unify t12 t22
  (TyFunF {}, _) -> unifyErr
 where
  unifyErr = throwError $ UnifyErr t1 t2
