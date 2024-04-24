{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Description: Fast yet purely functional implementation of
-- unification, using a map as a lazy substitution, i.e. a
-- manually-mainted "functional shared memory".
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
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Map.Merge.Lazy qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Prelude hiding (lookup)
import Swarm.Effect.Unify
import Swarm.Language.Types hiding (Type)

------------------------------------------------------------
-- Carrier

-- | Carrier type for unification: we maintain a lazy substitution, a counter for
--   generating fresh unification variables, and can throw unification errors.
newtype UnificationC m a = UnificationC
  { unUnificationC ::
      StateC (Subst IntVar UType) (StateC FreshVarCounter (ThrowC UnificationError m)) a
  }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO)

-- | Counter for generating fresh unification variables.
newtype FreshVarCounter = FreshVarCounter {getFreshVarCounter :: Int}
  deriving (Eq, Ord, Enum)

-- | Implementation of the 'Unification' effect in terms of the
--   'UnificationC' carrier.
instance Algebra sig m => Algebra (Unification :+: sig) (UnificationC m) where
  alg hdl sig ctx = UnificationC $ case sig of
    L (Unify t1 t2) -> (<$ ctx) <$> unify t1 t2
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

-- | Run a 'Unification' effect via the 'UnificationC' carrier
runUnification :: Algebra sig m => UnificationC m a -> m (Either UnificationError a)
runUnification = unUnificationC >>> evalState idS >>> evalState (FreshVarCounter 0) >>> runThrow

-- | A value of type @Subst n a@ is a substitution which maps
--   names of type @n@ (the /domain/, see 'dom') to values of type
--   @a@.  Substitutions can be /applied/ to certain terms (see
--   'subst'), replacing any free occurrences of names in the
--   domain with their corresponding values.  Thus, substitutions can
--   be thought of as functions of type @Term -> Term@ (for suitable
--   @Term@s that contain names and values of the right type).
--
--   Concretely, substitutions are stored using a @Map@.
--
--   NOTE, in this version (as compared to 'Swarm.Effect.Unify.Naive')
--   a substitution is represented lazily, that is, if we look up a
--   variable name in the map, the value we get might still contain
--   variables which are keys in the substitution, which we might have
--   to look up recursively.  This makes applying the substitution
--   more complex but is generally much faster because we don't have
--   to spend time traversing terms looking for variables to
--   substitute away --- we just leave them be, and look them up
--   lazily as needed.
newtype Subst n a = Subst {getSubst :: Map n a}
  deriving (Eq, Ord, Show)

  -- Note, tried using an IntMap instead of a Map but it was actually slower.

instance Functor (Subst n) where
  fmap f (Subst m) = Subst (M.map f $ m)

-- | The domain of a substitution is the set of names for which the
--   substitution is defined.
dom :: Subst n a -> Set n
dom = M.keysSet . getSubst

-- | The identity substitution, /i.e./ the unique substitution with an
--   empty domain, which acts as the identity function on terms.
idS :: Subst n a
idS = Subst M.empty

-- | Construct a singleton substitution, which maps the given name to
--   the given value.
(|->) :: n -> a -> Subst n a
x |-> t = Subst (M.singleton x t)

-- | Class of things supporting substitution.  @Substitutes n b a@ means
--   that we can apply a substitution of type @Subst n b@ to a
--   value of type @a@, replacing all the free names of type @n@
--   inside the @a@ with values of type @b@, resulting in a new value
--   of type @a@.
class Substitutes n b a where
  subst :: Has (Throw UnificationError) sig m => Subst n b -> a -> m a

-- | We can perform substitution on terms built up as the free monad
--   over a structure functor @f@.
--
-- XXX explain the extra complication here.
instance Substitutes IntVar UType UType where
  subst s f = go S.empty f
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

-- | Compose two substitutions.  Applying @s1 \@\@ s2@ is the same as
--   applying first @s2@, then @s1@; that is, semantically,
--   composition of substitutions corresponds exactly to function
--   composition when they are considered as functions on terms.
--
--   As one would expect, composition is associative and has 'idS' as
--   its identity.
(@@) :: (Ord n, Substitutes n a a) => Subst n a -> Subst n a -> Subst n a
(Subst s1) @@ (Subst s2) = Subst (s2 `M.union` s1)
  -- In this version, we do not apply s1 to all the values in s2, since
  -- we do not need to maintain the invariant that values in the mapping
  -- do not contain any of the keys!

-- | Compose a whole container of substitutions.  For example,
--   @compose [s1, s2, s3] = s1 \@\@ s2 \@\@ s3@.
compose :: (Ord n, Substitutes n a a, Foldable t) => t (Subst n a) -> Subst n a
compose = foldr (@@) idS

-- | Create a substitution from an association list of names and
--   values.
fromList :: Ord n => [(n, a)] -> Subst n a
fromList = Subst . M.fromList

-- | Convert a substitution into an association list.
toList :: Subst n a -> [(n, a)]
toList = M.assocs . getSubst

-- | Look up the value a particular name maps to under the given
--   substitution; or return @Nothing@ if the name being looked up is
--   not in the domain.
lookup :: Ord n => n -> Subst n a -> Maybe a
lookup x (Subst m) = M.lookup x m

-- | Look up a name in a substitution stored in a state effect.
lookupS :: (Ord n, Has (State (Subst n a)) sig m) => n -> m (Maybe a)
lookupS x = lookup x <$> get

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

-- | XXX note we don't do occurs check here
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

unifyVar ::
  ( Has (Throw UnificationError) sig m
  , Has (State (Subst IntVar UType)) sig m
  ) =>
  IntVar -> UType -> m UType
unifyVar x (Pure y) = do
  myv <- lookupS y
  case myv of
    Nothing -> modify @(Subst IntVar UType) ((x |-> Pure y) @@) >> pure (Pure y)
    Just yv -> unify (Pure x) yv
unifyVar x y = modify ((x |-> y) @@) >> pure y

-- | XXX
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
