{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Description: Naive (slow) substitution-based implementation of
-- unification.
module Swarm.Effect.Unify.Naive where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.State.Strict (StateC, evalState)
import Control.Carrier.Throw.Either (ThrowC, runThrow)
import Control.Category ((>>>))
import Control.Effect.State (get, gets, modify)
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
import Swarm.Effect.Unify
import Swarm.Language.Types hiding (Type)

------------------------------------------------------------
-- Naive carrier

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
      return $ fuvs (subst s t) <$ ctx
    R other -> alg (unUnificationC . hdl) (R (R (R other))) ctx

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
newtype Subst n a = Subst {getSubst :: Map n a}
  deriving (Eq, Ord, Show)

instance Functor (Subst n) where
  fmap f (Subst m) = Subst (M.map f $ m)

-- instance Pretty a => Pretty (Subst a) where
--   pretty (Subst s) = do
--     let es = map (uncurry prettyMapping) (M.assocs s)
--     ds <- punctuate "," es
--     braces (hsep ds)

-- prettyMapping :: (Pretty a, Members '[Reader PA, LFresh] r) => Name a -> a -> Sem r (Doc ann)
-- prettyMapping x a = pretty x <+> "->" <+> pretty a

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
(Subst s1) @@ (Subst s2) = Subst ((M.map (subst (Subst s1))) s2 `M.union` s1)

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

-- XXX rewrite using Montelli & Montanari approach,
-- https://en.wikipedia.org/wiki/Unification_(computer_science)#Unification_algorithms
-- Probably faster and will also be important for dealing with unifying recursive types.

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

unifyF ::
  Has (Throw UnificationError) sig m =>
  TypeF UType ->
  TypeF UType ->
  m (Subst IntVar UType)
unifyF t1 t2 = case (t1, t2) of
  (TyBaseF b1, TyBaseF b2) -> case b1 == b2 of
    True -> return idS
    False -> unifyErr
  (TyBaseF {}, _) -> unifyErr
  (TyVarF v1, TyVarF v2) -> case v1 == v2 of
    True -> return idS
    False -> unifyErr
  (TyVarF {}, _) -> unifyErr
  (TySumF t11 t12, TySumF t21 t22) -> do
    s1 <- unify t11 t21
    s2 <- unify t12 t22
    return $ s1 @@ s2
  (TySumF {}, _) -> unifyErr
  (TyProdF t11 t12, TyProdF t21 t22) -> do
    s1 <- unify t11 t21
    s2 <- unify t12 t22
    return $ s1 @@ s2
  (TyProdF {}, _) -> unifyErr
  (TyRcdF m1, TyRcdF m2) ->
    case ((==) `on` M.keysSet) m1 m2 of
      False -> unifyErr
      _ -> (fmap compose . sequence) (M.merge M.dropMissing M.dropMissing (M.zipWithMatched (const unify)) m1 m2)
  (TyRcdF {}, _) -> unifyErr
  (TyCmdF c1, TyCmdF c2) -> unify c1 c2
  (TyCmdF {}, _) -> unifyErr
  (TyDelayF c1, TyDelayF c2) -> unify c1 c2
  (TyDelayF {}, _) -> unifyErr
  (TyFunF t11 t12, TyFunF t21 t22) -> do
    s1 <- unify t11 t21
    s2 <- unify t12 t22
    return $ s1 @@ s2
  (TyFunF {}, _) -> unifyErr
 where
  unifyErr = throwError $ UnifyErr t1 t2
