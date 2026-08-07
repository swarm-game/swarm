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

import Control.Monad (zipWithM)
import Control.Monad.Free
import Data.Function (on)
import Data.Map ((!?))
import Data.Map qualified as M
import Data.Map.Merge.Lazy qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.State.Static.Local
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

-- | Counter for generating fresh unification variables.
newtype FreshVarCounter = FreshVarCounter {getFreshVarCounter :: Int}
  deriving (Eq, Ord, Enum)

-- | Naive handler of the 'Unification' effect.
--
--   We maintain an invariant on the current @Subst@ that map keys
--   never show up in any of the values.  For example, we could have
--   @{x -> a+5, y -> 5}@ but not @{x -> a+y, y -> 5}@.
runUnification :: Eff (Unification : es) a -> Eff es (Either UnificationError a)
runUnification = reinterpret (runErrorNoCallStack . evalState (FreshVarCounter 0) . evalState (idS :: Subst IntVar UType)) $ \_ -> \case
  Unify t1 t2 -> do
    s1 <- get @(Subst IntVar UType)
    let t1' = subst s1 t1
        t2' = subst s1 t2
    s2 <- unify t1' t2'
    modify (s2 @@)
    pure $ Right (subst s2 t1')
  ApplyBindings t -> do
    s <- get @(Subst IntVar UType)
    pure $ subst s t
  FreshIntVar -> do
    v <- IntVar <$> gets getFreshVarCounter
    modify @FreshVarCounter succ
    pure v
  FreeUVars t -> do
    s <- get @(Subst IntVar UType)
    pure $ fuvs (subst s t)

-- | Unify two types and return the mgu, i.e. the smallest
--   substitution which makes them equal.
unify ::
  Error UnificationError :> es =>
  UType ->
  UType ->
  Eff es (Subst IntVar UType)
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
  Error UnificationError :> es =>
  TypeF UType ->
  TypeF UType ->
  Eff es (Subst IntVar UType)
unifyF t1 t2 = case (t1, t2) of
  (TyConF c1 ts1, TyConF c2 ts2) -> case c1 == c2 of
    True -> compose <$> zipWithM unify ts1 ts2
    False -> unifyErr
  (TyConF {}, _) -> unifyErr
  (TyVarF _ v1, TyVarF _ v2) -> case v1 == v2 of
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
