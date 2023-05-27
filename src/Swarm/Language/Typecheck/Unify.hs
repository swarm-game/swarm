-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities related to type unification.
module Swarm.Language.Typecheck.Unify (
  UnifyStatus (..),
  unifyCheck,
) where

import Control.Unification
import Data.Foldable qualified as F
import Data.Function (on)
import Data.Map qualified as M
import Data.Map.Merge.Lazy qualified as M
import Swarm.Language.Types

data UnifyStatus = Apart | MightUnify | Equal
  deriving (Eq, Ord, Read, Show)

-- | The @Semigroup@ instance for @UnifyStatus@ is used to combine
--   results for compound types.
instance Semigroup UnifyStatus where
  -- If either part of a compound type is apart, then the whole thing is.
  Apart <> _ = Apart
  _ <> Apart = Apart
  -- Otherwise, if we're unsure about either part of a compound type,
  -- then we're unsure about the whole thing.
  MightUnify <> _ = MightUnify
  _ <> MightUnify = MightUnify
  -- Finally, if both parts are definitely equal then the whole thing is.
  Equal <> Equal = Equal

instance Monoid UnifyStatus where
  mempty = Equal

-- | Given two types, try hard to prove either that (1) they are
--   'Apart', i.e. cannot possibly unify, or (2) they are definitely
--   'Equal'.  In case (1), we can generate a much better error
--   message at the instant the two types come together than we could
--   if we threw a constraint into the unifier.  In case (2), we don't
--   have to bother with generating a constraint. If we don't know for
--   sure whether they will unify, return 'MightUnify'.
unifyCheck :: UType -> UType -> UnifyStatus
unifyCheck ty1 ty2 = case (ty1, ty2) of
  (UVar x, UVar y)
    | x == y -> Equal
    | otherwise -> MightUnify
  (UVar _, _) -> MightUnify
  (_, UVar _) -> MightUnify
  (UTerm t1, UTerm t2) -> unifyCheckF t1 t2

unifyCheckF :: TypeF UType -> TypeF UType -> UnifyStatus
unifyCheckF t1 t2 = case (t1, t2) of
  (TyBaseF b1, TyBaseF b2) -> case b1 == b2 of
    True -> Equal
    False -> Apart
  (TyBaseF {}, _) -> Apart
  (TyVarF v1, TyVarF v2) -> case v1 == v2 of
    True -> Equal
    False -> Apart
  (TyVarF {}, _) -> Apart
  (TySumF t11 t12, TySumF t21 t22) -> unifyCheck t11 t21 <> unifyCheck t12 t22
  (TySumF {}, _) -> Apart
  (TyProdF t11 t12, TyProdF t21 t22) -> unifyCheck t11 t21 <> unifyCheck t12 t22
  (TyProdF {}, _) -> Apart
  (TyRcdF m1, TyRcdF m2) ->
    case ((==) `on` M.keysSet) m1 m2 of
      False -> Apart
      _ -> F.fold (M.merge M.dropMissing M.dropMissing (M.zipWithMatched (const unifyCheck)) m1 m2)
  (TyRcdF {}, _) -> Apart
  (TyCmdF c1, TyCmdF c2) -> unifyCheck c1 c2
  (TyCmdF {}, _) -> Apart
  (TyDelayF c1, TyDelayF c2) -> unifyCheck c1 c2
  (TyDelayF {}, _) -> Apart
  (TyFunF t11 t12, TyFunF t21 t22) -> unifyCheck t11 t21 <> unifyCheck t12 t22
  (TyFunF {}, _) -> Apart
