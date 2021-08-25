{-# LANGUAGE OverloadedStrings #-}

module Swarm.Typecheck where

import           Data.Bifunctor (first)
import           Data.Text      (Text)

import           Swarm.AST

data Type
  = TyUnit
  | TyInt
  | TyDir
  | TyCmd                   -- Later this may have a result type attached
  | Type :->: Type
  deriving (Eq, Ord, Show)

infixr 1 :->:

data TypeErr
  = NotFunTy Term Type
  | Mismatch Term {- expected -} Type {- inferred -} Type

------------------------------------------------------------

infer :: Term -> Either TypeErr Type
infer (TConst c) = inferConst c
infer (TDir _)   = return TyDir
infer (TInt _)   = return TyInt
infer (TApp f x) = do
  (ty1, ty2) <- inferFunTy f
  check x ty1
  return ty2
infer (TBind c1 c2) = do   -- Later this may have a variable binding etc.
  check c1 TyCmd
  check c2 TyCmd
  return TyCmd
infer TNop = return TyCmd

-- | The types of some constants can be inferred.  Others (e.g. those
--   that are overloaded) must be checked.
inferConst :: Const -> Either TypeErr Type
inferConst Wait    = return TyCmd
inferConst Move    = return TyCmd
inferConst Turn    = return (TyDir :->: TyCmd)
inferConst Harvest = return TyCmd
inferConst Repeat  = return (TyInt :->: TyCmd :->: TyCmd)
inferConst Build   = return (TyCmd :->: TyCmd)

inferFunTy :: Term -> Either TypeErr (Type, Type)
inferFunTy t = do
  ty <- infer t
  case ty of
    ty1 :->: ty2 -> return (ty1, ty2)
    _            -> Left (NotFunTy t ty)

check :: Term -> Type -> Either TypeErr ()
check (TConst c) ty = checkConst c ty
check (TApp t1 t2) ty = do
  ty2 <- infer t2
  check t1 (ty2 :->: ty)

-- Fall-through case: switch into inference mode
check t ty          = infer t >>= checkEqual t ty

checkEqual :: Term -> Type -> Type -> Either TypeErr ()
checkEqual t ty ty'
  | ty == ty' = return ()
  | otherwise = Left (Mismatch t ty ty')

checkConst :: Const -> Type -> Either TypeErr ()
-- No other cases for now!
-- Fall-through case
checkConst c ty = inferConst c >>= checkEqual (TConst c) ty
