{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Swarm.Typecheck where

import           Data.Functor.Identity
import           Data.Map              (Map)
import qualified Data.Map              as M

import           Swarm.AST
import           Swarm.Types

------------------------------------------------------------
-- Type errors

data TypeErr
  = NotFunTy Term Type
  | NotCmdTy Term Type
  | Mismatch Term {- expected -} Type {- inferred -} Type
  | UnboundVar Var
  | CantInfer Term

------------------------------------------------------------
-- Type inference / checking

type Ctx = Map Var Type

data a ::: b = a ::: b

(<:::>) :: Functor f => a -> f b -> f (a ::: b)
a <:::> fb = fmap (a :::) fb

lookupTy :: Var -> Ctx -> Either TypeErr Type
lookupTy x ctx = maybe (Left (UnboundVar x)) return (M.lookup x ctx)

infer :: Ctx -> Term -> Either TypeErr (ATerm ::: Type)
infer _   TUnit                     = return $ TUnit ::: TyUnit
infer _   (TConst c)                = TConst c <:::> inferConst c
infer _   (TDir d)                  = return $ TDir d ::: TyDir
infer _   (TInt n)                  = return $ TInt n ::: TyInt
infer _   (TString s)               = return $ TString s ::: TyString
infer _   (TBool b)                 = return $ TBool b ::: TyBool
infer ctx (TApp _ (TApp _ (TApp _ (TConst If) cond) thn) els) = do
  acond <- check ctx cond TyBool  --- allow cmd bool too??
  athn ::: thnTy <- infer ctx thn
  aels ::: elsTy <- infer ctx els
  checkEqual thn thnTy elsTy
  return $
    TApp (Identity thnTy) (TApp (Identity thnTy) (TApp (Identity TyBool) (TConst If) acond) athn) aels ::: thnTy
infer ctx (TDelay x)                = do
  t ::: ty <- infer ctx x
  return $ TDelay t ::: ty
infer ctx (TApp _ (TConst Force) t) = do
  at ::: ty <- infer ctx t
  return $ TApp (Identity ty) (TConst Force) at ::: ty
infer ctx (TVar _ x)                = do
  ty <- lookupTy x ctx
  return $ TVar (Identity ty) x ::: ty
infer ctx (TLam x (Just argTy) t)   = do
  at ::: resTy <- infer (M.insert x argTy ctx) t
  return $ TLam x (Identity argTy) at ::: (argTy :->: resTy)
infer ctx (TApp _ f x)              = do
  (af ::: fTy) <- infer ctx f
  (ty1, ty2) <- decomposeFunTy f fTy
  ax <- check ctx x ty1
  return $ TApp (Identity ty1) af ax ::: ty2
infer ctx (TLet x Nothing t1 t2)    = do
  at1 ::: xTy <- infer ctx t1
  at2 ::: t2Ty <- infer (M.insert x xTy ctx) t2
  return $ TLet x (Identity xTy) at1 at2 ::: t2Ty
infer ctx (TLet x (Just xTy) t1 t2) = do
  at1 <- check ctx t1 xTy
  at2 ::: t2Ty <- infer (M.insert x xTy ctx) t2
  return $ TLet x (Identity xTy) at1 at2 ::: t2Ty
infer ctx (TBind mx _ c1 c2)        = do
  ac1 ::: ty1 <- infer ctx c1
  a <- decomposeCmdTy c1 ty1
  ac2 ::: cmdb <- infer (maybe id (`M.insert` a) mx ctx) c2
  _ <- decomposeCmdTy c2 cmdb
  return $ TBind mx (Identity ty1) ac1 ac2 ::: cmdb
infer _ TNop = return $ TNop ::: TyCmd TyUnit
infer _ t = Left $ CantInfer t

decomposeCmdTy :: Term -> Type -> Either TypeErr Type
decomposeCmdTy _ (TyCmd resTy) = return resTy
decomposeCmdTy t ty            = Left (NotCmdTy t ty)

-- | The types of some constants can be inferred.  Others (e.g. those
--   that are overloaded) must be checked.
inferConst :: Const -> Either TypeErr Type
inferConst Wait    = return $ TyCmd TyUnit
inferConst Move    = return $ TyCmd TyUnit
inferConst Turn    = return $ TyDir :->: TyCmd TyUnit
inferConst Harvest = return $ TyCmd TyUnit
inferConst Repeat  = return $ TyInt :->: TyCmd TyUnit :->: TyCmd TyUnit
inferConst Build   = return $ TyCmd TyUnit :->: TyCmd TyUnit
inferConst Run     = return $ TyString :->: TyCmd TyUnit
inferConst GetX    = return $ TyCmd TyInt
inferConst GetY    = return $ TyCmd TyInt

inferConst c       = Left $ CantInfer (TConst c)

decomposeFunTy :: Term -> Type -> Either TypeErr (Type, Type)
decomposeFunTy _ (ty1 :->: ty2) = return (ty1, ty2)
decomposeFunTy t ty             = Left (NotFunTy t ty)

check :: Ctx -> Term -> Type -> Either TypeErr ATerm
check _ (TConst c) ty = checkConst c ty >> return (TConst c)
check ctx (TApp _ (TApp _ (TApp _ (TConst If) cond) thn) els) resTy = do
  acond <- check ctx cond TyBool
  athn  <- check ctx thn resTy
  aels  <- check ctx els resTy
  return $
    TApp (Identity resTy) (TApp (Identity resTy) (TApp (Identity TyBool) (TConst If) acond) athn) aels
check ctx t@(TLam x Nothing body) ty = do
  (ty1, ty2) <- decomposeFunTy t ty
  check (M.insert x ty1 ctx) body ty2
check ctx (TApp _ t1 t2) ty = do
  at2 ::: ty2 <- infer ctx t2
  at1 <- check ctx t1 (ty2 :->: ty)
  return $ TApp (Identity ty2) at1 at2

-- Fall-through case: switch into inference mode
check ctx t ty          = do
  at ::: ty' <- infer ctx t
  checkEqual t ty ty'
  return at

checkEqual :: Term -> Type -> Type -> Either TypeErr ()
checkEqual t ty ty'
  | ty == ty' = return ()
  | otherwise = Left (Mismatch t ty ty')

checkConst :: Const -> Type -> Either TypeErr ()
-- No cases for now!  Add some cases once constants become overloaded.

-- Fall-through case
checkConst c ty = inferConst c >>= checkEqual (TConst c) ty
