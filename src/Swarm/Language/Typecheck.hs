-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Typecheck
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Type checking and inference for the Swarm language.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Swarm.Language.Typecheck
  ( -- * Type errors
    TypeErr(..)

    -- * Context

  , lookupTy

    -- * Bidirectional type checking / inference

    -- ** Inference
  , infer, inferConst

    -- ** Checking
  , check, checkEqual, checkConst

    -- ** Decomposition utilities

  , decomposeCmdTy
  , decomposeFunTy
  , decomposePairTy
  ) where


import qualified Data.Map              as M

import           Swarm.Language.Syntax
import           Swarm.Language.Types
import           Swarm.Util

------------------------------------------------------------
-- Type errors

-- | Errors that can occur during type checking.  The idea is that
--   each error carries information that can be used to help explain
--   what went wrong (though the amount of information carried can and
--   should be very much improved in the future); errors can then
--   separately be pretty-printed to display them to the user.
data TypeErr

  -- | The given term should have a function type, but it has the
  -- given type instead.
  = NotFunTy Term Type

  -- | The given term should have a function type, but it has the
  -- given type instead.
  | NotPairTy Term Type

  -- | The given term should have a command type, but it has the
  -- given type instead.
  | NotCmdTy Term Type

  -- | The given term has a command type, but was expected from the
  --   context to have some other type.
  | NonCmdTyExpected Term Type

  -- | The given term has a pair type, but was expected from the
  --   context to have some other type.
  | NonPairTyExpected Term Type

  -- | The given term was expected to have a certain type, but has a
  -- different type instead.
  | Mismatch Term {- expected -} Type {- inferred -} Type

  -- | An undefined variable was encountered.
  | UnboundVar Var

  -- | Tried to infer the type of a term which we cannot infer.
  | CantInfer Term

------------------------------------------------------------
-- Type inference / checking

-- | Look up the type of a variable in the context.
lookupTy :: Var -> Ctx -> Either TypeErr Type
lookupTy x ctx = maybe (Left (UnboundVar x)) return (M.lookup x ctx)

-- | Try to infer the type of a term under a given context, either
--   returning a type error, or the term with all type annotations
--   filled in (see the documentation for 'ATerm'), along with its
--   type.
infer :: Ctx -> Term -> Either TypeErr (ATerm ::: Type)

-- Some simple cases.
infer _   TUnit                     = return $ TUnit ::: TyUnit
infer _   (TConst c)                = (TConst c :::) <$> inferConst c
infer _   (TDir d)                  = return $ TDir d ::: TyDir
infer _   (TInt n)                  = return $ TInt n ::: TyInt
infer _   (TString s)               = return $ TString s ::: TyString
infer _   (TBool b)                 = return $ TBool b ::: TyBool

-- To infer the type of a pair, just infer both components.
infer ctx (TPair t1 t2)             = do
  at1 ::: ty1 <- infer ctx t1
  at2 ::: ty2 <- infer ctx t2
  return $ TPair at1 at2 ::: (ty1 :*: ty2)

-- To infer the type of (return t), infer (t :: a) and then yield
-- (return t :: cmd a).  Note that right now we cannot just deal with
-- return in inferConst, because the type of return would have to be
-- polymorphic, and we don't (yet) have polymorphism in the type
-- system.
infer ctx (TApp _ (TConst Return) t) = do
  at ::: ty <- infer ctx t
  return $ TApp (ID ty) (TConst Return) at ::: TyCmd ty

-- To infer the type of (if b t1 t2):
infer ctx (TApp _ (TApp _ (TApp _ (TConst If) cond) thn) els) = do

  -- Make sure b has type bool
  acond <- check ctx cond TyBool

  -- Infer the types of the branches and make sure they are equal
  athn ::: thnTy <- infer ctx thn
  aels ::: elsTy <- infer ctx els
  checkEqual thn thnTy elsTy

  return $
    TApp (ID thnTy) (TApp (ID thnTy) (TApp (ID TyBool) (TConst If) acond) athn) aels ::: thnTy

-- fst
infer ctx (TApp _ (TConst Fst) t) = do

  -- Infer the type of t and make sure it's a pair type
  at ::: ty <- infer ctx t
  (ty1, _) <- decomposePairTy t ty

  -- Return the type of the first component
  return $ TApp (ID ty) (TConst Fst) at ::: ty1

-- snd is similar.
infer ctx (TApp _ (TConst Snd) t) = do
  at ::: ty <- infer ctx t
  (_, ty2) <- decomposePairTy t ty
  return $ TApp (ID ty) (TConst Snd) at ::: ty2

-- delay t has the same type as t.
infer ctx (TDelay x)                = do
  t ::: ty <- infer ctx x
  return $ TDelay t ::: ty

-- force t has the same type as t.
infer ctx (TApp _ (TConst Force) t) = do
  at ::: ty <- infer ctx t
  return $ TApp (ID ty) (TConst Force) at ::: ty

-- Just look up variables in the context.
infer ctx (TVar x)                = do
  ty <- lookupTy x ctx
  return $ TVar x ::: ty

-- We can infer the type of a lambda if the type of the argument is
-- provided.  Just infer the body under an extended context and return
-- the appropriate function type.
infer ctx (TLam x (Just argTy) t)   = do
  at ::: resTy <- infer (M.insert x argTy ctx) t
  return $ TLam x (ID argTy) at ::: (argTy :->: resTy)

-- To infer the type of an application:
infer ctx (TApp _ f x)              = do

  -- Infer the type of the left-hand side and make sure it has a function type.
  (af ::: fTy) <- infer ctx f
  (ty1, ty2) <- decomposeFunTy f fTy

  -- Then check that the argument has the right type.
  ax <- check ctx x ty1
  return $ TApp (ID ty1) af ax ::: ty2

-- We can infer the type of a let whether a type has been provided for
-- the variable or not.
infer ctx (TLet x Nothing t1 t2)    = do
  at1 ::: xTy <- infer ctx t1
  at2 ::: t2Ty <- infer (M.insert x xTy ctx) t2
  return $ TLet x (ID xTy) at1 at2 ::: t2Ty
infer ctx (TLet x (Just xTy) t1 t2) = do
  at1 <- check (M.insert x xTy ctx) t1 xTy
  at2 ::: t2Ty <- infer (M.insert x xTy ctx) t2
  return $ TLet x (ID xTy) at1 at2 ::: t2Ty

infer ctx (TDef x Nothing t1) = do
  at1 ::: xTy <- infer ctx t1
  return $ TDef x (ID xTy) at1 ::: TyCmd' TyUnit (M.singleton x xTy)
infer ctx (TDef x (Just xTy) t1) = do
  at1 <- check (M.insert x xTy ctx) t1 xTy
  return $ TDef x (ID xTy) at1 ::: TyCmd' TyUnit (M.singleton x xTy)

-- Bind.  Infer both commands and make sure they have command types.
-- If the first one binds a variable, make sure to add it to the
-- context when checking the second command.
infer ctx (TBind mx _ c1 c2)        = do
  ac1 ::: ty1 <- infer ctx c1
  (a,ctx1) <- decomposeCmdTy c1 ty1
  ac2 ::: cmdb <- infer (ctx1 `M.union` maybe id (`M.insert` a) mx ctx) c2
  (b,ctx2) <- decomposeCmdTy c2 cmdb
  return $ TBind mx (ID ty1) ac1 ac2 ::: TyCmd' b (ctx2 `M.union` ctx1)
infer _ t = Left $ CantInfer t

-- | Decompose a type that is supposed to be a command type.
decomposeCmdTy :: Term -> Type -> Either TypeErr (Type, Ctx)
decomposeCmdTy _ (TyCmd' resTy ctx) = return (resTy, ctx)
decomposeCmdTy t ty                 = Left (NotCmdTy t ty)

-- | Decompose a type that is supposed to be a function type.
decomposeFunTy :: Term -> Type -> Either TypeErr (Type, Type)
decomposeFunTy _ (ty1 :->: ty2) = return (ty1, ty2)
decomposeFunTy t ty             = Left (NotFunTy t ty)

-- | Decompose a type that is supposed to be a pair type.
decomposePairTy :: Term -> Type -> Either TypeErr (Type, Type)
decomposePairTy _ (ty1 :*: ty2) = return (ty1, ty2)
decomposePairTy t ty            = Left (NotPairTy t ty)

-- | The types of some constants can be inferred.  Others (e.g. those
--   that are overloaded or polymorphic) must be checked.
inferConst :: Const -> Either TypeErr Type
inferConst Wait        = return $ TyCmd TyUnit
inferConst Halt        = return $ TyCmd TyUnit
inferConst Noop        = return $ TyCmd TyUnit
inferConst Move        = return $ TyCmd TyUnit
inferConst Turn        = return $ TyDir :->: TyCmd TyUnit
inferConst Grab        = return $ TyCmd TyUnit
inferConst Place       = return $ TyString :->: TyCmd TyUnit
inferConst Give        = return $ TyString :->: TyString :->: TyCmd TyUnit
inferConst Craft       = return $ TyString :->: TyCmd TyUnit
inferConst Build       = return $ TyString :->: TyCmd TyUnit :->: TyCmd TyString
inferConst Run         = return $ TyString :->: TyCmd TyUnit
inferConst GetX        = return $ TyCmd TyInt
inferConst GetY        = return $ TyCmd TyInt
inferConst Random      = return $ TyInt :->: TyCmd TyInt
inferConst Say         = return $ TyString :->: TyCmd TyUnit
inferConst View        = return $ TyString :->: TyCmd TyUnit
inferConst Appear      = return $ TyString :->: TyCmd TyUnit
inferConst IsHere      = return $ TyString :->: TyCmd TyBool
inferConst Not         = return $ TyBool :->: TyBool
inferConst (Cmp _)     = return $ TyInt :->: TyInt :->: TyBool
inferConst (Arith Neg) = return $ TyInt :->: TyInt
inferConst (Arith _)   = return $ TyInt :->: TyInt :->: TyInt

inferConst c           = Left $ CantInfer (TConst c)

-- | @check ctx t ty@ checks that @t@ has type @ty@ under context
--   @ctx@, returning either a type error or a fully type-annotated
--   term.
check :: Ctx -> Term -> Type -> Either TypeErr ATerm
check _ (TConst c) ty = checkConst c ty >> return (TConst c)
check ctx (TPair t1 t2) (ty1 :*: ty2) = do
  at1 <- check ctx t1 ty1
  at2 <- check ctx t2 ty2
  return $ TPair at1 at2
check _ t@TPair{} ty = Left $ NonPairTyExpected t ty
check _ t@(TApp _ (TConst Return) _) ty = Left $ NonCmdTyExpected t ty
check ctx (TApp _ (TApp _ (TApp _ (TConst If) cond) thn) els) resTy = do
  acond <- check ctx cond TyBool
  athn  <- check ctx thn resTy
  aels  <- check ctx els resTy
  return $
    TApp (ID resTy) (TApp (ID resTy) (TApp (ID TyBool) (TConst If) acond) athn) aels
check ctx t@(TLam x Nothing body) ty = do
  (ty1, ty2) <- decomposeFunTy t ty
  abody <- check (M.insert x ty1 ctx) body ty2
  return $ TLam x (ID ty1) abody
check ctx (TApp _ t1 t2) ty = do
  at2 ::: ty2 <- infer ctx t2
  at1 <- check ctx t1 (ty2 :->: ty)
  return $ TApp (ID ty2) at1 at2

-- Fall-through case: switch into inference mode
check ctx t ty          = do
  at ::: ty' <- infer ctx t
  checkEqual t ty ty'
  return at

-- | Ensure that two types are equal.
checkEqual :: Term -> Type -> Type -> Either TypeErr ()
checkEqual t ty ty'
  | ty == ty' = return ()
  | otherwise = Left (Mismatch t ty ty')

-- | Check that a constant can have a given type.
checkConst :: Const -> Type -> Either TypeErr ()

-- This would be neat (overloaded constants), but type inference falls
-- over a bit.  To make this work we would have to bite the bullet and
-- do a full-fledged constraint-solving version of the type checker
-- with unification variables etc.

-- checkConst Build (TyCmd TyUnit :->: TyCmd TyUnit) = return ()
-- checkConst Build (TyString     :->: TyCmd TyUnit) = return ()
-- checkConst Build ty = Left $ BadBuildTy ty

-- Fall-through case
checkConst c ty = inferConst c >>= checkEqual (TConst c) ty
