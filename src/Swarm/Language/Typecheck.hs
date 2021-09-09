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
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Swarm.Language.Typecheck
  ( -- * Type errors
    TypeErr(..)

    -- * Infer monad

  , Infer, runInfer, runInfer', lookup, withBinding, withBindings

    -- * Bidirectional type checking / inference

    -- ** Inference
  , inferTop, infer, inferConst

    -- ** Checking
  , check, checkEqual, checkConst

    -- ** Decomposition utilities

  , decomposeCmdTy
  , decomposeFunTy
  , decomposePairTy
  ) where


import           Control.Category      ((>>>))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Map              as M
import           Data.Maybe
import           Prelude               hiding (lookup)

-- import           Control.Unification.IntVar

import           Swarm.Language.Syntax
import           Swarm.Language.Types

------------------------------------------------------------
-- Inference monad

type Infer = ReaderT Ctx (ExceptT TypeErr Identity)
                          {- (IntBindingT TypeF Identity)) -}

runInfer :: Infer a -> Either TypeErr a
runInfer = runInfer' M.empty

runInfer' :: Ctx -> Infer a -> Either TypeErr a
runInfer' ctx = flip runReaderT ctx >>> runExceptT >>> runIdentity

lookup :: Var -> Infer Type
lookup x = do
  ctx <- ask
  maybe (throwError $ UnboundVar x) return (M.lookup x ctx)

withBinding :: MonadReader Ctx m => Var -> Type -> m a -> m a
withBinding x ty = local (M.insert x ty)

withBindings :: MonadReader Ctx m => Ctx -> m a -> m a
withBindings ctx = local (M.union ctx)

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

inferTop :: Term -> Either TypeErr Type
inferTop = runInfer . infer

-- | Try to infer the type of a term under a given context, either
--   returning a type error, or the type of the term.
infer :: Term -> Infer Type

-- Some simple cases.
infer   TUnit                     = return TyUnit
infer   (TConst c)                = inferConst c
infer   (TDir _)                  = return TyDir
infer   (TInt _)                  = return TyInt
infer   (TString _)               = return TyString
infer   (TBool _)                 = return TyBool

-- To infer the type of a pair, just infer both components.
infer (TPair t1 t2)             = (:*:) <$> infer t1 <*> infer t2

-- To infer the type of (return t), infer (t :: a) and then yield (cmd
-- a).  Note that right now we cannot just deal with return in
-- inferConst, because the type of return would have to be
-- polymorphic, and we don't (yet) have polymorphism in the type
-- system.
infer (TApp (TConst Return) t) = Cmd <$> infer t

-- To infer the type of (if b t1 t2):
infer (TApp (TApp (TApp (TConst If) cond) thn) els) = do

  -- Make sure b has type bool
  check cond TyBool

  -- Infer the types of the branches and make sure they are equal
  thnTy <- infer thn
  elsTy <- infer els
  checkEqual thn thnTy elsTy

  return thnTy

-- fst
infer (TApp (TConst Fst) t) = do

  -- Infer the type of t and make sure it's a pair type
  ty <- infer t
  (ty1, _) <- decomposePairTy t ty

  -- Return the type of the first component
  return ty1

-- snd is similar.
infer (TApp (TConst Snd) t) = do
  ty <- infer t
  (_, ty2) <- decomposePairTy t ty
  return ty2

-- delay t has the same type as t.
infer (TDelay t)                = infer t

-- force t has the same type as t.
infer (TApp (TConst Force) t) = infer t

-- Just look up variables in the context.
infer (TVar x)                = lookup x

-- We can infer the type of a lambda if the type of the argument is
-- provided.  Just infer the body under an extended context and return
-- the appropriate function type.
infer (TLam x (Just argTy) t)   = do
  resTy <- withBinding x argTy $ infer t
  return $ argTy :->: resTy

-- To infer the type of an application:
infer (TApp f x)              = do

  -- Infer the type of the left-hand side and make sure it has a function type.
  fTy <- infer f
  (ty1, ty2) <- decomposeFunTy f fTy

  -- Then check that the argument has the right type.
  check x ty1
  return ty2

-- We can infer the type of a let whether a type has been provided for
-- the variable or not.
infer (TLet x Nothing t1 t2)    = do
  xTy <- infer t1
  withBinding  x xTy $ infer t2
infer (TLet x (Just xTy) t1 t2) = do
  withBinding  x xTy $ check t1 xTy
  withBinding  x xTy $ infer t2

infer (TDef x Nothing t1) = do
  xTy <- infer t1
  return $ TyCmd TyUnit (M.singleton x xTy)
infer (TDef x (Just xTy) t1) = do
  withBinding  x xTy $ check t1 xTy
  return $ TyCmd TyUnit (M.singleton x xTy)

-- Bind.  Infer both commands and make sure they have command types.
-- If the first one binds a variable, make sure to add it to the
-- context when checking the second command.
infer (TBind mx c1 c2)        = do
  ty1 <- infer c1
  (a,ctx1) <- decomposeCmdTy c1 ty1
  withBindings ctx1 $ maybe id (`withBinding` a) mx $ do
    cmdb <- infer c2
    (b,ctx2) <- decomposeCmdTy c2 cmdb
    return $ TyCmd b (ctx2 `M.union` ctx1)
infer t = throwError $ CantInfer t

-- | Decompose a type that is supposed to be a command type.
decomposeCmdTy :: MonadError TypeErr m => Term -> Type -> m (Type, Ctx)
decomposeCmdTy _ (TyCmd resTy ctx) = return (resTy, ctx)
decomposeCmdTy t ty                = throwError $ NotCmdTy t ty

-- | Decompose a type that is supposed to be a function type.
decomposeFunTy :: MonadError TypeErr m => Term -> Type -> m (Type, Type)
decomposeFunTy _ (ty1 :->: ty2) = return (ty1, ty2)
decomposeFunTy t ty             = throwError $ NotFunTy t ty

-- | Decompose a type that is supposed to be a pair type.
decomposePairTy :: MonadError TypeErr m => Term -> Type -> m (Type, Type)
decomposePairTy _ (ty1 :*: ty2) = return (ty1, ty2)
decomposePairTy t ty            = throwError $ NotPairTy t ty

-- | The types of some constants can be inferred.  Others (e.g. those
--   that are overloaded or polymorphic) must be checked.
inferConst :: MonadError TypeErr m => Const -> m Type
inferConst Wait        = return $ Cmd TyUnit
inferConst Halt        = return $ Cmd TyUnit
inferConst Noop        = return $ Cmd TyUnit
inferConst Move        = return $ Cmd TyUnit
inferConst Turn        = return $ TyDir :->: Cmd TyUnit
inferConst Grab        = return $ Cmd TyUnit
inferConst Place       = return $ TyString :->: Cmd TyUnit
inferConst Give        = return $ TyString :->: TyString :->: Cmd TyUnit
inferConst Craft       = return $ TyString :->: Cmd TyUnit
inferConst Build       = return $ TyString :->: Cmd TyUnit :->: Cmd TyString
inferConst Run         = return $ TyString :->: Cmd TyUnit
inferConst GetX        = return $ Cmd TyInt
inferConst GetY        = return $ Cmd TyInt
inferConst Random      = return $ TyInt :->: Cmd TyInt
inferConst Say         = return $ TyString :->: Cmd TyUnit
inferConst View        = return $ TyString :->: Cmd TyUnit
inferConst Appear      = return $ TyString :->: Cmd TyUnit
inferConst IsHere      = return $ TyString :->: Cmd TyBool
inferConst Not         = return $ TyBool :->: TyBool
inferConst (Cmp _)     = return $ TyInt :->: TyInt :->: TyBool
inferConst (Arith Neg) = return $ TyInt :->: TyInt
inferConst (Arith _)   = return $ TyInt :->: TyInt :->: TyInt

inferConst c           = throwError $ CantInfer (TConst c)

-- | @check t ty@ checks that @t@ has type @ty@.
check :: Term -> Type -> Infer ()
check (TConst c) ty = checkConst c ty
check (TPair t1 t2) (ty1 :*: ty2) = do
  check t1 ty1
  check t2 ty2
check t@TPair{} ty = throwError $ NonPairTyExpected t ty
check t@(TApp (TConst Return) _) ty = throwError $ NonCmdTyExpected t ty
check (TApp (TApp (TApp (TConst If) cond) thn) els) resTy = do
  check cond TyBool
  check thn resTy
  check els resTy
check t@(TLam x Nothing body) ty = do
  (ty1, ty2) <- decomposeFunTy t ty
  withBinding  x ty1 $ check body ty2
check (TApp t1 t2) ty = do
  ty2 <- infer t2
  check t1 (ty2 :->: ty)

-- Fall-through case: switch into inference mode
check t ty          = infer t >>= checkEqual t ty

-- | Ensure that two types are equal.
checkEqual :: MonadError TypeErr m => Term -> Type -> Type -> m ()
checkEqual t ty ty'
  | ty == ty' = return ()
  | otherwise = throwError $ Mismatch t ty ty'

-- | Check that a constant can have a given type.
checkConst :: MonadError TypeErr m => Const -> Type -> m ()

-- This would be neat (overloaded constants), but type inference falls
-- over a bit.  To make this work we would have to bite the bullet and
-- do a full-fledged constraint-solving version of the type checker
-- with unification variables etc.

-- checkConst Build (Cmd TyUnit :->: Cmd TyUnit) = return ()
-- checkConst Build (TyString     :->: Cmd TyUnit) = return ()
-- checkConst Build ty = throwError $ BadBuildTy ty

-- Fall-through case
checkConst c ty = inferConst c >>= checkEqual (TConst c) ty
