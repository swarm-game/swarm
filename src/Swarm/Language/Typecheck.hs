-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Typecheck
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Type inference for the Swarm language.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
  -- For 'Ord IntVar' instance

module Swarm.Language.Typecheck
  ( -- * Type errors
    TypeErr(..)

    -- * Infer monad

  , Infer, runInfer, runInfer', lookup, withBinding, withBindings

    -- * Bidirectional type checking / inference

    -- ** Type inference
  , inferTop, inferTop', inferModule, infer, inferConst, check

    -- ** Decomposition utilities

  , decomposeCmdTy
  , decomposeFunTy
  , decomposePairTy
  ) where


import           Control.Category           ((>>>))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Foldable              (fold)
import           Data.Functor.Identity
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Set                   (Set, (\\))
import qualified Data.Set                   as S
import           Prelude                    hiding (lookup)

import           Control.Unification        hiding (applyBindings, (=:=))
import qualified Control.Unification        as U
import           Control.Unification.IntVar

import           Data.Functor.Fixedpoint    (cata)
import           Swarm.Language.Syntax
import           Swarm.Language.Types

------------------------------------------------------------
-- Inference monad

type Infer = ReaderT UCtx (ExceptT TypeErr (IntBindingT TypeF Identity))

runInfer :: Infer UModule -> Either TypeErr TModule
runInfer = runInfer' M.empty

runInfer' :: TCtx -> Infer UModule -> Either TypeErr TModule
runInfer' ctx =
  (>>= applyBindings) >>>
  (>>= \(Module uty uctx) -> Module <$> (fromU <$> generalize uty) <*> pure (fromU uctx)) >>>
  flip runReaderT (toU ctx) >>>
  runExceptT >>>
  evalIntBindingT >>>
  runIdentity

lookup :: Var -> Infer UType
lookup x = do
  ctx <- ask
  maybe (throwError $ UnboundVar x) instantiate (M.lookup x ctx)

withBinding :: MonadReader UCtx m => Var -> UPolytype -> m a -> m a
withBinding x ty = local (M.insert x ty)

withBindings :: MonadReader UCtx m => UCtx -> m a -> m a
withBindings ctx = local (M.union ctx)

------------------------------------------------------------
-- Dealing with variables: free variables, fresh variables,
-- substitution

deriving instance Ord IntVar

class FreeVars a where
  freeVars :: a -> Infer (Set (Either Var IntVar))

instance FreeVars Type where
  freeVars = return . cata (\case {TyVarF x -> S.singleton (Left x); f -> fold f})

instance FreeVars UType where
  freeVars ut = do
    fuvs <- fmap (S.fromList . map Right) . lift . lift $ getFreeVars ut
    let ftvs = ucata (const S.empty)
                     (\case {TyVarF x -> S.singleton (Left x); f -> fold f})
                     ut
    return $ fuvs `S.union` ftvs

instance FreeVars t => FreeVars (Poly t) where
  freeVars (Forall xs t) = (\\ S.fromList (map Left xs)) <$> freeVars t

instance FreeVars UCtx where
  freeVars = fmap S.unions . mapM freeVars . M.elems

fresh :: Infer UType
fresh = UVar <$> lift (lift freeVar)

substU :: Map (Either Var IntVar) UType -> UType -> UType
substU m = ucata
  (\v -> fromMaybe (UVar v) (M.lookup (Right v) m))
  (\case
      TyVarF v -> fromMaybe (UTyVar v) (M.lookup (Left v) m)
      f        -> UTerm f
  )

------------------------------------------------------------
-- Lifted stuff from unification-fd

(=:=) :: UType -> UType -> Infer ()
s =:= t = void (lift $ s U.=:= t)

class HasBindings u where
  applyBindings :: u -> Infer u

instance HasBindings UType where
  applyBindings = lift . U.applyBindings

instance HasBindings UPolytype where
  applyBindings (Forall xs u) = Forall xs <$> applyBindings u

instance HasBindings UCtx where
  applyBindings = mapM applyBindings

instance HasBindings UModule where
  applyBindings (Module uty uctx) = Module <$> applyBindings uty <*> applyBindings uctx

------------------------------------------------------------
-- Converting between mono- and polytypes

instantiate :: UPolytype -> Infer UType
instantiate (Forall xs uty) = do
  xs' <- mapM (const fresh) xs
  return $ substU (M.fromList (zip (map Left xs) xs')) uty

skolemize :: UPolytype -> Infer UType
skolemize (Forall xs uty) = do
  xs' <- mapM (const fresh) xs
  return $ substU (M.fromList (zip (map Left xs) (map toSkolem xs'))) uty
  where
    toSkolem (UVar v) = UTyVar (mkVarName "s" v)
    toSkolem x        = error $ "Impossible! Non-UVar in skolemize.toSkolem: " ++ show x

generalize :: UType -> Infer UPolytype
generalize uty = do
  uty' <- applyBindings uty
  ctx <- ask
  tmfvs  <- freeVars uty'
  ctxfvs <- freeVars ctx
  let fvs = S.toList $ tmfvs \\ ctxfvs
      xs  = map (either id (mkVarName "a")) fvs
  return $ Forall xs (substU (M.fromList (zip fvs (map UTyVar xs))) uty')

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

  -- | An undefined variable was encountered.
  | UnboundVar Var

  | Infinite IntVar UType

  -- | The given term was expected to have a certain type, but has a
  -- different type instead.
  | Mismatch (TypeF UType) (TypeF UType)

  | DefNotTopLevel Term

instance Fallible TypeF IntVar TypeErr where
  occursFailure = Infinite
  mismatchFailure = Mismatch

------------------------------------------------------------
-- Type inference / checking

inferTop :: Term -> Either TypeErr TModule
inferTop = runInfer . inferModule

inferTop' :: TCtx -> Term -> Either TypeErr TModule
inferTop' ctx = runInfer' ctx . inferModule

-- | Infer the signature of a top-level module containing definitions.
inferModule :: Term -> Infer UModule
inferModule (TDef x Nothing t1) = do
  ty <- infer t1
  pty <- generalize ty
  return $ Module (UTyCmd UTyUnit) (M.singleton x pty)
inferModule (TDef x (Just pty) t1) = do
  let upty = toU pty
  uty <- skolemize upty
  withBinding x upty $ check t1 uty
  return $ Module (UTyCmd UTyUnit) (M.singleton x upty)
inferModule (TBind mx c1 c2) = do
  Module cmda ctx1 <- inferModule c1
  a <- decomposeCmdTy c1 cmda
  withBindings ctx1 $ maybe id (`withBinding` Forall [] a) mx $ do
    Module cmdb ctx2 <- inferModule c2
    b <- decomposeCmdTy c2 cmdb
    return $ Module (UTyCmd b) (ctx2 `M.union` ctx1)
inferModule t = trivMod <$> infer t

-- | Try to infer the type of a term under a given context.
infer :: Term -> Infer UType

infer   TUnit                     = return UTyUnit
infer   (TConst c)                = instantiate $ inferConst c
infer   (TDir _)                  = return UTyDir
infer   (TInt _)                  = return UTyInt
infer   (TString _)               = return UTyString
infer   (TBool _)                 = return UTyBool

-- To infer the type of a pair, just infer both components.
infer (TPair t1 t2)               = UTyProd <$> infer t1 <*> infer t2

-- delay t has the same type as t.
infer (TDelay t)                  = infer t

-- Just look up variables in the context.
infer (TVar x)                    = lookup x

-- To infer the type of a lambda if the type of the argument is
-- provided, just infer the body under an extended context and return
-- the appropriate function type.
infer (TLam x (Just argTy) t)   = do
  let uargTy = toU argTy
  resTy <- withBinding x (Forall [] uargTy) $ infer t
  return $ UTyFun uargTy resTy

-- If the type of the argument is not provided, create a fresh
-- unification variable for it and proceed.
infer (TLam x Nothing t) = do
  argTy <- fresh
  resTy <- withBinding x (Forall [] argTy) $ infer t
  return $ UTyFun argTy resTy

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
  upty <- generalize =<< infer t1
  withBinding  x upty $ infer t2
infer (TLet x (Just pty) t1 t2) = do
  let upty = toU pty
  uty <- skolemize upty
  withBinding x upty $ do
    check t1 uty
    infer t2

infer t@TDef {} = throwError $ DefNotTopLevel t

infer (TBind mx c1 c2) = do
  ty1 <- infer c1
  a <- decomposeCmdTy c1 ty1
  ty2 <- case mx of
    Nothing -> infer c2
    Just x  -> withBinding x (Forall [] a) $ infer c2
  _ <- decomposeCmdTy c2 ty2
  return ty2

-- | Decompose a type that is supposed to be a command type.
decomposeCmdTy :: Term -> UType -> Infer UType
decomposeCmdTy _ (UTyCmd a) = return a
decomposeCmdTy _ ty = do
  a <- fresh
  ty =:= UTyCmd a
  return a

-- | Decompose a type that is supposed to be a function type.
decomposeFunTy :: Term -> UType -> Infer (UType, UType)
decomposeFunTy _ (UTyFun ty1 ty2) = return (ty1, ty2)
decomposeFunTy _ ty             = do
  ty1 <- fresh
  ty2 <- fresh
  ty =:= UTyFun ty1 ty2
  return (ty1, ty2)

-- | Decompose a type that is supposed to be a pair type.
decomposePairTy :: Term -> UType -> Infer (UType, UType)
decomposePairTy _ (UTyProd ty1 ty2) = return (ty1, ty2)
decomposePairTy _ ty = do
  ty1 <- fresh
  ty2 <- fresh
  ty =:= UTyProd ty1 ty2
  return (ty1, ty2)

-- | Infer the type of a constant.
inferConst :: Const -> UPolytype
inferConst c = case c of
  Wait -> Forall [] $ UTyCmd UTyUnit
  Noop -> Forall [] $ UTyCmd UTyUnit
  Halt -> Forall [] $ UTyCmd UTyUnit

  Move -> Forall [] $ UTyCmd UTyUnit
  Turn -> Forall [] $ UTyFun UTyDir (UTyCmd UTyUnit)
  Grab -> Forall [] $ UTyCmd UTyUnit
  Place -> Forall [] $ UTyFun UTyString (UTyCmd UTyUnit)
  Give -> Forall [] $ UTyFun UTyString (UTyFun UTyString (UTyCmd UTyUnit))
  Make -> Forall [] $ UTyFun UTyString (UTyCmd UTyUnit)
  Build -> Forall ["a"] $ UTyFun UTyString (UTyFun (UTyCmd "a") (UTyCmd UTyString))
  Say -> Forall [] $ UTyFun UTyString (UTyCmd UTyUnit)
  View -> Forall [] $ UTyFun UTyString (UTyCmd UTyUnit)
  Appear -> Forall [] $ UTyFun UTyString (UTyCmd UTyUnit)

  GetX -> Forall [] $ UTyCmd UTyInt
  GetY -> Forall [] $ UTyCmd UTyInt
  Blocked -> Forall [] $ UTyCmd UTyBool
  Ishere -> Forall [] $ UTyFun UTyString (UTyCmd UTyBool)
  Random -> Forall [] $ UTyFun UTyInt (UTyCmd UTyInt)

  Run -> Forall [] $ UTyFun UTyString (UTyCmd UTyUnit)

  Not -> Forall [] $ UTyFun UTyBool UTyBool
  Cmp _ -> Forall ["a"] (UTyFun "a" (UTyFun "a" UTyBool))
  Neg -> Forall [] $ UTyFun UTyInt UTyInt
  Arith _ ->  Forall [] $ UTyFun UTyInt (UTyFun UTyInt UTyInt)

  If -> Forall ["a"] $ UTyFun UTyBool (UTyFun "a" (UTyFun "a" "a"))
  Fst -> Forall ["a","b"] $ UTyFun (UTyProd "a" "b") "a"
  Snd -> Forall ["a","b"] $ UTyFun (UTyProd "a" "b") "b"
  Force -> Forall ["a"] $ UTyFun "a" "a"
  Return -> Forall ["a"] $ UTyFun "a" (UTyCmd "a")
  Try -> Forall ["a"] $ UTyFun (UTyCmd "a") (UTyFun (UTyCmd "a") (UTyCmd "a"))
  Raise -> Forall ["a"] $ UTyFun UTyString (UTyCmd "a")

-- | @check t ty@ checks that @t@ has type @ty@.
check :: Term -> UType -> Infer ()
check t ty = do
  ty' <- infer t
  _ <- ty =:= ty'
  return ()
