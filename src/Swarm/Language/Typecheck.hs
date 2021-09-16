-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Typecheck
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Type inference for the Swarm language.  For the approach used here,
-- see
-- https://byorgey.wordpress.com/2021/09/08/implementing-hindley-milner-with-the-unification-fd-library/ .
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
  -- For 'Ord IntVar' instance

module Swarm.Language.Typecheck
  ( -- * Type errors
    TypeErr(..)

    -- * Inference monad

  , Infer, runInfer, lookup
  , fresh

    -- * Unification

    , substU, (=:=), HasBindings(..)
    , instantiate, skolemize, generalize

    -- * Type inferen

  , inferTop, inferModule, infer, inferConst, check

  , decomposeCmdTy
  , decomposeFunTy
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

import           Swarm.Language.Context     hiding (lookup)
import qualified Swarm.Language.Context     as Ctx
import           Swarm.Language.Syntax
import           Swarm.Language.Types

------------------------------------------------------------
-- Inference monad

-- | The concrete monad used for type inference.  'IntBindingT' is a
--   monad transformer provided by the @unification-fd@ library which
--   supports various operations such as generating fresh variables
--   and unifying things.
type Infer = ReaderT UCtx (ExceptT TypeErr (IntBindingT TypeF Identity))

-- | Run a top-level inference computation, returning either a
--   'TypeErr' or a fully resolved 'TModule'.
runInfer :: TCtx -> Infer UModule -> Either TypeErr TModule
runInfer ctx =
  (>>= applyBindings) >>>
  (>>= \(Module uty uctx) -> Module <$> (fromU <$> generalize uty) <*> pure (fromU uctx)) >>>
  flip runReaderT (toU ctx) >>>
  runExceptT >>>
  evalIntBindingT >>>
  runIdentity

-- | Look up a variable in the ambient type context, either throwing
--   an 'UnboundVar' error if it is not found, or opening its
--   associated 'UPolytype' with fresh unification variables via
--   'instantiate'.
lookup :: Var -> Infer UType
lookup x = do
  ctx <- ask
  maybe (throwError $ UnboundVar x) instantiate (Ctx.lookup x ctx)

------------------------------------------------------------
-- Dealing with variables: free variables, fresh variables,
-- substitution

-- | @unification-fd@ does not provide an 'Ord' instance for 'IntVar',
--   so we must provide our own, in order to be able to store
--   'IntVar's in a 'Set'.
deriving instance Ord IntVar

-- | A class for getting the free variables (unification or type
--   variables) of a thing.
class FreeVars a where
  freeVars :: a -> Infer (Set (Either Var IntVar))

-- | We can get the free variables of a type (which would consist of
--   only type variables).
instance FreeVars Type where
  freeVars = return . cata (\case {TyVarF x -> S.singleton (Left x); f -> fold f})

-- | We can get the free variables of a 'UType' (which would consist
--   of unification variables as well as type variables).
instance FreeVars UType where
  freeVars ut = do
    fuvs <- fmap (S.fromList . map Right) . lift . lift $ getFreeVars ut
    let ftvs = ucata (const S.empty)
                     (\case {TyVarF x -> S.singleton (Left x); f -> fold f})
                     ut
    return $ fuvs `S.union` ftvs

-- | We can also get the free variables of a polytype.
instance FreeVars t => FreeVars (Poly t) where
  freeVars (Forall xs t) = (\\ S.fromList (map Left xs)) <$> freeVars t

-- | We can get the free variables in any polytype in a context.
instance FreeVars UCtx where
  freeVars = fmap S.unions . mapM freeVars . M.elems . unCtx

-- | Generate a fresh unification variable.
fresh :: Infer UType
fresh = UVar <$> lift (lift freeVar)

-- | Perform a substitution over a 'UType', substituting for both type
--   and unification variables.  Note that since 'UType's do not have
--   any binding constructs, we don't have to worry about ignoring
--   bound variables; all variables in a 'UType' are free.
substU :: Map (Either Var IntVar) UType -> UType -> UType
substU m = ucata
  (\v -> fromMaybe (UVar v) (M.lookup (Right v) m))
  (\case
      TyVarF v -> fromMaybe (UTyVar v) (M.lookup (Left v) m)
      f        -> UTerm f
  )

------------------------------------------------------------
-- Lifted stuff from unification-fd

infix 4 =:=

-- | Constrain two types to be equal.
(=:=) :: UType -> UType -> Infer ()
s =:= t = void (lift $ s U.=:= t)

-- | @unification-fd@ provides a function 'U.applyBindings' which
--   fully substitutes for any bound unification variables (for
--   efficiency, it does not perform such substitution as it goes
--   along).  The 'HasBindings' class is for anything which has
--   unification variables in it and to which we can usefully apply
--   'U.applyBindings'.
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

-- | To 'instantiate' a 'UPolytype', we generate a fresh unification
--   variable for each variable bound by the `Forall`, and then
--   substitute them throughout the type.
instantiate :: UPolytype -> Infer UType
instantiate (Forall xs uty) = do
  xs' <- mapM (const fresh) xs
  return $ substU (M.fromList (zip (map Left xs) xs')) uty

-- | 'skolemize' is like 'instantiate', except we substitute fresh
--   /type/ variables instead of unification variables.  Such
--   variables cannot unify with anything other than themselves.  This
--   is used when checking something with a polytype explicitly
--   specified by the user.
skolemize :: UPolytype -> Infer UType
skolemize (Forall xs uty) = do
  xs' <- mapM (const fresh) xs
  return $ substU (M.fromList (zip (map Left xs) (map toSkolem xs'))) uty
  where
    toSkolem (UVar v) = UTyVar (mkVarName "s" v)
    toSkolem x        = error $ "Impossible! Non-UVar in skolemize.toSkolem: " ++ show x

-- | 'generalize' is the opposite of 'instantiate': add a 'Forall'
--   which closes over all free type and unification variables.
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

  -- | An undefined variable was encountered.
  = UnboundVar Var

  | Infinite IntVar UType

  -- | The given term was expected to have a certain type, but has a
  -- different type instead.
  | Mismatch (TypeF UType) (TypeF UType)

  -- | A definition was encountered not at the top level.
  | DefNotTopLevel Term

instance Fallible TypeF IntVar TypeErr where
  occursFailure = Infinite
  mismatchFailure = Mismatch

------------------------------------------------------------
-- Type inference / checking

-- | Top-level type inference function: given a context of definition
--   types and a top-level term, either return a type error or its
--   type as a 'TModule'.
inferTop :: TCtx -> Term -> Either TypeErr TModule
inferTop ctx = runInfer ctx . inferModule

-- | Infer the signature of a top-level expression which might
--   contain definitions.
inferModule :: Term -> Infer UModule
inferModule = \case

  -- For definitions with no type signature, just infer the type of
  -- the body, generalize it, and return an appropriate context.
  TDef x Nothing t1 -> do
    ty <- infer t1
    pty <- generalize ty
    return $ Module (UTyCmd UTyUnit) (singleton x pty)

  -- If a (poly)type signature has been provided, skolemize it and
  -- check the definition.
  TDef x (Just pty) t1 -> do
    let upty = toU pty
    uty <- skolemize upty
    withBinding x upty $ check t1 uty
    return $ Module (UTyCmd UTyUnit) (singleton x upty)

  -- To handle a 'TBind', infer the types of both sides, combining the
  -- returned modules appropriately.  Have to be careful to use the
  -- correct context when checking the right-hand side in particular.
  TBind mx c1 c2 -> do

    -- First, infer the left side.
    Module cmda ctx1 <- inferModule c1
    a <- decomposeCmdTy cmda

    -- Now infer the right side under an extended context: things in
    -- scope on the right-hand side include both any definitions
    -- created by the left-hand side, as well as a variable as in @x
    -- <- c1; c2@.  The order of extensions here matters: in theory,
    -- c1 could define something with the same name as x, in which
    -- case the bound x should shadow the defined one; hence, we apply
    -- that binding /after/ (i.e. /within/) the application of @ctx1@.
    withBindings ctx1 $ maybe id (`withBinding` Forall [] a) mx $ do
      Module cmdb ctx2 <- inferModule c2

      -- We don't actually need the result type since we're just going
      -- to return cmdb, but it's important to ensure it's a command
      -- type anyway.  Otherwise something like 'move; 3' would be
      -- accepted with type int.
      _ <- decomposeCmdTy cmdb

      -- Ctx.union is right-biased, so ctx1 `union` ctx2 means later
      -- definitions will shadow previous ones.
      return $ Module cmdb (ctx1 `Ctx.union` ctx2)

  -- In all other cases, there can no longer be any definitions in the
  -- term, so delegate to 'infer'.
  t -> trivMod <$> infer t

-- | Infer the type of a term which does not contain definitions.
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
  (ty1, ty2) <- decomposeFunTy fTy

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
  -- If an explicit polytype has been provided,
  uty <- skolemize upty
  withBinding x upty $ do
    check t1 uty
    infer t2

infer t@TDef {} = throwError $ DefNotTopLevel t

infer (TBind mx c1 c2) = do
  ty1 <- infer c1
  a <- decomposeCmdTy ty1
  ty2 <- maybe id (`withBinding` Forall [] a) mx $ infer c2
  _ <- decomposeCmdTy ty2
  return ty2

-- | Decompose a type that is supposed to be a command type.
decomposeCmdTy :: UType -> Infer UType
decomposeCmdTy (UTyCmd a) = return a
decomposeCmdTy ty = do
  a <- fresh
  ty =:= UTyCmd a
  return a

-- | Decompose a type that is supposed to be a function type.
decomposeFunTy :: UType -> Infer (UType, UType)
decomposeFunTy (UTyFun ty1 ty2) = return (ty1, ty2)
decomposeFunTy ty = do
  ty1 <- fresh
  ty2 <- fresh
  ty =:= UTyFun ty1 ty2
  return (ty1, ty2)

-- | Infer the type of a constant.
inferConst :: Const -> UPolytype
inferConst c = case c of
  Wait -> Forall [] $ UTyCmd UTyUnit
  Noop -> Forall [] $ UTyCmd UTyUnit
  Selfdestruct -> Forall [] $ UTyCmd UTyUnit

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
