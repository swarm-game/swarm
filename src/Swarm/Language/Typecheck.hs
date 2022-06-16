{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- For 'Ord IntVar' instance

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
module Swarm.Language.Typecheck (
  -- * Type errors
  TypeErr (..),
  getTypeErrLocation,

  -- * Inference monad
  Infer,
  runInfer,
  lookup,
  fresh,

  -- * Unification
  substU,
  (=:=),
  HasBindings (..),
  instantiate,
  skolemize,
  generalize,

  -- * Type inferen
  inferTop,
  inferModule,
  infer,
  inferConst,
  check,
  decomposeCmdTy,
  decomposeFunTy,
) where

import Control.Category ((>>>))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable (fold)
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Prelude hiding (lookup)

import Control.Unification hiding (applyBindings, (=:=))
import qualified Control.Unification as U
import Control.Unification.IntVar

import Swarm.Language.Context hiding (lookup)
import qualified Swarm.Language.Context as Ctx
import Swarm.Language.Parse.QQ (tyQ)
import Swarm.Language.Syntax
import Swarm.Language.Types

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
  (>>= applyBindings)
    >>> (>>= \(Module uty uctx) -> Module <$> (fromU <$> generalize uty) <*> pure (fromU uctx))
    >>> flip runReaderT (toU ctx)
    >>> runExceptT
    >>> evalIntBindingT
    >>> runIdentity

-- | Look up a variable in the ambient type context, either throwing
--   an 'UnboundVar' error if it is not found, or opening its
--   associated 'UPolytype' with fresh unification variables via
--   'instantiate'.
lookup :: Location -> Var -> Infer UType
lookup loc x = do
  ctx <- ask
  maybe (throwError $ UnboundVar loc x) instantiate (Ctx.lookup x ctx)

------------------------------------------------------------
-- Dealing with variables: free variables, fresh variables,
-- substitution

-- | @unification-fd@ does not provide an 'Ord' instance for 'IntVar',
--   so we must provide our own, in order to be able to store
--   'IntVar's in a 'Set'.
deriving instance Ord IntVar

-- | A class for getting the free unification variables of a thing.
class FreeVars a where
  freeVars :: a -> Infer (Set IntVar)

-- | We can get the free unification variables of a 'UType'.
instance FreeVars UType where
  freeVars ut = fmap S.fromList . lift . lift $ getFreeVars ut

-- | We can also get the free variables of a polytype.
instance FreeVars t => FreeVars (Poly t) where
  freeVars (Forall _ t) = freeVars t

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
substU m =
  ucata
    (\v -> fromMaybe (UVar v) (M.lookup (Right v) m))
    ( \case
        TyVarF v -> fromMaybe (UTyVar v) (M.lookup (Left v) m)
        f -> UTerm f
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
  toSkolem x = error $ "Impossible! Non-UVar in skolemize.toSkolem: " ++ show x

-- | 'generalize' is the opposite of 'instantiate': add a 'Forall'
--   which closes over all free type and unification variables.
generalize :: UType -> Infer UPolytype
generalize uty = do
  uty' <- applyBindings uty
  ctx <- ask
  tmfvs <- freeVars uty'
  ctxfvs <- freeVars ctx
  let fvs = S.toList $ tmfvs \\ ctxfvs
      xs = map (mkVarName "a") fvs
  return $ Forall xs (substU (M.fromList (zip (map Right fvs) (map UTyVar xs))) uty')

------------------------------------------------------------
-- Type errors

-- | Errors that can occur during type checking.  The idea is that
--   each error carries information that can be used to help explain
--   what went wrong (though the amount of information carried can and
--   should be very much improved in the future); errors can then
--   separately be pretty-printed to display them to the user.
data TypeErr
  = -- | An undefined variable was encountered.
    UnboundVar Location Var
  | -- | A Skolem variable escaped its local context.
    EscapedSkolem Location Var
  | Infinite IntVar UType
  | -- | The given term was expected to have a certain type, but has a
    -- different type instead.
    Mismatch Location (TypeF UType) (TypeF UType)
  | -- | A definition was encountered not at the top level.
    DefNotTopLevel Location Term
  | -- | A term was encountered which we cannot infer the type of.
    --   This should never happen.
    CantInfer Location Term
  deriving (Show)

instance Fallible TypeF IntVar TypeErr where
  occursFailure = Infinite
  mismatchFailure = Mismatch NoLoc

getTypeErrLocation :: TypeErr -> Maybe Location
getTypeErrLocation te = case te of
  UnboundVar l _ -> Just l
  EscapedSkolem l _ -> Just l
  Infinite _ _ -> Nothing
  Mismatch l _ _ -> Just l
  DefNotTopLevel l _ -> Just l
  CantInfer l _ -> Just l

------------------------------------------------------------
-- Type inference / checking

-- | Top-level type inference function: given a context of definition
--   types and a top-level term, either return a type error or its
--   type as a 'TModule'.
inferTop :: TCtx -> Syntax -> Either TypeErr TModule
inferTop ctx = runInfer ctx . inferModule

-- | Infer the signature of a top-level expression which might
--   contain definitions.
inferModule :: Syntax -> Infer UModule
inferModule s@(Syntax _ t) = (`catchError` addLocToTypeErr s) $ case t of
  -- For definitions with no type signature, make up a fresh type
  -- variable for the body, infer the body under an extended context,
  -- and unify the two.  Then generalize the type and return an
  -- appropriate context.
  SDef _ x Nothing t1 -> do
    xTy <- fresh
    ty <- withBinding x (Forall [] xTy) $ infer t1
    xTy =:= ty
    pty <- generalize ty
    return $ Module (UTyCmd UTyUnit) (singleton x pty)

  -- If a (poly)type signature has been provided, skolemize it and
  -- check the definition.
  SDef _ x (Just pty) t1 -> do
    let upty = toU pty
    uty <- skolemize upty
    withBinding x upty $ check t1 uty
    return $ Module (UTyCmd UTyUnit) (singleton x upty)

  -- To handle a 'TBind', infer the types of both sides, combining the
  -- returned modules appropriately.  Have to be careful to use the
  -- correct context when checking the right-hand side in particular.
  SBind mx c1 c2 -> do
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
    withBindings ctx1 $
      maybe id (`withBinding` Forall [] a) mx $ do
        Module cmdb ctx2 <- inferModule c2

        -- We don't actually need the result type since we're just going
        -- to return cmdb, but it's important to ensure it's a command
        -- type anyway.  Otherwise something like 'move; 3' would be
        -- accepted with type int.
        _ <- decomposeCmdTy cmdb

        -- Ctx.union is right-biased, so ctx1 `union` ctx2 means later
        -- definitions will shadow previous ones.  Include the binder
        -- (if any) as well, since binders are made available at the top
        -- level, just like definitions. e.g. if the user writes `r <- build {move}`,
        -- then they will be able to refer to r again later.
        let ctxX = maybe Ctx.empty (`Ctx.singleton` Forall [] a) mx
        return $ Module cmdb (ctx1 `Ctx.union` ctxX `Ctx.union` ctx2)

  -- In all other cases, there can no longer be any definitions in the
  -- term, so delegate to 'infer'.
  _anyOtherTerm -> trivMod <$> infer s

-- | Infer the type of a term which does not contain definitions.
infer :: Syntax -> Infer UType
infer s@(Syntax l t) = (`catchError` addLocToTypeErr s) $ case t of
  TUnit -> return UTyUnit
  TConst c -> instantiate $ inferConst c
  TDir _ -> return UTyDir
  TInt _ -> return UTyInt
  TAntiInt _ -> return UTyInt
  TString _ -> return UTyString
  TAntiString _ -> return UTyString
  TBool _ -> return UTyBool
  TRobot _ -> return UTyRobot
  -- We should never encounter a TRef since they do not show up in
  -- surface syntax, only as values while evaluating (*after*
  -- typechecking).
  TRef _ -> throwError $ CantInfer l t
  -- To infer the type of a pair, just infer both components.
  SPair t1 t2 -> UTyProd <$> infer t1 <*> infer t2
  -- if t : ty, then  {t} : {ty}.
  -- Note that in theory, if the @Maybe Var@ component of the @SDelay@
  -- is @Just@, we should typecheck the body under a context extended
  -- with a type binding for the variable, and ensure that the type of
  -- the variable is the same as the type inferred for the overall
  -- @SDelay@.  However, we rely on the invariant that such recursive
  -- @SDelay@ nodes are never generated from the surface syntax, only
  -- dynamically at runtime when evaluating recursive let or def expressions,
  -- so we don't have to worry about typechecking them here.
  SDelay _ dt -> UTyDelay <$> infer dt
  -- Just look up variables in the context.
  TVar x -> lookup l x
  -- To infer the type of a lambda if the type of the argument is
  -- provided, just infer the body under an extended context and return
  -- the appropriate function type.
  SLam x (Just argTy) lt -> do
    let uargTy = toU argTy
    resTy <- withBinding x (Forall [] uargTy) $ infer lt
    return $ UTyFun uargTy resTy

  -- If the type of the argument is not provided, create a fresh
  -- unification variable for it and proceed.
  SLam x Nothing lt -> do
    argTy <- fresh
    resTy <- withBinding x (Forall [] argTy) $ infer lt
    return $ UTyFun argTy resTy

  -- To infer the type of an application:
  SApp f x -> do
    -- Infer the type of the left-hand side and make sure it has a function type.
    fTy <- infer f
    (ty1, ty2) <- decomposeFunTy fTy

    -- Then check that the argument has the right type.
    check x ty1 `catchError` addLocToTypeErr x
    return ty2

  -- We can infer the type of a let whether a type has been provided for
  -- the variable or not.
  SLet _ x Nothing t1 t2 -> do
    xTy <- fresh
    uty <- withBinding x (Forall [] xTy) $ infer t1
    xTy =:= uty
    upty <- generalize uty
    withBinding x upty $ infer t2
  SLet _ x (Just pty) t1 t2 -> do
    let upty = toU pty
    -- If an explicit polytype has been provided, skolemize it and check
    -- definition and body under an extended context.
    uty <- skolemize upty
    resTy <- withBinding x upty $ do
      check t1 uty `catchError` addLocToTypeErr t1
      infer t2
    -- Make sure no skolem variables have escaped.
    ask >>= mapM_ noSkolems
    return resTy
  SDef {} -> throwError $ DefNotTopLevel l t
  SBind mx c1 c2 -> do
    ty1 <- infer c1
    a <- decomposeCmdTy ty1
    ty2 <- maybe id (`withBinding` Forall [] a) mx $ infer c2
    _ <- decomposeCmdTy ty2
    return ty2
 where
  noSkolems :: UPolytype -> Infer ()
  noSkolems (Forall xs upty) = do
    upty' <- applyBindings upty
    let tyvs =
          ucata
            (const S.empty)
            (\case TyVarF v -> S.singleton v; f -> fold f)
            upty'
        ftyvs = tyvs `S.difference` S.fromList xs
    unless (S.null ftyvs) $
      throwError $ EscapedSkolem l (head (S.toList ftyvs))

addLocToTypeErr :: Syntax -> TypeErr -> Infer a
addLocToTypeErr s te = case te of
  Mismatch NoLoc a b -> throwError $ Mismatch (sLoc s) a b
  _ -> throwError te

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
inferConst c = toU $ case c of
  Wait -> [tyQ| int -> cmd () |]
  Noop -> [tyQ| cmd () |]
  Selfdestruct -> [tyQ| cmd () |]
  Move -> [tyQ| cmd () |]
  Turn -> [tyQ| dir -> cmd () |]
  Grab -> [tyQ| cmd string |]
  Place -> [tyQ| string -> cmd () |]
  Give -> [tyQ| robot -> string -> cmd () |]
  Install -> [tyQ| robot -> string -> cmd () |]
  Make -> [tyQ| string -> cmd () |]
  Has -> [tyQ| string -> cmd bool |]
  Count -> [tyQ| string -> cmd int |]
  Reprogram -> [tyQ| robot -> {cmd a} -> cmd () |]
  Build -> [tyQ| {cmd a} -> cmd robot |]
  Drill -> [tyQ| dir -> cmd () |]
  Salvage -> [tyQ| cmd () |]
  Say -> [tyQ| string -> cmd () |]
  Log -> [tyQ| string -> cmd () |]
  View -> [tyQ| robot -> cmd () |]
  Appear -> [tyQ| string -> cmd () |]
  Create -> [tyQ| string -> cmd () |]
  Whereami -> [tyQ| cmd (int * int) |]
  Blocked -> [tyQ| cmd bool |]
  Scan -> [tyQ| dir -> cmd (() + string) |]
  Upload -> [tyQ| robot -> cmd () |]
  Ishere -> [tyQ| string -> cmd bool |]
  Self -> [tyQ| robot |]
  Parent -> [tyQ| robot |]
  Base -> [tyQ| robot |]
  Whoami -> [tyQ| cmd string |]
  Setname -> [tyQ| string -> cmd () |]
  Random -> [tyQ| int -> cmd int |]
  Run -> [tyQ| string -> cmd () |]
  If -> [tyQ| bool -> {a} -> {a} -> a |]
  Inl -> [tyQ| a -> a + b |]
  Inr -> [tyQ| b -> a + b |]
  Case -> [tyQ|a + b -> (a -> c) -> (b -> c) -> c |]
  Fst -> [tyQ| a * b -> a |]
  Snd -> [tyQ| a * b -> b |]
  Force -> [tyQ| {a} -> a |]
  Return -> [tyQ| a -> cmd a |]
  Try -> [tyQ| {cmd a} -> {cmd a} -> cmd a |]
  Undefined -> [tyQ| a |]
  ErrorStr -> [tyQ| string -> a |]
  Not -> [tyQ| bool -> bool |]
  Neg -> [tyQ| int -> int |]
  Eq -> cmpBinT
  Neq -> cmpBinT
  Lt -> cmpBinT
  Gt -> cmpBinT
  Leq -> cmpBinT
  Geq -> cmpBinT
  And -> [tyQ| bool -> bool -> bool|]
  Or -> [tyQ| bool -> bool -> bool|]
  Add -> arithBinT
  Sub -> arithBinT
  Mul -> arithBinT
  Div -> arithBinT
  Exp -> arithBinT
  Format -> [tyQ| a -> string |]
  Concat -> [tyQ| string -> string -> string |]
  AppF -> [tyQ| (a -> b) -> a -> b |]
  Teleport -> [tyQ| robot -> (int * int) -> cmd () |]
  As -> [tyQ| robot -> {cmd a} -> cmd a |]
  RobotNamed -> [tyQ| string -> cmd robot |]
  RobotNumbered -> [tyQ| int -> cmd robot |]
  Knows -> [tyQ| string -> cmd bool |]
 where
  cmpBinT = [tyQ| a -> a -> bool |]
  arithBinT = [tyQ| int -> int -> int |]

-- | @check t ty@ checks that @t@ has type @ty@.
check :: Syntax -> UType -> Infer ()
check t ty = do
  ty' <- infer t
  _ <- ty =:= ty'
  return ()
