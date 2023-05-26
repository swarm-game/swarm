{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- For 'Ord IntVar' instance

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Type inference for the Swarm language.  For the approach used here,
-- see
-- https://byorgey.wordpress.com/2021/09/08/implementing-hindley-milner-with-the-unification-fd-library/ .
module Swarm.Language.Typecheck (
  -- * Type errors
  TypeErr (..),
  InvalidAtomicReason (..),
  getTypeErrSrcLoc,

  -- * Inference monad
  TC,
  runTC,
  lookup,
  fresh,

  -- * Unification
  substU,
  expect,
  (=:=),
  HasBindings (..),
  instantiate,
  skolemize,
  generalize,

  -- * Type inference
  inferTop,
  inferModule,
  infer,
  inferConst,
  check,
  decomposeCmdTy,
  decomposeFunTy,
  isSimpleUType,
) where

import Control.Arrow ((***))
import Control.Category ((>>>))
import Control.Lens ((^.))
import Control.Lens.Indexed (itraverse)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Unification hiding (applyBindings, (=:=))
import Control.Unification qualified as U
import Control.Unification.IntVar
import Data.Data (Data, gmapM)
import Data.Foldable (fold)
import Data.Functor.Identity
import Data.Generics (mkM)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set, (\\))
import Data.Set qualified as S
import Data.Text qualified as T
import Swarm.Language.Context hiding (lookup)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Module
import Swarm.Language.Parse.QQ (tyQ)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck.Unify
import Swarm.Language.Types
import Prelude hiding (lookup)

------------------------------------------------------------
-- Type checking monad

-- | The concrete monad used for type checking.  'IntBindingT' is a
--   monad transformer provided by the @unification-fd@ library which
--   supports various operations such as generating fresh variables
--   and unifying things.
type TC = ReaderT UCtx (ExceptT TypeErr (IntBindingT TypeF Identity))

-- | Run a top-level inference computation, returning either a
--   'TypeErr' or a fully resolved 'TModule'.
runTC :: TCtx -> TC UModule -> Either TypeErr TModule
runTC ctx =
  (>>= applyBindings)
    >>> ( >>=
            \(Module u uctx) ->
              Module
                <$> mapM (fmap fromU . generalize) u
                <*> pure (fromU uctx)
        )
    >>> flip runReaderT (toU ctx)
    >>> runExceptT
    >>> evalIntBindingT
    >>> runIdentity

-- | Look up a variable in the ambient type context, either throwing
--   an 'UnboundVar' error if it is not found, or opening its
--   associated 'UPolytype' with fresh unification variables via
--   'instantiate'.
lookup :: SrcLoc -> Var -> TC UType
lookup loc x = do
  ctx <- ask
  maybe (throwError $ UnboundVar loc x) instantiate (Ctx.lookup x ctx)

-- | Add a source location to a type error and re-throw it.
addLocToTypeErr :: Syntax' ty -> TypeErr -> TC a
addLocToTypeErr s te = case te of
  UnifyErr NoLoc a b -> throwError $ UnifyErr (s ^. sLoc) a b
  Mismatch NoLoc mt a b -> throwError $ Mismatch (s ^. sLoc) mt a b
  _ -> throwError te

------------------------------------------------------------
-- Dealing with variables: free variables, fresh variables,
-- substitution

-- | @unification-fd@ does not provide an 'Ord' instance for 'IntVar',
--   so we must provide our own, in order to be able to store
--   'IntVar's in a 'Set'.
deriving instance Ord IntVar

-- | A class for getting the free unification variables of a thing.
class FreeVars a where
  freeVars :: a -> TC (Set IntVar)

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
fresh :: TC UType
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

-- | @expect t expTy actTy@ expects that term @t@ has type @expTy@,
--   where it actually has type @actTy@.  Ensure those types are the
--   same.
expect :: Maybe Syntax -> UType -> UType -> TC UType
expect ms expected actual = case unifyCheck expected actual of
  Apart -> throwError $ Mismatch NoLoc ms expected actual
  Equal -> return expected
  MightUnify -> lift $ expected U.=:= actual

-- | Constrain two types to be equal, first with a quick-and-dirty
--   check to see whether we know for sure they either are or cannot
--   be equal, generating an equality constraint for the unified as a
--   last resort.
(=:=) :: UType -> UType -> TC UType
(=:=) = expect Nothing

-- | @unification-fd@ provides a function 'U.applyBindings' which
--   fully substitutes for any bound unification variables (for
--   efficiency, it does not perform such substitution as it goes
--   along).  The 'HasBindings' class is for anything which has
--   unification variables in it and to which we can usefully apply
--   'U.applyBindings'.
class HasBindings u where
  applyBindings :: u -> TC u

instance HasBindings UType where
  applyBindings = lift . U.applyBindings

instance HasBindings UPolytype where
  applyBindings (Forall xs u) = Forall xs <$> applyBindings u

instance HasBindings UCtx where
  applyBindings = mapM applyBindings

instance (HasBindings u, Data u) => HasBindings (Term' u) where
  applyBindings = gmapM (mkM (applyBindings @(Syntax' u)))

instance (HasBindings u, Data u) => HasBindings (Syntax' u) where
  applyBindings (Syntax' l t u) = Syntax' l <$> applyBindings t <*> applyBindings u

instance HasBindings UModule where
  applyBindings (Module u uctx) = Module <$> applyBindings u <*> applyBindings uctx

------------------------------------------------------------
-- Converting between mono- and polytypes

-- | To 'instantiate' a 'UPolytype', we generate a fresh unification
--   variable for each variable bound by the `Forall`, and then
--   substitute them throughout the type.
instantiate :: UPolytype -> TC UType
instantiate (Forall xs uty) = do
  xs' <- mapM (const fresh) xs
  return $ substU (M.fromList (zip (map Left xs) xs')) uty

-- | 'skolemize' is like 'instantiate', except we substitute fresh
--   /type/ variables instead of unification variables.  Such
--   variables cannot unify with anything other than themselves.  This
--   is used when checking something with a polytype explicitly
--   specified by the user.
skolemize :: UPolytype -> TC UType
skolemize (Forall xs uty) = do
  xs' <- mapM (const fresh) xs
  return $ substU (M.fromList (zip (map Left xs) (map toSkolem xs'))) uty
 where
  toSkolem (UVar v) = UTyVar (mkVarName "s" v)
  toSkolem x = error $ "Impossible! Non-UVar in skolemize.toSkolem: " ++ show x

-- | 'generalize' is the opposite of 'instantiate': add a 'Forall'
--   which closes over all free type and unification variables.
--
--   Pick nice type variable names instead of reusing whatever fresh
--   names happened to be used for the free variables.
generalize :: UType -> TC UPolytype
generalize uty = do
  uty' <- applyBindings uty
  ctx <- ask
  tmfvs <- freeVars uty'
  ctxfvs <- freeVars ctx
  let fvs = S.toList $ tmfvs \\ ctxfvs
      alphabet = ['a' .. 'z']
      -- Infinite supply of pretty names a, b, ..., z, a0, ... z0, a1, ... z1, ...
      prettyNames = map T.pack (map (: []) alphabet ++ [x : show n | n <- [0 :: Int ..], x <- alphabet])
      -- Associate each free variable with a new pretty name
      renaming = zip fvs prettyNames
  return $
    Forall
      (map snd renaming)
      (substU (M.fromList . map (Right *** UTyVar) $ renaming) uty')

------------------------------------------------------------
-- Type errors

-- | Errors that can occur during type checking.  The idea is that
--   each error carries information that can be used to help explain
--   what went wrong (though the amount of information carried can and
--   should be very much improved in the future); errors can then
--   separately be pretty-printed to display them to the user.
data TypeErr
  = -- | An undefined variable was encountered.
    UnboundVar SrcLoc Var
  | -- | A Skolem variable escaped its local context.
    EscapedSkolem SrcLoc Var
  | -- | Occurs check failure, i.e. infinite type.
    Infinite IntVar UType
  | -- | Error generated by the unifier.
    UnifyErr SrcLoc (TypeF UType) (TypeF UType)
  | -- | Type mismatch caught by 'unifyCheck'.  The given term was
    --   expected to have a certain type, but has a different type
    --   instead.
    Mismatch SrcLoc (Maybe Syntax) UType UType -- expected, actual
  | -- | A definition was encountered not at the top level.
    DefNotTopLevel SrcLoc Term
  | -- | A term was encountered which we cannot infer the type of.
    --   This should never happen.
    CantInfer SrcLoc Term
  | -- | We can't infer the type of a record projection @r.x@ if we
    --   don't concretely know the type of the record @r@.
    CantInferProj SrcLoc Term
  | -- | An attempt to project out a nonexistent field
    UnknownProj SrcLoc Var Term
  | -- | An invalid argument was provided to @atomic@.
    InvalidAtomic SrcLoc InvalidAtomicReason Term
  deriving (Show)

-- | Various reasons the body of an @atomic@ might be invalid.
data InvalidAtomicReason
  = -- | The argument has too many tangible commands.
    TooManyTicks Int
  | -- | The argument uses some way to duplicate code: @def@, @let@, or lambda.
    AtomicDupingThing
  | -- | The argument referred to a variable with a non-simple type.
    NonSimpleVarType Var UPolytype
  | -- | The argument had a nested @atomic@
    NestedAtomic
  | -- | The argument contained a long command
    LongConst
  deriving (Show)

instance Fallible TypeF IntVar TypeErr where
  occursFailure = Infinite
  mismatchFailure = UnifyErr NoLoc

getTypeErrSrcLoc :: TypeErr -> Maybe SrcLoc
getTypeErrSrcLoc te = case te of
  UnboundVar l _ -> Just l
  EscapedSkolem l _ -> Just l
  Infinite _ _ -> Nothing
  UnifyErr l _ _ -> Just l
  Mismatch l _ _ _ -> Just l
  DefNotTopLevel l _ -> Just l
  CantInfer l _ -> Just l
  CantInferProj l _ -> Just l
  UnknownProj l _ _ -> Just l
  InvalidAtomic l _ _ -> Just l

------------------------------------------------------------
-- Type decomposition

-- | Decompose a type that is supposed to be a delay type.
decomposeDelayTy :: UType -> TC UType
decomposeDelayTy (UTyDelay a) = return a
decomposeDelayTy ty = do
  a <- fresh
  _ <- ty =:= UTyDelay a
  return a

-- | Decompose a type that is supposed to be a command type.
decomposeCmdTy :: UType -> TC UType
decomposeCmdTy (UTyCmd a) = return a
decomposeCmdTy ty = do
  a <- fresh
  _ <- ty =:= UTyCmd a
  return a

-- | Decompose a type that is supposed to be a function type.
decomposeFunTy :: UType -> TC (UType, UType)
decomposeFunTy (UTyFun ty1 ty2) = return (ty1, ty2)
decomposeFunTy ty = do
  ty1 <- fresh
  ty2 <- fresh
  _ <- ty =:= UTyFun ty1 ty2
  return (ty1, ty2)

-- | Decompose a type that is supposed to be a product type.
decomposeProdTy :: UType -> TC (UType, UType)
decomposeProdTy (UTyProd ty1 ty2) = return (ty1, ty2)
decomposeProdTy ty = do
  ty1 <- fresh
  ty2 <- fresh
  _ <- ty =:= UTyProd ty1 ty2
  return (ty1, ty2)

-- | XXX
decomposeRcdTy :: UType -> TC (Map Var UType)
decomposeRcdTy (UTyRcd m) = return m
decomposeRcdTy _ = throwError undefined -- XXX

------------------------------------------------------------
-- Type inference / checking

-- | Top-level type inference function: given a context of definition
--   types and a top-level term, either return a type error or its
--   type as a 'TModule'.
inferTop :: TCtx -> Syntax -> Either TypeErr TModule
inferTop ctx = runTC ctx . inferModule

-- XXX should this be checkModule?  How does it get used?

-- | Infer the signature of a top-level expression which might
--   contain definitions.
inferModule :: Syntax -> TC UModule
inferModule s@(Syntax l t) = (`catchError` addLocToTypeErr s) $ case t of
  -- For definitions with no type signature, make up a fresh type
  -- variable for the body, infer the body under an extended context,
  -- and unify the two.  Then generalize the type and return an
  -- appropriate context.
  SDef r x Nothing t1 -> do
    xTy <- fresh
    t1' <- withBinding (lvVar x) (Forall [] xTy) $ infer t1
    _ <- xTy =:= t1' ^. sType
    pty <- generalize (t1' ^. sType)
    return $ Module (Syntax' l (SDef r x Nothing t1') (UTyCmd UTyUnit)) (singleton (lvVar x) pty)

  -- If a (poly)type signature has been provided, skolemize it and
  -- check the definition.
  SDef r x (Just pty) t1 -> do
    let upty = toU pty
    uty <- skolemize upty
    t1' <- withBinding (lvVar x) upty $ check t1 uty
    return $ Module (Syntax' l (SDef r x (Just pty) t1') (UTyCmd UTyUnit)) (singleton (lvVar x) upty)

  -- To handle a 'TBind', infer the types of both sides, combining the
  -- returned modules appropriately.  Have to be careful to use the
  -- correct context when checking the right-hand side in particular.
  SBind mx c1 c2 -> do
    -- First, infer the left side.
    Module c1' ctx1 <- inferModule c1
    a <- decomposeCmdTy (c1' ^. sType)

    -- Now infer the right side under an extended context: things in
    -- scope on the right-hand side include both any definitions
    -- created by the left-hand side, as well as a variable as in @x
    -- <- c1; c2@.  The order of extensions here matters: in theory,
    -- c1 could define something with the same name as x, in which
    -- case the bound x should shadow the defined one; hence, we apply
    -- that binding /after/ (i.e. /within/) the application of @ctx1@.
    withBindings ctx1 $
      maybe id ((`withBinding` Forall [] a) . lvVar) mx $ do
        Module c2' ctx2 <- inferModule c2

        -- We don't actually need the result type since we're just
        -- going to return the entire type, but it's important to
        -- ensure it's a command type anyway.  Otherwise something
        -- like 'move; 3' would be accepted with type int.
        _ <- decomposeCmdTy (c2' ^. sType)

        -- Ctx.union is right-biased, so ctx1 `union` ctx2 means later
        -- definitions will shadow previous ones.  Include the binder
        -- (if any) as well, since binders are made available at the top
        -- level, just like definitions. e.g. if the user writes `r <- build {move}`,
        -- then they will be able to refer to r again later.
        let ctxX = maybe Ctx.empty ((`Ctx.singleton` Forall [] a) . lvVar) mx
        return $
          Module
            (Syntax' l (SBind mx c1' c2') (c2' ^. sType))
            (ctx1 `Ctx.union` ctxX `Ctx.union` ctx2)

  -- In all other cases, there can no longer be any definitions in the
  -- term, so delegate to 'infer'.
  _anyOtherTerm -> trivMod <$> infer s

-- | Infer the type of a term which does not contain definitions,
--   returning a type-annotated term.
--
--   The only cases explicitly handled in 'infer' are those where
--   pushing an expected type down into the term can't possibly help,
--   e.g. most primitives, XXX
--
--   For everything else we prefer 'check' because it can often result
--   in better and more localized type error messages.
infer :: Syntax -> TC (Syntax' UType)
infer s@(Syntax l t) = (`catchError` addLocToTypeErr s) $ case t of
  -- Primitives, i.e. things for which we immediately know the only
  -- possible correct type, and knowing an expected type would provide
  -- no extra information.
  TUnit -> return $ Syntax' l TUnit UTyUnit
  TConst c -> Syntax' l (TConst c) <$> (instantiate . toU $ inferConst c)
  TDir d -> return $ Syntax' l (TDir d) UTyDir
  TInt n -> return $ Syntax' l (TInt n) UTyInt
  TAntiInt x -> return $ Syntax' l (TAntiInt x) UTyInt
  TText x -> return $ Syntax' l (TText x) UTyText
  TAntiText x -> return $ Syntax' l (TAntiText x) UTyText
  TBool b -> return $ Syntax' l (TBool b) UTyBool
  TRobot r -> return $ Syntax' l (TRobot r) UTyActor
  TRequireDevice d -> return $ Syntax' l (TRequireDevice d) (UTyCmd UTyUnit)
  TRequire n d -> return $ Syntax' l (TRequire n d) (UTyCmd UTyUnit)
  SRequirements x t1 -> do
    t1' <- infer t1
    return $ Syntax' l (SRequirements x t1') (UTyCmd UTyUnit)

  -- We should never encounter a TRef since they do not show up in
  -- surface syntax, only as values while evaluating (*after*
  -- typechecking).
  TRef _ -> throwError $ CantInfer l t
  -- XXX move atomic to check.
  -- We need a special case for checking the argument to 'atomic'.
  -- 'atomic t' has the same type as 't', which must have a type of
  -- the form 'cmd a'.  't' must also be syntactically free of
  -- variables.

  TConst Atomic :$: at -> inferAtomic True Atomic at
  TConst Instant :$: at -> inferAtomic False Instant at
  -- Just look up variables in the context.
  TVar x -> Syntax' l (TVar x) <$> lookup l x
  -- To infer the type of an application:
  SApp f x -> do
    -- Infer the type of the left-hand side and make sure it has a function type.
    f' <- infer f
    (ty1, ty2) <- decomposeFunTy (f' ^. sType)

    -- Then check that the argument has the right type.
    x' <- check x ty1 `catchError` addLocToTypeErr x
    return $ Syntax' l (SApp f' x') ty2

  -- XXX move SLet to check.  If variable type annotation has been
  -- provided we can call check on t1, otherwise infer; then call
  -- check on t2.

  -- We can infer the type of a let whether a type has been provided for
  -- the variable or not.
  SLet r x Nothing t1 t2 -> do
    xTy <- fresh
    t1' <- withBinding (lvVar x) (Forall [] xTy) $ infer t1
    let uty = t1' ^. sType
    _ <- xTy =:= uty
    upty <- generalize uty
    t2' <- withBinding (lvVar x) upty $ infer t2
    return $ Syntax' l (SLet r x Nothing t1' t2') (t2' ^. sType)
  SLet r x (Just pty) t1 t2 -> do
    let upty = toU pty
    -- If an explicit polytype has been provided, skolemize it and check
    -- definition and body under an extended context.
    uty <- skolemize upty
    (t1', t2') <- withBinding (lvVar x) upty $ do
      (,)
        <$> check t1 uty
        `catchError` addLocToTypeErr t1
        <*> infer t2
    -- Make sure no skolem variables have escaped.
    ask >>= mapM_ noSkolems
    return $ Syntax' l (SLet r x (Just pty) t1' t2') (t2' ^. sType)
  SDef {} -> throwError $ DefNotTopLevel l t
  SBind mx c1 c2 -> do
    -- XXX move SBind to check
    c1' <- infer c1
    a <- decomposeCmdTy (c1' ^. sType)
    c2' <- maybe id ((`withBinding` Forall [] a) . lvVar) mx $ infer c2
    _ <- decomposeCmdTy (c2' ^. sType)
    return $ Syntax' l (SBind mx c1' c2') (c2' ^. sType)
  SProj t1 x -> do
    t1' <- infer t1
    case t1' ^. sType of
      UTyRcd m -> case M.lookup x m of
        Just xTy -> return $ Syntax' l (SProj t1' x) xTy
        Nothing -> throwError $ UnknownProj l x (SProj t1 x)
      _ -> throwError $ CantInferProj l (SProj t1 x)
  SRcd m -> do
    m' <- itraverse (\x -> infer . fromMaybe (STerm (TVar x))) m
    return $ Syntax' l (SRcd (Just <$> m')) (UTyRcd (fmap (^. sType) m'))
  SAnnotate c pty -> do
    let upty = toU pty
    -- Typecheck against skolemized polytype.
    uty <- skolemize upty
    _ <- check c uty `catchError` addLocToTypeErr c
    -- Make sure no skolem variables have escaped.
    ask >>= mapM_ noSkolems
    -- If check against skolemized polytype is successful,
    -- instantiate polytype with unification variables.
    -- Free variables should be able to unify with anything in
    -- following inference steps.
    iuty <- instantiate upty
    c'' <- check c iuty `catchError` addLocToTypeErr c
    return $ Syntax' l (SAnnotate c'' pty) (c'' ^. sType)

  -- Fallback: to infer the type of anything else, make up a fresh unification
  -- variable for its type and check against it.
  _ -> do
    sTy <- fresh
    check s sTy
 where
  noSkolems :: UPolytype -> TC ()
  noSkolems (Forall xs upty) = do
    upty' <- applyBindings upty
    let tyvs =
          ucata
            (const S.empty)
            (\case TyVarF v -> S.singleton v; f -> fold f)
            upty'
        ftyvs = tyvs `S.difference` S.fromList xs
    unless (S.null ftyvs) $
      throwError $
        EscapedSkolem l (head (S.toList ftyvs))

  inferAtomic :: Bool -> Const -> Syntax -> TC (Syntax' UType)
  inferAtomic validateTickBudget constName at = do
    argTy <- fresh
    at' <- check at (UTyCmd argTy)
    atomic' <- infer (Syntax l (TConst constName))
    -- It's important that we typecheck the subterm @at@ *before* we
    -- check that it is a valid argument to @atomic@: this way we can
    -- ensure that we have already inferred the types of any variables
    -- referenced.
    when validateTickBudget $
      validAtomic at
    return $ Syntax' l (SApp atomic' at') (at' ^. sType)

-- | Infer the type of a constant.
inferConst :: Const -> Polytype
inferConst c = case c of
  Wait -> [tyQ| int -> cmd unit |]
  Noop -> [tyQ| cmd unit |]
  Selfdestruct -> [tyQ| cmd unit |]
  Move -> [tyQ| cmd unit |]
  Push -> [tyQ| cmd unit |]
  Stride -> [tyQ| int -> cmd unit |]
  Turn -> [tyQ| dir -> cmd unit |]
  Grab -> [tyQ| cmd text |]
  Harvest -> [tyQ| cmd text |]
  Place -> [tyQ| text -> cmd unit |]
  Give -> [tyQ| actor -> text -> cmd unit |]
  Equip -> [tyQ| text -> cmd unit |]
  Unequip -> [tyQ| text -> cmd unit |]
  Make -> [tyQ| text -> cmd unit |]
  Has -> [tyQ| text -> cmd bool |]
  Equipped -> [tyQ| text -> cmd bool |]
  Count -> [tyQ| text -> cmd int |]
  Reprogram -> [tyQ| actor -> {cmd a} -> cmd unit |]
  Build -> [tyQ| {cmd a} -> cmd actor |]
  Drill -> [tyQ| dir -> cmd (unit + text) |]
  Salvage -> [tyQ| cmd unit |]
  Say -> [tyQ| text -> cmd unit |]
  Listen -> [tyQ| cmd text |]
  Log -> [tyQ| text -> cmd unit |]
  View -> [tyQ| actor -> cmd unit |]
  Appear -> [tyQ| text -> cmd unit |]
  Create -> [tyQ| text -> cmd unit |]
  Halt -> [tyQ| actor -> cmd unit |]
  Time -> [tyQ| cmd int |]
  Scout -> [tyQ| dir -> cmd bool |]
  Whereami -> [tyQ| cmd (int * int) |]
  Detect -> [tyQ| text -> ((int * int) * (int * int)) -> cmd (unit + (int * int)) |]
  Resonate -> [tyQ| text -> ((int * int) * (int * int)) -> cmd int |]
  Sniff -> [tyQ| text -> cmd int |]
  Chirp -> [tyQ| text -> cmd dir |]
  Watch -> [tyQ| dir -> cmd unit |]
  Surveil -> [tyQ| (int * int) -> cmd unit |]
  Heading -> [tyQ| cmd dir |]
  Blocked -> [tyQ| cmd bool |]
  Scan -> [tyQ| dir -> cmd (unit + text) |]
  Upload -> [tyQ| actor -> cmd unit |]
  Ishere -> [tyQ| text -> cmd bool |]
  Isempty -> [tyQ| cmd bool |]
  Self -> [tyQ| actor |]
  Parent -> [tyQ| actor |]
  Base -> [tyQ| actor |]
  Meet -> [tyQ| cmd (unit + actor) |]
  MeetAll -> [tyQ| (b -> actor -> cmd b) -> b -> cmd b |]
  Whoami -> [tyQ| cmd text |]
  Setname -> [tyQ| text -> cmd unit |]
  Random -> [tyQ| int -> cmd int |]
  Run -> [tyQ| text -> cmd unit |]
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
  Fail -> [tyQ| text -> a |]
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
  Format -> [tyQ| a -> text |]
  Concat -> [tyQ| text -> text -> text |]
  Chars -> [tyQ| text -> int |]
  Split -> [tyQ| int -> text -> (text * text) |]
  CharAt -> [tyQ| int -> text -> int |]
  ToChar -> [tyQ| int -> text |]
  AppF -> [tyQ| (a -> b) -> a -> b |]
  Swap -> [tyQ| text -> cmd text |]
  Atomic -> [tyQ| cmd a -> cmd a |]
  Instant -> [tyQ| cmd a -> cmd a |]
  Key -> [tyQ| text -> key |]
  InstallKeyHandler -> [tyQ| text -> (key -> cmd unit) -> cmd unit |]
  Teleport -> [tyQ| actor -> (int * int) -> cmd unit |]
  As -> [tyQ| actor -> {cmd a} -> cmd a |]
  RobotNamed -> [tyQ| text -> cmd actor |]
  RobotNumbered -> [tyQ| int -> cmd actor |]
  Knows -> [tyQ| text -> cmd bool |]
 where
  cmpBinT = [tyQ| a -> a -> bool |]
  arithBinT = [tyQ| int -> int -> int |]

-- | @check t ty@ checks that @t@ has type @ty@, returning a
--   type-annotated AST if so.
--
--   We try to stay in checking mode as far as possible, decomposing
--   the expected type as we go and pushing it through the recursion.
check :: Syntax -> UType -> TC (Syntax' UType)
check s@(Syntax l t) expected = (`catchError` addLocToTypeErr s) $ case t of
  -- if t : ty, then  {t} : {ty}.
  -- Note that in theory, if the @Maybe Var@ component of the @SDelay@
  -- is @Just@, we should typecheck the body under a context extended
  -- with a type binding for the variable, and ensure that the type of
  -- the variable is the same as the type inferred for the overall
  -- @SDelay@.  However, we rely on the invariant that such recursive
  -- @SDelay@ nodes are never generated from the surface syntax, only
  -- dynamically at runtime when evaluating recursive let or def expressions,
  -- so we don't have to worry about typechecking them here.
  SDelay d s1 -> do
    ty1 <- decomposeDelayTy expected
    s1' <- check s1 ty1
    return $ Syntax' l (SDelay d s1') (UTyDelay ty1)
  SPair s1 s2 -> do
    (ty1, ty2) <- decomposeProdTy expected
    s1' <- check s1 ty1
    s2' <- check s2 ty2
    return $ Syntax' l (SPair s1' s2') (UTyProd ty1 ty2)
  SLam x xTy body -> do
    (argTy, resTy) <- decomposeFunTy expected
    _ <- maybe (return argTy) (=:= argTy) (toU xTy)
    body' <- withBinding (lvVar x) (Forall [] argTy) $ check body resTy
    return $ Syntax' l (SLam x xTy body') (UTyFun argTy resTy)
  -- SRcd m -> do
  --   fieldTys <- decomposeRcdTy expected
  --   -- XXX
  --   -- ensure keys in m, fieldTys match.
  --   -- If so, check each field vs its expected type.
  --   -- m' <- itraverse (\x -> infer . fromMaybe (STerm (TVar x))) m
  --   -- return $ Syntax' l (SRcd (Just <$> m')) (UTyRcd (fmap (^. sType) m'))
  --   undefined

  -- Fallback: switch into inference mode, and check that the type we
  -- get is what we expected.
  _ -> do
    Syntax' l' t' actual <- infer s
    Syntax' l' t' <$> expect (Just s) expected actual

------------------------------------------------------------
-- Special atomic checking

-- | Ensure a term is a valid argument to @atomic@.  Valid arguments
--   may not contain @def@, @let@, or lambda. Any variables which are
--   referenced must have a primitive, first-order type such as
--   @text@ or @int@ (in particular, no functions, @cmd@, or
--   @delay@).  We simply assume that any locally bound variables are
--   OK without checking their type: the only way to bind a variable
--   locally is with a binder of the form @x <- c1; c2@, where @c1@ is
--   some primitive command (since we can't refer to external
--   variables of type @cmd a@).  If we wanted to do something more
--   sophisticated with locally bound variables we would have to
--   inline this analysis into typechecking proper, instead of having
--   it be a separate, out-of-band check.
--
--   The goal is to ensure that any argument to @atomic@ is guaranteed
--   to evaluate and execute in some small, finite amount of time, so
--   that it's impossible to write a term which runs atomically for an
--   indefinite amount of time and freezes the rest of the game.  Of
--   course, nothing prevents one from writing a large amount of code
--   inside an @atomic@ block; but we want the execution time to be
--   linear in the size of the code.
--
--   We also ensure that the atomic block takes at most one tick,
--   i.e. contains at most one tangible command. For example, @atomic
--   (move; move)@ is invalid, since that would allow robots to move
--   twice as fast as usual by doing both actions in one tick.
validAtomic :: Syntax -> TC ()
validAtomic s@(Syntax l t) = do
  n <- analyzeAtomic S.empty s
  when (n > 1) $ throwError (InvalidAtomic l (TooManyTicks n) t)

-- | Analyze an argument to @atomic@: ensure it contains no nested
--   atomic blocks and no references to external variables, and count
--   how many tangible commands it will execute.
analyzeAtomic :: Set Var -> Syntax -> TC Int
analyzeAtomic locals (Syntax l t) = case t of
  -- Literals, primitives, etc. that are fine and don't require a tick
  -- to evaluate
  TUnit {} -> return 0
  TDir {} -> return 0
  TInt {} -> return 0
  TAntiInt {} -> return 0
  TText {} -> return 0
  TAntiText {} -> return 0
  TBool {} -> return 0
  TRobot {} -> return 0
  TRequireDevice {} -> return 0
  TRequire {} -> return 0
  SRequirements {} -> return 0
  -- Constants.
  TConst c
    -- Nested 'atomic' is not allowed.
    | c == Atomic -> throwError $ InvalidAtomic l NestedAtomic t
    -- We cannot allow long commands (commands that may require more
    -- than one tick to execute) since that could freeze the game.
    | isLong c -> throwError $ InvalidAtomic l LongConst t
    -- Otherwise, return 1 or 0 depending on whether the command is
    -- tangible.
    | otherwise -> return $ if isTangible c then 1 else 0
  -- Special case for if: number of tangible commands is the *max* of
  -- the branches instead of the sum, since exactly one of them will be
  -- executed.
  TConst If :$: tst :$: thn :$: els ->
    (+) <$> analyzeAtomic locals tst <*> (max <$> analyzeAtomic locals thn <*> analyzeAtomic locals els)
  -- Pairs, application, and delay are simple: just recurse and sum the results.
  SPair s1 s2 -> (+) <$> analyzeAtomic locals s1 <*> analyzeAtomic locals s2
  SApp s1 s2 -> (+) <$> analyzeAtomic locals s1 <*> analyzeAtomic locals s2
  SDelay _ s1 -> analyzeAtomic locals s1
  -- Bind is similarly simple except that we have to keep track of a local variable
  -- bound in the RHS.
  SBind mx s1 s2 -> (+) <$> analyzeAtomic locals s1 <*> analyzeAtomic (maybe id (S.insert . lvVar) mx locals) s2
  SRcd m -> sum <$> mapM analyzeField (M.assocs m)
   where
    analyzeField :: (Var, Maybe Syntax) -> TC Int
    analyzeField (x, Nothing) = analyzeAtomic locals (STerm (TVar x))
    analyzeField (_, Just s) = analyzeAtomic locals s
  SProj {} -> return 0
  -- Variables are allowed if bound locally, or if they have a simple type.
  TVar x
    | x `S.member` locals -> return 0
    | otherwise -> do
        mxTy <- asks $ Ctx.lookup x
        case mxTy of
          -- If the variable is undefined, return 0 to indicate the
          -- atomic block is valid, because we'd rather have the error
          -- caught by the real name+type checking.
          Nothing -> return 0
          Just xTy -> do
            -- Use applyBindings to make sure that we apply as much
            -- information as unification has learned at this point.  In
            -- theory, continuing to typecheck other terms elsewhere in
            -- the program could give us further information about xTy,
            -- so we might have incomplete information at this point.
            -- However, since variables referenced in an atomic block
            -- must necessarily have simple types, it's unlikely this
            -- will really make a difference.  The alternative, more
            -- "correct" way to do this would be to simply emit some
            -- constraints at this point saying that xTy must be a
            -- simple type, and check later that the constraint holds,
            -- after performing complete type inference.  However, since
            -- the current approach is much simpler, we'll stick with
            -- this until such time as we have concrete examples showing
            -- that the more correct, complex way is necessary.
            xTy' <- applyBindings xTy
            if isSimpleUPolytype xTy'
              then return 0
              else throwError (InvalidAtomic l (NonSimpleVarType x xTy') t)
  -- No lambda, `let` or `def` allowed!
  SLam {} -> throwError (InvalidAtomic l AtomicDupingThing t)
  SLet {} -> throwError (InvalidAtomic l AtomicDupingThing t)
  SDef {} -> throwError (InvalidAtomic l AtomicDupingThing t)
  -- We should never encounter a TRef since they do not show up in
  -- surface syntax, only as values while evaluating (*after*
  -- typechecking).
  TRef {} -> throwError (CantInfer l t)
  -- An explicit type annotation doesn't change atomicity
  SAnnotate s _ -> analyzeAtomic locals s

-- | A simple polytype is a simple type with no quantifiers.
isSimpleUPolytype :: UPolytype -> Bool
isSimpleUPolytype (Forall [] ty) = isSimpleUType ty
isSimpleUPolytype _ = False

-- | A simple type is a sum or product of base types.
isSimpleUType :: UType -> Bool
isSimpleUType = \case
  UTyBase {} -> True
  UTyVar {} -> False
  UTySum ty1 ty2 -> isSimpleUType ty1 && isSimpleUType ty2
  UTyProd ty1 ty2 -> isSimpleUType ty1 && isSimpleUType ty2
  UTyFun {} -> False
  UTyCmd {} -> False
  UTyDelay {} -> False
  -- Make the pattern-match coverage checker happy
  UVar {} -> False
  UTerm {} -> False
