{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
  InvalidAtomicReason (..),
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

import Control.Category ((>>>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Unification hiding (applyBindings, (=:=))
import Control.Unification qualified as U
import Control.Unification.IntVar
import Data.Foldable (fold)
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set, (\\))
import Data.Set qualified as S
import Swarm.Language.Context hiding (lookup)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Parse.QQ (tyQ)
import Swarm.Language.Syntax
import Swarm.Language.Types
import Prelude hiding (lookup)

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
  | -- | An invalid argument was provided to @atomic@.
    InvalidAtomic Location InvalidAtomicReason Term
  deriving (Show)

-- | Various reasons the body of an @atomic@ might be invalid.
data InvalidAtomicReason
  = -- | The arugment has too many tangible commands.
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
  mismatchFailure = Mismatch NoLoc

getTypeErrLocation :: TypeErr -> Maybe Location
getTypeErrLocation te = case te of
  UnboundVar l _ -> Just l
  EscapedSkolem l _ -> Just l
  Infinite _ _ -> Nothing
  Mismatch l _ _ -> Just l
  DefNotTopLevel l _ -> Just l
  CantInfer l _ -> Just l
  InvalidAtomic l _ _ -> Just l

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
  TConst c -> instantiate . toU $ inferConst c
  TDir _ -> return UTyDir
  TInt _ -> return UTyInt
  TAntiInt _ -> return UTyInt
  TText _ -> return UTyText
  TAntiText _ -> return UTyText
  TBool _ -> return UTyBool
  TRobot _ -> return UTyRobot
  -- We should never encounter a TRef since they do not show up in
  -- surface syntax, only as values while evaluating (*after*
  -- typechecking).
  TRef _ -> throwError $ CantInfer l t
  TRequireDevice _ -> return $ UTyCmd UTyUnit
  TRequire _ _ -> return $ UTyCmd UTyUnit
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
  -- We need a special case for checking the argument to 'atomic'.
  -- 'atomic t' has the same type as 't', which must have a type of
  -- the form 'cmd a'.  't' must also be syntactically free of
  -- variables.
  TConst Atomic :$: at -> do
    argTy <- fresh
    check at (UTyCmd argTy)
    -- It's important that we typecheck the subterm @at@ *before* we
    -- check that it is a valid argument to @atomic@: this way we can
    -- ensure that we have already inferred the types of any variables
    -- referenced.
    validAtomic at
    return $ UTyCmd argTy

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
inferConst :: Const -> Polytype
inferConst c = case c of
  Wait -> [tyQ| int -> cmd unit |]
  Noop -> [tyQ| cmd unit |]
  Selfdestruct -> [tyQ| cmd unit |]
  Move -> [tyQ| cmd unit |]
  Turn -> [tyQ| dir -> cmd unit |]
  Grab -> [tyQ| cmd text |]
  Harvest -> [tyQ| cmd text |]
  Place -> [tyQ| text -> cmd unit |]
  Give -> [tyQ| robot -> text -> cmd unit |]
  Install -> [tyQ| robot -> text -> cmd unit |]
  Make -> [tyQ| text -> cmd unit |]
  Has -> [tyQ| text -> cmd bool |]
  Installed -> [tyQ| text -> cmd bool |]
  Count -> [tyQ| text -> cmd int |]
  Reprogram -> [tyQ| robot -> {cmd a} -> cmd unit |]
  Build -> [tyQ| {cmd a} -> cmd robot |]
  Drill -> [tyQ| dir -> cmd unit |]
  Salvage -> [tyQ| cmd unit |]
  Say -> [tyQ| text -> cmd unit |]
  Listen -> [tyQ| cmd text |]
  Log -> [tyQ| text -> cmd unit |]
  View -> [tyQ| robot -> cmd unit |]
  Appear -> [tyQ| text -> cmd unit |]
  Create -> [tyQ| text -> cmd unit |]
  Time -> [tyQ| cmd int |]
  Whereami -> [tyQ| cmd (int * int) |]
  Blocked -> [tyQ| cmd bool |]
  Scan -> [tyQ| dir -> cmd (unit + text) |]
  Upload -> [tyQ| robot -> cmd unit |]
  Ishere -> [tyQ| text -> cmd bool |]
  Self -> [tyQ| robot |]
  Parent -> [tyQ| robot |]
  Base -> [tyQ| robot |]
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
  Infinity -> [tyQ| int |]
  Format -> [tyQ| a -> text |]
  Concat -> [tyQ| text -> text -> text |]
  Chars -> [tyQ| text -> int |]
  Split -> [tyQ| int -> text -> (text * text) |]
  AppF -> [tyQ| (a -> b) -> a -> b |]
  Swap -> [tyQ| text -> cmd text |]
  Atomic -> [tyQ| cmd a -> cmd a |]
  Teleport -> [tyQ| robot -> (int * int) -> cmd unit |]
  As -> [tyQ| robot -> {cmd a} -> cmd a |]
  RobotNamed -> [tyQ| text -> cmd robot |]
  RobotNumbered -> [tyQ| int -> cmd robot |]
  Knows -> [tyQ| text -> cmd bool |]
 where
  cmpBinT = [tyQ| a -> a -> bool |]
  arithBinT = [tyQ| int -> int -> int |]

-- | @check t ty@ checks that @t@ has type @ty@.
check :: Syntax -> UType -> Infer ()
check t ty = do
  ty' <- infer t
  _ <- ty =:= ty'
  return ()

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
validAtomic :: Syntax -> Infer ()
validAtomic s@(Syntax l t) = do
  n <- analyzeAtomic S.empty s
  when (n > 1) $ throwError (InvalidAtomic l (TooManyTicks n) t)

-- | Analyze an argument to @atomic@: ensure it contains no nested
--   atomic blocks and no references to external variables, and count
--   how many tangible commands it will execute.
analyzeAtomic :: Set Var -> Syntax -> Infer Int
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
  SBind mx s1 s2 -> (+) <$> analyzeAtomic locals s1 <*> analyzeAtomic (maybe id S.insert mx locals) s2
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
