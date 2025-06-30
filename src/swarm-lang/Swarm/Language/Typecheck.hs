{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Type inference for the Swarm language.  For the approach used here,
-- see
-- https://byorgey.wordpress.com/2021/09/08/implementing-hindley-milner-with-the-unification-fd-library/ .
module Swarm.Language.Typecheck (
  -- * Type errors
  ContextualTypeErr (..),
  TypeErr (..),
  InvalidAtomicReason (..),
  prettyTypeErrText,

  -- * Type provenance
  Source (..),
  withSource,
  Join,
  getJoin,

  -- * Typechecking stack
  TCFrame (..),
  LocatedTCFrame (..),
  TCStack,
  withFrame,

  -- * Typechecking monad
  fresh,

  -- * Unification
  instantiate,
  skolemize,
  generalize,

  -- * Type inference
  inferTop,
  checkTop,
  infer,
  inferConst,
  check,
  isSimpleUType,
) where

import Control.Arrow ((***))
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.State.Strict (StateC, evalState)
import Control.Carrier.Throw.Either (ThrowC, runThrow)
import Control.Category ((>>>))
import Control.Effect.Catch (Catch, catchError)
import Control.Effect.Error (Error)
import Control.Effect.Reader
import Control.Effect.State
import Control.Effect.Throw
import Control.Lens (view, (^.))
import Control.Lens.Indexed (itraverse)
import Control.Monad (forM, forM_, void, when, (<=<), (>=>))
import Control.Monad.Free qualified as Free
import Data.Bifunctor (first, second)
import Data.Data (gmapM)
import Data.Foldable (fold)
import Data.Generics (mkM)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set, (\\))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Swarm.Effect.Unify (Unification, UnificationError)
import Swarm.Effect.Unify qualified as U
import Swarm.Effect.Unify.Fast qualified as U
import Swarm.Language.Context hiding (lookup)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Kindcheck (KindError (..), processPolytype, processType)
import Swarm.Language.Load (Module (..), SourceMap)
import Swarm.Language.Parser.QQ (tyQ)
import Swarm.Language.Parser.Util (getLocRange)
import Swarm.Language.Requirements.Analysis (requirements)
import Swarm.Language.Requirements.Type (ReqCtx)
import Swarm.Language.Syntax
import Swarm.Language.TDVar (TDVar, tdVarName)
import Swarm.Language.Types
import Swarm.Pretty
import Prelude hiding (lookup)

------------------------------------------------------------
-- Typechecking stack

-- | A frame to keep track of something we were in the middle of doing
--   during typechecking.
data TCFrame where
  -- | Checking a definition.
  TCLet :: Var -> TCFrame
  -- | Inferring the LHS of an application.  Stored Syntax is the term
  --   on the RHS.
  TCAppL :: Syntax Raw -> TCFrame
  -- | Checking the RHS of an application.  Stored Syntax is the term
  -- on the LHS.
  TCAppR :: Syntax Raw -> TCFrame
  -- | Recursively checking an import.
  TCImport :: ImportLoc -> TCFrame
  deriving (Show)

instance PrettyPrec TCFrame where
  prettyPrec _ = \case
    TCLet x -> "While checking the definition of" <+> ppr x
    TCAppL s -> "While checking a function applied to an argument: _" <+> prettyPrec 11 s
    TCAppR s -> "While checking the argument to a function:" <+> prettyPrec 10 s <+> "_"
    TCImport loc -> "While checking an import:" <+> prettyPrec 0 loc

-- | A typechecking stack frame together with the relevant @SrcLoc@.
data LocatedTCFrame = LocatedTCFrame SrcLoc TCFrame
  deriving (Show)

instance PrettyPrec LocatedTCFrame where
  prettyPrec p (LocatedTCFrame _ f) = prettyPrec p f

-- | A typechecking stack keeps track of what we are currently in the
--   middle of doing during typechecking.
type TCStack = [LocatedTCFrame]

-- | Push a frame on the typechecking stack.
withFrame :: Has (Reader TCStack) sig m => SrcLoc -> TCFrame -> m a -> m a
withFrame l f = local (LocatedTCFrame l f :)

------------------------------------------------------------
-- Type source

-- | The source of a type during typechecking.
data Source
  = -- | An expected type that was "pushed down" from the context.
    Expected
  | -- | An actual/inferred type that was "pulled up" from a term.
    Actual
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Generic eliminator for 'Source'.  Choose the first argument if
--   the 'Source' is 'Expected', and the second argument if 'Actual'.
withSource :: Source -> a -> a -> a
withSource Expected e _ = e
withSource Actual _ a = a

-- | A value along with its source (expected vs actual).
type Sourced a = (Source, a)

-- | A "join" where an expected thing meets an actual thing.
newtype Join a = Join (Source -> a)
  deriving (Functor)

instance Foldable Join where
  foldMap :: Monoid m => (a -> m) -> Join a -> m
  foldMap f j = f a1 <> f a2
   where
    (a1, a2) = getJoin j

instance Traversable Join where
  traverse :: Applicative f => (a -> f b) -> Join a -> f (Join b)
  traverse f j = joined <$> f a1 <*> f a2
   where
    (a1, a2) = getJoin j

instance (Show a) => Show (Join a) where
  show (getJoin -> (e, a)) = "(expected: " <> show e <> ", actual: " <> show a <> ")"

type TypeJoin = Join UType

-- | Create a 'Join' from an expected thing and an actual thing (in that order).
joined :: a -> a -> Join a
joined expect actual = Join (\case Expected -> expect; Actual -> actual)

-- | Create a 'Join' from a 'Sourced' thing together with another
--   thing (which is assumed to have the opposite 'Source').
mkJoin :: Sourced a -> a -> Join a
mkJoin (src, a1) a2 = Join $ \s -> if s == src then a1 else a2

-- | Convert a 'Join' into a pair of (expected, actual).
getJoin :: Join a -> (a, a)
getJoin (Join j) = (j Expected, j Actual)

------------------------------------------------------------
-- Type checking

fromInferredSyntax ::
  ( Has Unification sig m
  , Has (Reader UCtx) sig m
  , Has (Throw ContextualTypeErr) sig m
  ) =>
  Syntax Inferred ->
  m (Syntax Typed)
fromInferredSyntax = traverseTypes (checkPredicative <=< (fmap fromU . generalize))

finalizeInferredSyntax ::
  ( Has Unification sig m
  , Has (Reader UCtx) sig m
  , Has (Throw ContextualTypeErr) sig m
  ) =>
  Syntax Inferred ->
  m (Syntax Typed)
finalizeInferredSyntax = applyBindings >=> fromInferredSyntax

-- | Run a top-level inference computation, either throwing a
--   'ContextualTypeErr' or returning a fully resolved 'Syntax Typed'.
runTC ::
  Has (Throw ContextualTypeErr) sig m =>
  TCtx ->
  ReqCtx ->
  TDCtx ->
  TVCtx ->
  SourceMap Raw ->
  ReaderC UCtx (ReaderC TCStack (U.UnificationC (ReaderC ReqCtx (ReaderC TDCtx (ReaderC TVCtx (ReaderC (SourceMap Raw) (StateC (SourceMap Inferred) m))))))) (Syntax Inferred) ->
  m (Syntax Typed)
runTC ctx reqCtx tdctx tvCtx srcMap =
  (>>= finalizeInferredSyntax)
    >>> runReader (toU ctx)
    >>> runReader []
    >>> U.runUnification
    >>> runReader reqCtx
    >>> runReader tdctx
    >>> runReader tvCtx
    >>> runReader srcMap
    >>> evalState M.empty
    >>> reportUnificationError

checkPredicative :: Has (Throw ContextualTypeErr) sig m => Maybe a -> m a
checkPredicative = maybe (throwError (mkRawTypeErr Impredicative)) pure

reportUnificationError ::
  Has (Throw ContextualTypeErr) sig m =>
  m (Either UnificationError a) -> m a
reportUnificationError = (>>= either (throwError . mkRawTypeErr . UnificationErr) pure)

-- | Look up a variable in the ambient type context, either throwing
--   an 'UnboundVar' error if it is not found, or opening its
--   associated 'UPolytype' with fresh unification variables via
--   'instantiate'.
lookup ::
  ( Has (Throw ContextualTypeErr) sig m
  , Has (Reader TCStack) sig m
  , Has (Reader UCtx) sig m
  , Has (Reader TVCtx) sig m
  , Has Unification sig m
  ) =>
  SrcLoc ->
  Var ->
  m UType
lookup loc x = do
  ctx <- ask @UCtx
  maybe (throwTypeErr loc $ UnboundVar x) instantiate (Ctx.lookup x ctx)

-- | Catch any thrown type errors and re-throw them with an added source
--   location.
addLocToTypeErr ::
  ( Has (Throw ContextualTypeErr) sig m
  , Has (Catch ContextualTypeErr) sig m
  ) =>
  SrcLoc ->
  m a ->
  m a
addLocToTypeErr l m =
  m `catchError` \case
    CTE NoLoc stk te -> throwError $ CTE l stk te
    cte -> throwError cte

------------------------------------------------------------
-- Dealing with variables: free variables, fresh variables,
-- substitution

-- | A class for getting the free unification variables of a thing.
class FreeUVars a where
  freeUVars :: Has Unification sig m => a -> m (Set IntVar)

-- | We can get the free unification variables of a 'UType'.
instance FreeUVars UType where
  freeUVars = U.freeUVars

-- | We can also get the free variables of a polytype.
instance (FreeUVars t) => FreeUVars (Poly q t) where
  freeUVars = freeUVars . ptBody

-- | We can get the free variables in any polytype in a context.
instance FreeUVars UCtx where
  freeUVars = fmap S.unions . mapM freeUVars . M.elems . unCtx

-- | Generate a fresh unification variable.
fresh :: Has Unification sig m => m UType
fresh = Free.Pure <$> U.freshIntVar

-- | Perform a substitution over a 'UType', substituting for both type
--   and unification variables.  Note that since 'UType's do not have
--   any binding constructs, we don't have to worry about ignoring
--   bound variables; all variables in a 'UType' are free.
substU :: Map (Either Var IntVar) UType -> UType -> UType
substU m =
  ucata
    (\v -> fromMaybe (Free.Pure v) (M.lookup (Right v) m))
    ( \case
        TyVarF o v -> fromMaybe (UTyVar' o v) (M.lookup (Left v) m)
        f -> Free.Free f
    )

-- | Make sure none of the given skolem variables have escaped.
noSkolems ::
  ( Has Unification sig m
  , Has (Reader TCStack) sig m
  , Has (Throw ContextualTypeErr) sig m
  ) =>
  SrcLoc ->
  [Var] ->
  UPolytype ->
  m ()
noSkolems l skolems (unPoly -> (xs, upty)) = do
  upty' <- applyBindings upty
  let tyvs =
        ucata
          (const S.empty)
          (\case TyVarF _ v -> S.singleton v; f -> fold f)
          upty'
      freeTyvs = tyvs `S.difference` S.fromList xs
      escapees = freeTyvs `S.intersection` S.fromList skolems

  -- In case of multiple escapees, just generate an error for one
  forM_ (S.lookupMin escapees) $ throwTypeErr l . EscapedSkolem

-- ~~~~ Note [lookupMin to get an arbitrary element]
--
-- `S.lookupMin :: Set a -> Maybe a` returns the smallest
-- element of a set, or Nothing if the set is empty. We don't
-- actually care about getting the *smallest* type variable, but
-- lookupMin is a convenient way to say "just get one element if
-- any exist". The forM_ is actually over the Maybe so it represents
-- doing the throwTypeErr either zero or one time, depending on
-- whether lookupMin returns Nothing or Just.

-- | @unify t expTy actTy@ ensures that the given two types are equal.
--   If we know the actual term @t@ which is supposed to have these
--   types, we can use it to generate better error messages.
unify ::
  ( Has Unification sig m
  , Has (Throw ContextualTypeErr) sig m
  , Has (Reader TCStack) sig m
  ) =>
  Maybe (Syntax Raw) ->
  TypeJoin ->
  m UType
unify ms j = do
  res <- expected U.=:= actual
  case res of
    Left _ -> do
      j' <- traverse U.applyBindings j
      throwTypeErr NoLoc $ Mismatch ms j'
    Right ty -> return ty
 where
  (expected, actual) = getJoin j

-- | The 'HasBindings' class is for anything which has
--   unification variables in it and to which we can usefully apply
--   'applyBindings'.
class HasBindings u where
  applyBindings :: Has Unification sig m => u -> m u

instance HasBindings UType where
  applyBindings = U.applyBindings

instance HasBindings UPolytype where
  applyBindings = traverse applyBindings

instance HasBindings UCtx where
  applyBindings = mapM applyBindings

instance HasBindings (Term Inferred) where
  applyBindings = gmapM (mkM (applyBindings @(Syntax Inferred)))

instance HasBindings (Syntax Inferred) where
  applyBindings (Syntax l t cs u) = Syntax l <$> applyBindings t <*> pure cs <*> applyBindings u

------------------------------------------------------------
-- Converting between mono- and polytypes

-- | To 'instantiate' a 'UPolytype', we generate a fresh unification
--   variable for each variable bound by the `Forall`, and then
--   substitute them throughout the type.
instantiate :: (Has Unification sig m, Has (Reader TVCtx) sig m) => UPolytype -> m UType
instantiate (unPoly -> (xs, uty)) = do
  xs' <- mapM (const fresh) xs
  boundSubst <- ask @TVCtx
  let s = M.mapKeys Left (M.fromList (zip xs xs') `M.union` unCtx boundSubst)
  return $ substU s uty

-- | 'skolemize' is like 'instantiate', except we substitute fresh
--   /type/ variables instead of unification variables.  Such
--   variables cannot unify with anything other than themselves.  This
--   is used when checking something with a polytype explicitly
--   specified by the user.
--
--   Returns a context mapping from instantiated type variables to generated
--   Skolem variables, along with the substituted type.
skolemize :: (Has Unification sig m, Has (Reader TVCtx) sig m) => UPolytype -> m (Ctx Var UType, UType)
skolemize (unPoly -> (xs, uty)) = do
  skolemNames <- forM xs $ \x -> do
    s <- mkVarName "s" <$> U.freshIntVar
    pure (x, s)
  boundSubst <- ask @TVCtx
  let xs' = map (uncurry UTyVar') skolemNames
      newSubst = M.fromList $ zip xs xs'
      s = M.mapKeys Left (newSubst `M.union` unCtx boundSubst)
  pure (Ctx.fromMap newSubst, substU s uty)

-- | 'generalize' is the opposite of 'instantiate': add a 'Forall'
--   which closes over all free type and unification variables.
--
--   Pick nice type variable names instead of reusing whatever fresh
--   names happened to be used for the free variables.
generalize :: (Has Unification sig m, Has (Reader UCtx) sig m) => UType -> m UPolytype
generalize uty = do
  uty' <- applyBindings uty
  ctx <- ask @UCtx
  tmfvs <- freeUVars uty'
  ctxfvs <- freeUVars ctx
  let fvs = S.toList $ tmfvs \\ ctxfvs
      alphabet = ['a' .. 'z']
      -- Infinite supply of pretty names a, b, ..., z, a0, ... z0, a1, ... z1, ...
      prettyNames = map T.pack (map (: []) alphabet ++ [x : show n | n <- [0 :: Int ..], x <- alphabet])
      -- Associate each free variable with a new pretty name
      renaming = zip fvs prettyNames
  return . absQuantify $
    mkPoly
      (map snd renaming)
      (substU (M.fromList . map (Right *** UTyVar) $ renaming) uty')

------------------------------------------------------------
-- Type errors
------------------------------------------------------------

--------------------------------------------------
-- Basic type errors

-- | Errors that can occur during type checking.  The idea is that
--   each error carries information that can be used to help explain
--   what went wrong (though the amount of information carried can and
--   should be very much improved in the future); errors can then
--   separately be pretty-printed to display them to the user.
data TypeErr
  = -- | An undefined variable was encountered.
    UnboundVar Var
  | -- | An undefined type was encountered.
    UnboundType TDVar
  | -- | A kind error was encountered.
    KindErr KindError
  | -- | A Skolem variable escaped its local context.
    EscapedSkolem Var
  | -- | Failure during unification.
    UnificationErr UnificationError
  | -- | Type mismatch caught by 'unify'.  The given term was
    --   expected to have a certain type, but has a different type
    --   instead.
    Mismatch (Maybe (Syntax Raw)) TypeJoin
  | -- | Record type mismatch.  The given term was expected to have a
    --   record type, but has a different type instead.
    MismatchRcd (Maybe (Syntax Raw)) UType
  | -- | Lambda argument type mismatch.
    LambdaArgMismatch TypeJoin
  | -- | Record field mismatch, i.e. based on the expected type we
    --   were expecting a record with certain fields, but found one with
    --   a different field set.
    FieldsMismatch (Join (Set Var))
  | -- | A definition was encountered not at the top level.
    DefNotTopLevel (Term Raw)
  | -- | A term was encountered which we cannot infer the type of.
    --   This should never happen.
    CantInfer (Term Raw)
  | -- | We can't infer the type of a record projection @r.x@ if we
    --   don't concretely know the type of the record @r@.
    CantInferProj (Term Raw)
  | -- | An attempt to project out a nonexistent field
    UnknownProj Var (Term Raw)
  | -- | An invalid argument was provided to @atomic@.
    InvalidAtomic InvalidAtomicReason (Term Raw)
  | -- | Some unification variables ended up in a type, probably due to
    --   impredicativity.  See https://github.com/swarm-game/swarm/issues/351 .
    Impredicative
  | -- | Read must be given a literal type as an argument.  See
    --   https://github.com/swarm-game/swarm/pull/2461#discussion_r2124125021
    ReadNonLiteralTypeArg (Term Raw)
  | -- | An import encountered during typechecking was not found in
    --   the import source map.  This should never happen and indicates
    --   a bug.
    UnknownImport ImportLoc
  deriving (Show)

instance PrettyPrec TypeErr where
  prettyPrec _ = \case
    UnificationErr ue -> ppr ue
    KindErr ke -> ppr ke
    Mismatch Nothing (getJoin -> (ty1, ty2)) ->
      "Type mismatch: expected" <+> ppr ty1 <> ", but got" <+> ppr ty2
    Mismatch (Just t) (getJoin -> (ty1, ty2)) ->
      nest 2 . vcat $
        [ "Type mismatch:"
        , "From context, expected" <+> pprCode t <+> "to" <+> typeDescription Expected ty1 <> ","
        , "but it" <+> typeDescription Actual ty2
        ]
    MismatchRcd Nothing ty ->
      "Type mismatch: expected a record type, but got" <+> ppr ty
    MismatchRcd (Just t) ty ->
      nest 2 . vcat $
        [ "Type mismatch:"
        , "From context, expected" <+> pprCode t <+> "to have a record type,"
        , "but it" <+> typeDescription Actual ty
        ]
    LambdaArgMismatch (getJoin -> (ty1, ty2)) ->
      "Lambda argument has type annotation" <+> pprCode ty2 <> ", but expected argument type" <+> pprCode ty1
    FieldsMismatch (getJoin -> (expFs, actFs)) ->
      fieldMismatchMsg expFs actFs
    EscapedSkolem x ->
      "Skolem variable" <+> ppr x <+> "would escape its scope"
    UnboundVar x ->
      "Undefined variable" <+> ppr x
    UnboundType x ->
      "Undefined type" <+> ppr x
    DefNotTopLevel t ->
      "Definitions may only be at the top level:" <+> pprCode t
    CantInfer t ->
      vsep
        [ "Couldn't infer the type of term:" <+> pprCode t
        , reportBug
        ]
    CantInferProj t ->
      "In the record projection" <+> pprCode t <> ", can't infer whether the LHS has a record type.  Try adding a type annotation."
    UnknownProj x t ->
      "Record does not have a field with name" <+> ppr x <> ":" <+> pprCode t
    InvalidAtomic reason t ->
      "Invalid atomic block:" <+> ppr reason <> ":" <+> pprCode t
    Impredicative ->
      "Unconstrained unification type variables encountered, likely due to an impredicative type. This is a known bug; for more information see https://github.com/swarm-game/swarm/issues/351 ."
    ReadNonLiteralTypeArg t ->
      "The `read` command must be given a literal type as its first argument (Swarm does not have dependent types); found" <+> pprCode t <+> "instead."
    UnknownImport loc ->
      vsep
        [ "Unknown import encountered:" <+> ppr loc <> "."
        , "This should never happen; please report this as a bug at https://github.com/swarm-game/swarm/issues/new?template=bug_report.md"
        ]
   where
    pprCode :: PrettyPrec a => a -> Doc ann
    pprCode = bquote . ppr

-- | Given a type and its source, construct an appropriate description
--   of it to go in a type mismatch error message.
typeDescription :: Source -> UType -> Doc a
typeDescription src ty
  | not (hasAnyUVars ty) =
      withSource src "have" "actually has" <+> "type" <+> bquote (ppr ty)
  | Just f <- isTopLevelConstructor ty =
      withSource src "be" "is actually" <+> tyNounPhrase f
  | otherwise =
      withSource src "have" "actually has" <+> "a type like" <+> bquote (ppr (fmap (const Wildcard) ty))

-- | Return an English noun phrase describing things with the given
--   top-level type constructor.
tyNounPhrase :: TypeF () -> Doc a
tyNounPhrase = \case
  TyConF c _ -> tyConNounPhrase c
  TyVarF {} -> "a type variable"
  TyRcdF {} -> "a record"
  TyRecF {} -> "a recursive type"
  TyRecVarF {} -> "a recursive type variable"

tyConNounPhrase :: TyCon -> Doc a
tyConNounPhrase = \case
  TCBase b -> baseTyNounPhrase b
  TCCmd -> "a command"
  TCDelay -> "a delayed expression"
  TCSum -> "a sum"
  TCProd -> "a pair"
  TCFun -> "a function"
  TCUser t -> ppr t

-- | Return an English noun phrase describing things with the given
--   base type.
baseTyNounPhrase :: BaseTy -> Doc a
baseTyNounPhrase = \case
  BVoid -> "void"
  BUnit -> "the unit value"
  BInt -> "an integer"
  BText -> "text"
  BDir -> "a direction"
  BBool -> "a boolean"
  BActor -> "an actor"
  BKey -> "a key"
  BType -> "a type"

-- | Generate an appropriate message when the sets of fields in two
--   record types do not match, explaining which fields are extra and
--   which are missing.
fieldMismatchMsg :: Set Var -> Set Var -> Doc a
fieldMismatchMsg expFs actFs =
  nest 2 . vcat $
    ["Field mismatch; record literal has:"]
      ++ ["- Extra field(s)" <+> prettyFieldSet extraFs | not (S.null extraFs)]
      ++ ["- Missing field(s)" <+> prettyFieldSet missingFs | not (S.null missingFs)]
 where
  extraFs = actFs `S.difference` expFs
  missingFs = expFs `S.difference` actFs
  prettyFieldSet = hsep . punctuate "," . map (bquote . ppr) . S.toList

--------------------------------------------------
-- Errors for 'atomic'

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
  | -- | The argument contained a suspend
    AtomicSuspend
  | -- | The argument contained an import
    AtomicImport
  deriving (Show)

instance PrettyPrec InvalidAtomicReason where
  prettyPrec _ = \case
    TooManyTicks n -> "block could take too many ticks (" <> pretty n <> ")"
    AtomicDupingThing -> "def, let, and lambda are not allowed"
    NonSimpleVarType _ ty ->
      "reference to variable with non-simple type" <+> ppr (prettyTextLine ty)
    NestedAtomic -> "nested atomic block"
    LongConst -> "commands that can take multiple ticks to execute are not allowed"
    AtomicSuspend ->
      "encountered a suspend command inside an atomic block" <> hardline <> reportBug
    AtomicImport -> "import is not allowed"

--------------------------------------------------
-- Type errors with context

-- | A type error along with various contextual information to help us
--   generate better error messages.
data ContextualTypeErr = CTE {cteSrcLoc :: SrcLoc, cteStack :: TCStack, cteTypeErr :: TypeErr}
  deriving (Show)

-- | Create a raw 'ContextualTypeErr' with no context information.
mkRawTypeErr :: TypeErr -> ContextualTypeErr
mkRawTypeErr = CTE NoLoc []

-- | Create a 'ContextualTypeErr' value from a 'TypeErr' and context.
mkTypeErr :: SrcLoc -> TCStack -> TypeErr -> ContextualTypeErr
mkTypeErr = CTE

-- | Throw a 'ContextualTypeErr'.
throwTypeErr ::
  ( Has (Throw ContextualTypeErr) sig m
  , Has (Reader TCStack) sig m
  ) =>
  SrcLoc ->
  TypeErr ->
  m a
throwTypeErr l te = do
  stk <- ask @TCStack
  throwError $ mkTypeErr l stk te

-- | Adapt some other error type to a 'ContextualTypeErr'.
adaptToTypeErr ::
  ( Has (Throw ContextualTypeErr) sig m
  , Has (Reader TCStack) sig m
  ) =>
  SrcLoc ->
  (e -> TypeErr) ->
  ThrowC e m a ->
  m a
adaptToTypeErr l adapt m = do
  res <- runThrow m
  case res of
    Left e -> throwTypeErr l (adapt e)
    Right a -> return a

--------------------------------------------------
-- Pretty-printing for contextual type errors

-- | Format a 'ContextualTypeError' for the user and render it as
--   @Text@.
prettyTypeErrText :: Text -> ContextualTypeErr -> Text
prettyTypeErrText code = docToText . prettyTypeErr code

-- | Format a 'ContextualTypeError' for the user.
prettyTypeErr :: Text -> ContextualTypeErr -> Doc ann
prettyTypeErr code (CTE l tcStack te) =
  vcat
    [ teLoc <> ppr te
    , ppr (BulletList "" (filterTCStack tcStack))
    ]
 where
  teLoc = case l of
    SrcLoc s e -> (showLoc . fst $ getLocRange code (s, e)) <> ": "
    NoLoc -> emptyDoc
  showLoc (r, c) = pretty r <> ":" <> pretty c

-- | Filter the TCStack so we stop printing context outside of a def/let
filterTCStack :: TCStack -> TCStack
filterTCStack tcStack = case tcStack of
  [] -> []
  -- A def/let is enough context to locate something; don't keep
  -- printing wider context after that
  t@(LocatedTCFrame _ (TCLet _)) : _ -> [t]
  t : xs -> t : filterTCStack xs

------------------------------------------------------------
-- Type decomposition

-- | Decompose a type that is supposed to be the application of a
--   given type constructor to a single type argument. Also take the
--   term which is supposed to have that type, for use in error
--   messages.
decomposeTyConApp1 ::
  ( Has Unification sig m
  , Has (Throw ContextualTypeErr) sig m
  , Has (Reader TDCtx) sig m
  , Has (Reader TCStack) sig m
  ) =>
  TyCon ->
  Syntax Raw ->
  Sourced UType ->
  m UType
decomposeTyConApp1 c t (src, UTyConApp (TCUser u) as) = do
  ty2 <- adaptToTypeErr NoLoc (UnboundType . getUnexpanded) $ expandTydef u as
  decomposeTyConApp1 c t (src, ty2)
decomposeTyConApp1 c _ (_, UTyConApp c' [a])
  | c == c' = return a
decomposeTyConApp1 c t ty = do
  a <- fresh
  _ <- unify (Just t) (mkJoin ty (UTyConApp c [a]))
  return a

decomposeCmdTy
  , decomposeDelayTy ::
    ( Has Unification sig m
    , Has (Throw ContextualTypeErr) sig m
    , Has (Reader TDCtx) sig m
    , Has (Reader TCStack) sig m
    ) =>
    Syntax Raw ->
    Sourced UType ->
    m UType
decomposeCmdTy = decomposeTyConApp1 TCCmd
decomposeDelayTy = decomposeTyConApp1 TCDelay

-- | Decompose a type which is expected to be a record type.  There
--   are three possible outcomes:
--
--     * If the type is definitely a record type, return its mapping
--       from field names to types.
--
--     * If the type is definitely not a record type, throw a type error.
--
--     * Otherwise, return @Nothing@.
--
--   This is the best we can do, and different than the way the other
--   @decompose...Ty@ functions work, because we can't solve for record
--   types via unification.
decomposeRcdTy ::
  ( Has (Reader TDCtx) sig m
  , Has (Reader TCStack) sig m
  , Has (Throw ContextualTypeErr) sig m
  ) =>
  Maybe (Syntax Raw) ->
  UType ->
  m (Maybe (Map Var UType))
decomposeRcdTy ms = \case
  ty@(UTyConApp tc as) -> case tc of
    -- User-defined type: expand it
    TCUser u -> do
      ty2 <- adaptToTypeErr NoLoc (UnboundType . getUnexpanded) $ expandTydef u as
      decomposeRcdTy ms ty2
    -- Any other type constructor application is definitely not a record type
    _ -> throwTypeErr (maybe NoLoc (view sLoc) ms) $ MismatchRcd ms ty
  -- Recursive type: expand it
  UTyRec x t -> decomposeRcdTy ms (unfoldRec x t)
  -- Record type
  UTyRcd m -> pure (Just m)
  -- With anything else (type variables, etc.) we're not sure
  _ -> pure Nothing

-- | Decompose a type that is supposed to be the application of a
--   given type constructor to two type arguments.  Also take the term
--   which is supposed to have that type, for use in error messages.
decomposeTyConApp2 ::
  ( Has Unification sig m
  , Has (Throw ContextualTypeErr) sig m
  , Has (Reader TDCtx) sig m
  , Has (Reader TCStack) sig m
  ) =>
  TyCon ->
  Syntax Raw ->
  Sourced UType ->
  m (UType, UType)
decomposeTyConApp2 c t (src, UTyConApp (TCUser u) as) = do
  ty2 <- adaptToTypeErr NoLoc (UnboundType . getUnexpanded) $ expandTydef u as
  decomposeTyConApp2 c t (src, ty2)
decomposeTyConApp2 c _ (_, UTyConApp c' [ty1, ty2])
  | c == c' = return (ty1, ty2)
decomposeTyConApp2 c t ty = do
  a1 <- fresh
  a2 <- fresh
  _ <- unify (Just t) (mkJoin ty (UTyConApp c [a1, a2]))
  return (a1, a2)

decomposeFunTy
  , decomposeProdTy ::
    ( Has Unification sig m
    , Has (Throw ContextualTypeErr) sig m
    , Has (Reader TDCtx) sig m
    , Has (Reader TCStack) sig m
    ) =>
    Syntax Raw ->
    Sourced UType ->
    m (UType, UType)
decomposeFunTy = decomposeTyConApp2 TCFun
decomposeProdTy = decomposeTyConApp2 TCProd

------------------------------------------------------------
-- Type inference / checking

-- | Top-level type inference function: given a context of definition
--   types, type synonyms, and a term, either return a type error or a
--   fully type-annotated version of the term.
inferTop ::
  Has (Error ContextualTypeErr) sig m =>
  TCtx -> ReqCtx -> TDCtx -> SourceMap Raw -> Syntax Raw -> m (Syntax Typed)
inferTop ctx reqCtx tdCtx srcMap = runTC ctx reqCtx tdCtx Ctx.empty srcMap . infer

-- | Top level type checking function.
checkTop ::
  Has (Error ContextualTypeErr) sig m =>
  TCtx -> ReqCtx -> TDCtx -> SourceMap Raw -> Syntax Raw -> Type -> m (Syntax Typed)
checkTop ctx reqCtx tdCtx srcMap t ty = runTC ctx reqCtx tdCtx Ctx.empty srcMap $ check t (toU ty)

-- | Infer the type of a term, returning a type-annotated term.
--
--   The only cases explicitly handled in 'infer' are those where
--   pushing an expected type down into the term can't possibly help,
--   e.g. most primitives, function application, and binds.
--
--   For most everything else we prefer 'check' because it can often
--   result in better and more localized type error messages.
--
--   Note that we choose to do kind checking inline as we go during
--   typechecking.  This has pros and cons.  The benefit is that we get
--   to piggyback on the existing source location tracking and
--   typechecking stack, so we can generate better error messages.  The
--   downside is that we have to be really careful not to miss any types
--   along the way; there is no difference, at the Haskell type level,
--   between ill- and well-kinded Swarm types, so we just have to make
--   sure that we call processType on every type embedded in the term
--   being checked.
infer ::
  ( Has (Reader UCtx) sig m
  , Has (Reader ReqCtx) sig m
  , Has (Reader TDCtx) sig m
  , Has (Reader TVCtx) sig m
  , Has (Reader (SourceMap Raw)) sig m
  , Has (State (SourceMap Inferred)) sig m
  , Has (Reader TCStack) sig m
  , Has Unification sig m
  , Has (Error ContextualTypeErr) sig m
  ) =>
  Syntax Raw ->
  m (Syntax Inferred)
infer s@(CSyntax l t cs) = addLocToTypeErr l $ case t of
  -- Primitives, i.e. things for which we immediately know the only
  -- possible correct type, and knowing an expected type would provide
  -- no extra information.
  TUnit -> return $ Syntax l TUnit cs UTyUnit
  TConst c -> Syntax l (TConst c) cs <$> (instantiate . toU $ inferConst c)
  TDir d -> return $ Syntax l (TDir d) cs UTyDir
  TInt n -> return $ Syntax l (TInt n) cs UTyInt
  TAntiInt x -> return $ Syntax l (TAntiInt x) cs UTyInt
  TText x -> return $ Syntax l (TText x) cs UTyText
  TAntiText x -> return $ Syntax l (TAntiText x) cs UTyText
  TBool b -> return $ Syntax l (TBool b) cs UTyBool
  TRobot r -> return $ Syntax l (TRobot r) cs UTyActor
  TRequire d -> return $ Syntax l (TRequire d) cs (UTyCmd UTyUnit)
  TStock n d -> return $ Syntax l (TStock n d) cs (UTyCmd UTyUnit)
  SRequirements x t1 -> do
    t1' <- infer t1
    return $ Syntax l (SRequirements x t1') cs (UTyCmd UTyUnit)

  -- We should never encounter a TRef since they do not show up in
  -- surface syntax, only as values while evaluating (*after*
  -- typechecking).
  TRef _ -> throwTypeErr l $ CantInfer t
  -- Just look up variables in the context.
  TVar x -> Syntax l (TVar x) cs <$> lookup l x
  -- It is helpful to handle lambdas in inference mode as well as
  -- checking mode; in particular, we can handle lambdas with an
  -- explicit type annotation on the argument.  Just infer the body
  -- under an extended context and return the appropriate function
  -- type.
  SLam x (Just argTy) body -> do
    argTy' <- adaptToTypeErr l KindErr $ processType argTy
    let uargTy = toU argTy'
    body' <- withBinding @Var @UPolytype (locVal x) (mkTrivPoly uargTy) $ infer body
    return $ Syntax l (SLam x (Just argTy') body') cs (UTyFun uargTy (body' ^. sType))

  -- Need special case here for applying 'atomic' or 'instant' so we
  -- don't handle it with the case for generic type application.
  -- This must come BEFORE the SApp case.
  TConst c :$: _
    | c `elem` [Atomic, Instant] -> fresh >>= check s
  -- Special case for applying 'read' to a type argument, since we need to make
  -- sure the type propagates to the inferred output type of 'read'.
  TConst Read :$: RTerm arg -> do
    argTy <- case arg of
      TType ty -> pure ty
      _ -> throwTypeErr l $ ReadNonLiteralTypeArg arg
    r' <- infer $ RSyntax l (TConst Read)
    argTy' <- adaptToTypeErr l (UnboundType . getUnexpanded) $ expandTydefs argTy
    arg' <- check (RTerm (TType argTy')) UTyType
    pure $ Syntax l (SApp r' arg') cs (UTyFun UTyText (toU argTy'))

  -- It works better to handle applications in *inference* mode.
  -- Knowing the expected result type of an application does not
  -- really help much.  In the typical case, the function being
  -- applied is either (1) a primitive or variable whose type we can
  -- easily infer, or (2) a nested application; in the second case in
  -- particular, handling applications in inference mode means we can
  -- stay in inference mode the whole way down the left-hand side of
  -- the chain of applications.  If we handled applications in
  -- checking mode, we would constantly flip back and forth between
  -- inference & checking and generate a fresh unification variable
  -- each time.
  SApp f x -> do
    -- Infer the type of the left-hand side and make sure it has a function type.
    (f', argTy, resTy) <- withFrame l (TCAppL x) $ do
      f' <- infer f
      (argTy, resTy) <- decomposeFunTy f (Actual, f' ^. sType)
      pure (f', argTy, resTy)

    -- Then check that the argument has the right type.
    x' <- withFrame l (TCAppR f) $ check x argTy

    -- Call applyBindings explicitly, so that anything we learned
    -- about unification variables while checking the type of the
    -- argument can flow to later steps.  This is especially helpful
    -- while checking applications of polymorphic multi-argument
    -- functions such as 'if'.  Without this call to 'applyBindings',
    -- type mismatches between the branches of an 'if' tend to get
    -- caught in the unifier, resulting in vague "can't unify"
    -- messages (for example, "if true {3} {move}" yields "can't
    -- unify Int and Cmd Unit").  With this 'applyBindings' call, we
    -- get more specific errors about how the second branch was
    -- expected to have the same type as the first (e.g. "expected
    -- `move` to have type `Int`, but it actually has type `Cmd
    -- Unit`).
    resTy' <- applyBindings resTy

    return $ Syntax l (SApp f' x') cs resTy'

  -- We handle binds in inference mode for a similar reason to
  -- application.
  --
  -- There is no way to annotate a bind with types or requirements in
  -- the surface syntax, so the second through fourth fields are
  -- necessarily Nothing.
  SBind mx _ _ _ c1 c2 -> do
    c1' <- infer c1
    a <- decomposeCmdTy c1 (Actual, c1' ^. sType)
    genA <- generalize a
    c2' <-
      maybe id ((`withBinding` genA) . locVal) mx $
        infer c2

    -- We don't actually need the result type since we're just
    -- going to return the entire type, but it's important to
    -- ensure it's a command type anyway.  Otherwise something
    -- like 'move; 3' would be accepted with type int.
    _ <- decomposeCmdTy c2 (Actual, c2' ^. sType)

    -- NOTE: it is probably not correct to say that the variable bound
    -- in a monadic bind has no requirements.  But as long as it is
    -- some kind of primitive value type (Int, Bool, etc.) it really
    -- doesn't matter, since it will already be evaluated, so using it
    -- in the future really does incur no requirements.  It would only
    -- matter if someone used something like
    --
    --   f <- (c : Cmd (Cmd Int)); ... f ...
    --
    -- so that f ends up with a type like Cmd Int.  But we already
    -- don't handle that kind of thing correctly anyway.  The real fix
    -- will have to wait for #231.
    let binderReqs = mempty

    return $ Syntax l (SBind mx (Just a) Nothing (Just binderReqs) c1' c2') cs (c2' ^. sType)

  -- Handle record projection in inference mode.  Knowing the expected
  -- type of r.x doesn't really help since we must infer the type of r
  -- first anyway.
  SProj t1 x -> do
    t1' <- infer t1
    mm <- decomposeRcdTy (Just t1) (t1' ^. sType)
    case mm of
      Just m -> case M.lookup x m of
        Just xTy -> return $ Syntax l (SProj t1' x) cs xTy
        Nothing -> throwTypeErr l $ UnknownProj x (SProj t1 x)
      Nothing -> throwTypeErr l $ CantInferProj (SProj t1 x)

  -- See Note [Checking and inference for record literals]
  SRcd m -> do
    m' <- traverse (itraverse $ \x -> infer . fromMaybe (RTerm (TVar (locVal x)))) m
    let rcdTy = M.fromList $ map (locVal *** (^. sType)) m'
    return $ Syntax' l (SRcd ((map . second) Just m')) cs (UTyRcd rcdTy)

  -- Once we're typechecking, we don't need to keep around explicit
  -- parens any more
  SParens t1 -> infer t1
  -- To infer a type-annotated term, switch into checking mode.
  -- However, we must be careful to deal properly with polymorphic
  -- type annotations.
  SAnnotate c pty -> do
    qpty <- quantify pty
    TydefInfo qpty' _ <- adaptToTypeErr l KindErr $ processPolytype qpty
    let upty = toU qpty'
    -- Typecheck against skolemized polytype.
    (skolemSubst, uty) <- skolemize upty
    _ <- check c uty
    -- Make sure no skolem variables have escaped.
    ask @UCtx >>= mapM_ (noSkolems l (Ctx.vars skolemSubst))
    -- If check against skolemized polytype is successful,
    -- instantiate polytype with unification variables.
    -- Free variables should be able to unify with anything in
    -- following typechecking steps.
    iuty <- instantiate upty
    c' <- check c iuty
    return $ Syntax l (SAnnotate c' (forgetQ qpty)) cs iuty

  -- To infer @import m in e@, first make sure we have loaded and
  -- typechecked the import, then infer @e@ in an extended context.
  SImportIn loc t1 -> do
    -- See whether we have already processed this import before
    usrcMap <- get @(SourceMap Inferred)
    umod <- case M.lookup loc usrcMap of
      -- We have: just use its already-typechecked version
      Just umod -> pure umod
      -- We haven't: go typecheck it and add it to the USourceMap before proceeding.
      Nothing -> do
        srcMap <- ask @(SourceMap Raw)
        case M.lookup loc srcMap of
          -- The lookup should always succeed, since the SourceMap was
          -- computed by transitively following all imports.
          Nothing -> throwTypeErr l $ UnknownImport loc
          Just smod -> do
            umod <- withFrame l (TCImport loc) $ inferModule smod
            modify @(SourceMap Inferred) $ M.insert loc umod
            pure umod

    -- Now infer t1 with the import's exports added to the context.
    t1' <- withBindings (moduleCtx umod) $ infer t1
    return $ Syntax l (SImportIn loc t1') cs (t1' ^. sType)
  TType ty -> pure $ Syntax l (TType ty) cs UTyType
  -- Fallback: to infer the type of anything else, make up a fresh unification
  -- variable for its type and check against it.
  _ -> do
    sTy <- fresh
    check s sTy

-- | Collect up the names and types of any top-level definitions into
--   a context.
collectDefs ::
  (Has Unification sig m, Has (Reader UCtx) sig m) =>
  Syntax Inferred ->
  m UCtx
collectDefs (Syntax _ (SLet LSDef _ x _ _ _ _ t) _ ty) = do
  ty' <- generalize ty
  (Ctx.singleton (lvVar x) ty' <>) <$> collectDefs t
collectDefs _ = pure Ctx.empty

-- | Infer the type of a module, i.e. import, by (1) typechecking and
--   annotating the term itself, and (2) collecting up the types of
--   all exported top-level definitions into a context.
inferModule ::
  ( Has (Reader UCtx) sig m
  , Has (Reader ReqCtx) sig m
  , Has (Reader TDCtx) sig m
  , Has (Reader TVCtx) sig m
  , Has (Reader (SourceMap Raw)) sig m
  , Has (State (SourceMap Inferred)) sig m
  , Has (Reader TCStack) sig m
  , Has Unification sig m
  , Has (Error ContextualTypeErr) sig m
  ) =>
  Module Raw -> m (Module Inferred)
inferModule (Module ms _ imps) = do
  -- Infer the type of the term
  mt <- mapM infer ms

  -- Now, if the term has top-level definitions, collect up their
  -- types and put them in the context.
  ctx <- maybe (pure Ctx.empty) collectDefs mt
  pure $ Module mt ctx imps

-- | Infer the type of a constant.
inferConst :: Const -> Polytype
inferConst c = run . runReader @TVCtx Ctx.empty . quantify $ case c of
  Wait -> [tyQ| Int -> Cmd Unit |]
  Noop -> [tyQ| Cmd Unit |]
  Selfdestruct -> [tyQ| Cmd Unit |]
  Move -> [tyQ| Cmd Unit |]
  Backup -> [tyQ| Cmd Unit |]
  Volume -> [tyQ| Int -> Cmd (Unit + Int) |]
  Path -> [tyQ| (Unit + Int) -> ((Int * Int) + Text) -> Cmd (Unit + (Dir * Int)) |]
  Push -> [tyQ| Cmd Unit |]
  Stride -> [tyQ| Int -> Cmd Unit |]
  Turn -> [tyQ| Dir -> Cmd Unit |]
  Grab -> [tyQ| Cmd Text |]
  Harvest -> [tyQ| Cmd Text |]
  Sow -> [tyQ| Text -> Cmd Unit |]
  Ignite -> [tyQ| Dir -> Cmd Unit |]
  Place -> [tyQ| Text -> Cmd Unit |]
  Ping -> [tyQ| Actor -> Cmd (Unit + (Int * Int)) |]
  Give -> [tyQ| Actor -> Text -> Cmd Unit |]
  Equip -> [tyQ| Text -> Cmd Unit |]
  Unequip -> [tyQ| Text -> Cmd Unit |]
  Make -> [tyQ| Text -> Cmd Unit |]
  Has -> [tyQ| Text -> Cmd Bool |]
  Equipped -> [tyQ| Text -> Cmd Bool |]
  Count -> [tyQ| Text -> Cmd Int |]
  Reprogram -> [tyQ| Actor -> {Cmd a} -> Cmd Unit |]
  Build -> [tyQ| {Cmd a} -> Cmd Actor |]
  Drill -> [tyQ| Dir -> Cmd (Unit + Text) |]
  Use -> [tyQ| Text -> Dir -> Cmd (Unit + Text) |]
  Salvage -> [tyQ| Cmd Unit |]
  Say -> [tyQ| Text -> Cmd Unit |]
  Listen -> [tyQ| Cmd Text |]
  Log -> [tyQ| Text -> Cmd Unit |]
  View -> [tyQ| Actor -> Cmd Unit |]
  Appear -> [tyQ| Text -> (Unit + Text) -> Cmd Unit |]
  Create -> [tyQ| Text -> Cmd Unit |]
  Halt -> [tyQ| Actor -> Cmd Unit |]
  Time -> [tyQ| Cmd Int |]
  Scout -> [tyQ| Dir -> Cmd Bool |]
  Whereami -> [tyQ| Cmd (Int * Int) |]
  LocateMe -> [tyQ| Cmd (Text * (Int * Int)) |]
  Waypoints -> [tyQ| Text -> (rec l. Unit + (Int * Int) * l) |]
  Structures -> [tyQ| Text -> Cmd (rec l. Unit + (Int * Int) * l) |]
  Floorplan -> [tyQ| Text -> Cmd (Int * Int) |]
  HasTag -> [tyQ| Text -> Text -> Bool |]
  TagMembers -> [tyQ| Text -> (rec l. Unit + Text * l) |]
  Detect -> [tyQ| Text -> ((Int * Int) * (Int * Int)) -> Cmd (Unit + (Int * Int)) |]
  Resonate -> [tyQ| Text -> ((Int * Int) * (Int * Int)) -> Cmd Int |]
  Density -> [tyQ| ((Int * Int) * (Int * Int)) -> Cmd Int |]
  Sniff -> [tyQ| Text -> Cmd Int |]
  Chirp -> [tyQ| Text -> Cmd Dir |]
  Watch -> [tyQ| Dir -> Cmd Unit |]
  Surveil -> [tyQ| (Int * Int) -> Cmd Unit |]
  Heading -> [tyQ| Cmd Dir |]
  Blocked -> [tyQ| Cmd Bool |]
  Scan -> [tyQ| Dir -> Cmd (Unit + Text) |]
  Upload -> [tyQ| Actor -> Cmd Unit |]
  Ishere -> [tyQ| Text -> Cmd Bool |]
  Isempty -> [tyQ| Cmd Bool |]
  Self -> [tyQ| Actor |]
  Parent -> [tyQ| Actor |]
  Base -> [tyQ| Actor |]
  Meet -> [tyQ| Cmd (Unit + Actor) |]
  MeetAll -> [tyQ| Cmd (rec l. Unit + Actor * l) |]
  Whoami -> [tyQ| Cmd Text |]
  Setname -> [tyQ| Text -> Cmd Unit |]
  Random -> [tyQ| Int -> Cmd Int |]
  If -> [tyQ| Bool -> {a} -> {a} -> a |]
  Inl -> [tyQ| a -> a + b |]
  Inr -> [tyQ| b -> a + b |]
  Case -> [tyQ| a + b -> (a -> c) -> (b -> c) -> c |]
  Match -> [tyQ| a * b -> (a -> b -> c) -> c |]
  Force -> [tyQ| {a} -> a |]
  Pure -> [tyQ| a -> Cmd a |]
  Try -> [tyQ| {Cmd a} -> {Cmd a} -> Cmd a |]
  Undefined -> [tyQ| a |]
  Fail -> [tyQ| Text -> a |]
  Not -> [tyQ| Bool -> Bool |]
  Neg -> [tyQ| Int -> Int |]
  Eq -> cmpBinT
  Neq -> cmpBinT
  Lt -> cmpBinT
  Gt -> cmpBinT
  Leq -> cmpBinT
  Geq -> cmpBinT
  And -> [tyQ| Bool -> Bool -> Bool|]
  Or -> [tyQ| Bool -> Bool -> Bool|]
  Add -> arithBinT
  Sub -> arithBinT
  Mul -> arithBinT
  Div -> arithBinT
  Exp -> arithBinT
  Format -> [tyQ| a -> Text |]
  Read -> [tyQ| Type -> Text -> a |]
  Print -> [tyQ| Text -> Text -> Cmd Text |]
  Erase -> [tyQ| Text -> Cmd Text |]
  Concat -> [tyQ| Text -> Text -> Text |]
  Chars -> [tyQ| Text -> Int |]
  Split -> [tyQ| Int -> Text -> (Text * Text) |]
  CharAt -> [tyQ| Int -> Text -> Int |]
  ToChar -> [tyQ| Int -> Text |]
  AppF -> [tyQ| (a -> b) -> a -> b |]
  Swap -> [tyQ| Text -> Cmd Text |]
  Atomic -> [tyQ| {Cmd a} -> Cmd a |]
  Instant -> [tyQ| {Cmd a} -> Cmd a |]
  Key -> [tyQ| Text -> Key |]
  InstallKeyHandler -> [tyQ| Text -> (Key -> Cmd Unit) -> Cmd Unit |]
  Teleport -> [tyQ| Actor -> (Int * Int) -> Cmd Unit |]
  Warp -> [tyQ| Actor -> (Text * (Int * Int)) -> Cmd Unit |]
  As -> [tyQ| Actor -> {Cmd a} -> Cmd a |]
  RobotNamed -> [tyQ| Text -> Cmd Actor |]
  RobotNumbered -> [tyQ| Int -> Cmd Actor |]
  Knows -> [tyQ| Text -> Cmd Bool |]
  Destroy -> [tyQ| Actor -> Cmd Unit |]
 where
  cmpBinT = [tyQ| a -> a -> Bool |]
  arithBinT = [tyQ| Int -> Int -> Int |]

-- | @check t ty@ checks that @t@ has type @ty@, returning a
--   type-annotated AST if so.
--
--   We try to stay in checking mode as far as possible, decomposing
--   the expected type as we go and pushing it through the recursion.
check ::
  ( Has (Reader UCtx) sig m
  , Has (Reader ReqCtx) sig m
  , Has (Reader TDCtx) sig m
  , Has (Reader TVCtx) sig m
  , Has (Reader (SourceMap Raw)) sig m
  , Has (State (SourceMap Inferred)) sig m
  , Has (Reader TCStack) sig m
  , Has Unification sig m
  , Has (Error ContextualTypeErr) sig m
  ) =>
  Syntax Raw ->
  UType ->
  m (Syntax Inferred)
check s@(CSyntax l t cs) expected = addLocToTypeErr l $ case t of
  -- Once we're typechecking, we don't need to keep around explicit
  -- parens any more
  SParens t1 -> check t1 expected
  -- If t : ty, then  {t} : {ty}.
  SDelay s1 -> do
    ty1 <- decomposeDelayTy s (Expected, expected)
    s1' <- check s1 ty1
    return $ Syntax l (SDelay s1') cs (UTyDelay ty1)

  -- To check the type of a pair, make sure the expected type is a
  -- product type, and push the two types down into the left and right.
  SPair s1 s2 -> do
    (ty1, ty2) <- decomposeProdTy s (Expected, expected)
    s1' <- check s1 ty1
    s2' <- check s2 ty2
    return $ Syntax l (SPair s1' s2') cs (UTyProd ty1 ty2)

  -- To check a lambda, make sure the expected type is a function type.
  SLam x mxTy body -> do
    (argTy, resTy) <- decomposeFunTy s (Expected, expected)
    mxTy' <- traverse (adaptToTypeErr l KindErr . processType) mxTy
    forM_ (toU mxTy') $ \xTy -> do
      res <- argTy U.=:= xTy
      case res of
        -- Generate a special error when the explicit type annotation
        -- on a lambda doesn't match the expected type,
        -- e.g. (\x:Int. x + 2) : Text -> Int, since the usual
        -- "expected/but got" language would probably be confusing.
        Left _ -> throwTypeErr l $ LambdaArgMismatch (joined argTy xTy)
        Right _ -> return ()

    body' <- withBinding @Var @UPolytype (locVal x) (mkTrivPoly argTy) $ check body resTy
    return $ Syntax l (SLam x mxTy' body') cs (UTyFun argTy resTy)

  -- Special case for checking the argument to 'atomic' (or
  -- 'instant').  Both have the type @{Cmd a} -> Cmd a@.

  TConst c :$: at
    | c `elem` [Atomic, Instant] -> do
        argTy <- decomposeCmdTy s (Expected, expected)
        at' <- check at (UTyDelay (UTyCmd argTy))
        atomic' <- infer (RSyntax l (TConst c))
        -- It's important that we typecheck the subterm @at@ *before* we
        -- check that it is a valid argument to @atomic@: this way we can
        -- ensure that we have already inferred the types of any variables
        -- referenced.
        --
        -- When c is Atomic we validate that the argument to atomic is
        -- guaranteed to operate within a single tick.  When c is Instant
        -- we skip this check.
        when (c == Atomic) $ validAtomic at
        return $ Syntax l (SApp atomic' at') cs (UTyCmd argTy)

  -- Checking the type of a let- or def-expression.
  SLet ls r x mxTy _ _ t1 t2 -> withFrame l (TCLet (locVal x)) $ do
    mqxTy <- traverse quantify mxTy
    (skolems, upty, t1') <- case mqxTy of
      -- No type annotation was provided for the let binding, so infer its type.
      Nothing -> do
        -- The let could be recursive, so we must generate a fresh
        -- unification variable for the type of x and infer the type
        -- of t1 with x in the context.
        xTy <- fresh
        t1' <- withBinding @Var @UPolytype (locVal x) (mkTrivPoly xTy) $ infer t1
        let uty = t1' ^. sType
        uty' <- unify (Just t1) (joined xTy uty)
        upty <- generalize uty'
        return ([], upty, t1')
      -- An explicit polytype annotation has been provided. Perform
      -- implicit quantification, kind checking, and skolemization,
      -- then check definition and body under an extended context.
      Just pty -> do
        TydefInfo pty' _ <- adaptToTypeErr l KindErr . processPolytype $ pty
        let upty = toU pty'
        (ss, uty) <- skolemize upty
        t1' <- withBinding (locVal x) upty . withBindings ss $ check t1 uty
        return (Ctx.vars ss, upty, t1')

    -- Check the requirements of t1.
    tdCtx <- ask @TDCtx
    reqCtx <- ask @ReqCtx
    let Syntax _ tt1 _ _ = t1
        reqs = requirements tdCtx reqCtx tt1

    -- If we are checking a 'def', ensure t2 has a command type.  This ensures that
    -- something like 'def ... end; x + 3' is not allowed, since this
    -- would result in the whole thing being wrapped in pure, like
    -- 'pure (def ... end; x + 3)', which means the def would be local and
    -- not persist to the next REPL input, which could be surprising.
    --
    -- On the other hand, 'let x = y in x + 3' is perfectly fine.
    when (ls == LSDef) $ void $ decomposeCmdTy t2 (Expected, expected)

    -- Now check the type of the body, under a context extended with
    -- the type and requirements of the bound variable.
    t2' <-
      withBinding (locVal x) upty $
        withBinding (locVal x) reqs $
          check t2 expected

    -- Make sure none of the generated skolem variables have escaped.
    ask @UCtx >>= mapM_ (noSkolems l skolems)

    -- Annotate a 'def' with requirements, but not 'let'.  The reason
    -- is so that let introduces truly "local" bindings which never
    -- persist, but def introduces "global" bindings.  Variables bound
    -- in the environment can only be used to typecheck future REPL
    -- terms if the environment holds not only a value but also a type
    -- + requirements for them.  For example:
    --
    -- > def x : Int = 3 end; pure (x + 2)
    -- 5
    -- > x
    -- 3
    -- > let y : Int = 3 in y + 2
    -- 5
    -- > y
    -- 1:1: Undefined variable y
    -- > let y = 3 in def x = 5 end; pure (x + y)
    -- 8
    -- > y
    -- 1:1: Undefined variable y
    -- > x
    -- 5
    let mreqs = case ls of
          LSDef -> Just reqs
          LSLet -> Nothing

    -- Return the annotated let.
    return $ Syntax l (SLet ls r x mxTy mqxTy mreqs t1' t2') cs expected

  -- Kind-check a type definition and then check the body under an
  -- extended context.
  STydef x pty _ t1 -> do
    tydef@(TydefInfo pty' _) <- adaptToTypeErr l KindErr $ processPolytype pty
    t1' <- withBindingTD (tdVarName (locVal x)) tydef (check t1 expected)
    -- Eliminate the type alias in the reported type, since it is not
    -- in scope in the ambient context to which we report back the type.
    expected' <- elimTydef (locVal x) tydef <$> applyBindings expected
    return $ Syntax l (STydef x pty' (Just tydef) t1') cs expected'

  -- To check a record, ensure the expected type is a record type,
  -- ensure all the right fields are present, and push the expected
  -- types of all the fields down into recursive checks.
  --
  -- We have to be careful here --- if the expected type is not
  -- manifestly a record type but might unify with one (i.e. if the
  -- expected type is a variable) then we can't generate type
  -- variables for its subparts and push them, we have to switch
  -- completely into inference mode.  See Note [Checking and inference
  -- for record literals].
  SRcd fields
    | UTyRcd tyMap <- expected -> do
        let fieldMap = M.fromList $ map (first locVal) fields
            expectedFields = M.keysSet tyMap
            actualFields = M.keysSet fieldMap
        when (actualFields /= expectedFields) $
          throwTypeErr l $
            FieldsMismatch (joined expectedFields actualFields)
        -- Since we checked above that 'fields' and 'tyMap' have the
        -- same keys, we know this lookup into the tyMap will never fail;
        -- however, we still use lookup + mapMaybe to avoid partial functions.
        let fieldsWithTypes = mapMaybe (\(x, mt) -> (x,mt,) <$> M.lookup (locVal x) tyMap) fields
        fields' <-
          traverse
            (\(x, mt, ty) -> (x,) . Just <$> check (fromMaybe (RTerm (TVar (locVal x))) mt) ty)
            fieldsWithTypes
        return $ Syntax l (SRcd fields') cs expected

  -- The type of @suspend t@ is @Cmd T@ if @t : T@.
  SSuspend s1 -> do
    argTy <- decomposeCmdTy s (Expected, expected)
    s1' <- check s1 argTy
    return $ Syntax l (SSuspend s1') cs expected

  -- Fallback: switch into inference mode, and check that the type we
  -- get is what we expected.
  _ -> do
    Syntax l' t' _ actual <- infer s
    Syntax l' t' cs <$> unify (Just s) (joined expected actual)

-- ~~~~ Note [Checking and inference for record literals]
--
-- We need to handle record literals in both inference and checking
-- mode.  By way of contrast, with a pair, if we are in checking mode
-- and the expected type is not manifestly a product type, we can just
-- generate fresh unification variables for the types of the two
-- components, generate a constraint that the expected type is equal
-- to a product type of these two fresh types, and continue in
-- checking mode on both sides.  With records, however, we cannot do
-- that, since we don't know what field names to generate. If we are
-- checking a record and the expected type is not manifestly a record
-- type, we must simply switch into inference mode.  However, it is
-- still helpful to be able to handle records in checking mode too,
-- since if we know a record type it is helpful to be able to push the
-- field types down into the fields.

------------------------------------------------------------
-- Special atomic checking

-- | Ensure a term is a valid argument to @atomic@.  Valid arguments
--   may not contain @def@, @let@, or lambda. Any variables which are
--   referenced must have a primitive, first-order type such as
--   @Text@ or @Int@ (in particular, no functions, @Cmd@, or
--   @delay@).  We simply assume that any locally bound variables are
--   OK without checking their type: the only way to bind a variable
--   locally is with a binder of the form @x <- c1; c2@, where @c1@ is
--   some primitive command (since we can't refer to external
--   variables of type @Cmd a@).  If we wanted to do something more
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
validAtomic ::
  ( Has (Reader UCtx) sig m
  , Has (Reader TCStack) sig m
  , Has Unification sig m
  , Has (Throw ContextualTypeErr) sig m
  ) =>
  Syntax Raw ->
  m ()
validAtomic s@(RSyntax l t) = do
  n <- analyzeAtomic S.empty s
  when (n > 1) $ throwTypeErr l $ InvalidAtomic (TooManyTicks n) t

-- | Analyze an argument to @atomic@: ensure it contains no nested
--   atomic blocks and no references to external variables, and count
--   how many tangible commands it will execute.
analyzeAtomic ::
  ( Has (Reader UCtx) sig m
  , Has (Reader TCStack) sig m
  , Has Unification sig m
  , Has (Throw ContextualTypeErr) sig m
  ) =>
  Set Var ->
  Syntax Raw ->
  m Int
analyzeAtomic locals (RSyntax l t) = case t of
  -- Literals, primitives, etc. that are fine and don't require a tick
  -- to evaluate
  TUnit {} -> return 0
  TDir {} -> return 0
  TInt {} -> return 0
  TAntiInt {} -> return 0
  TText {} -> return 0
  TAntiText {} -> return 0
  TAntiSyn {} -> return 0
  TBool {} -> return 0
  TRobot {} -> return 0
  TRequire {} -> return 0
  TStock {} -> return 0
  SRequirements {} -> return 0
  STydef {} -> return 0
  TType {} -> return 0
  -- Constants.
  TConst c
    -- Nested 'atomic' is not allowed.
    | c == Atomic -> throwTypeErr l $ InvalidAtomic NestedAtomic t
    -- We cannot allow long commands (commands that may require more
    -- than one tick to execute) since that could freeze the game.
    | isLong c -> throwTypeErr l $ InvalidAtomic LongConst t
    -- Otherwise, return 1 or 0 depending on whether the command is
    -- tangible.
    | otherwise -> return $ if isTangible c then 1 else 0
  -- Special case for if: number of tangible commands is the *max* of
  -- the branches instead of the sum, since exactly one of them will be
  -- executed.
  TConst If :$: tst :$: thn :$: els ->
    (+) <$> analyzeAtomic locals tst <*> (max <$> analyzeAtomic locals thn <*> analyzeAtomic locals els)
  -- Pairs, application, delay, and parens are simple: just recurse and sum the results.
  SPair s1 s2 -> (+) <$> analyzeAtomic locals s1 <*> analyzeAtomic locals s2
  SApp s1 s2 -> (+) <$> analyzeAtomic locals s1 <*> analyzeAtomic locals s2
  SDelay s1 -> analyzeAtomic locals s1
  SParens s1 -> analyzeAtomic locals s1
  -- Bind is similarly simple except that we have to keep track of a local variable
  -- bound in the RHS.
  SBind mx _ _ _ s1 s2 -> (+) <$> analyzeAtomic locals s1 <*> analyzeAtomic (maybe id (S.insert . locVal) mx locals) s2
  SRcd m -> sum <$> mapM analyzeField m
   where
    analyzeField ::
      ( Has (Reader UCtx) sig m
      , Has (Reader TCStack) sig m
      , Has Unification sig m
      , Has (Throw ContextualTypeErr) sig m
      ) =>
      (LocVar, Maybe (Syntax Raw)) ->
      m Int
    analyzeField (Loc _ x, Nothing) = analyzeAtomic locals (RTerm (TVar x))
    analyzeField (_, Just s) = analyzeAtomic locals s
  SProj {} -> return 0
  -- Variables are allowed if bound locally, or if they have a simple type.
  TVar x
    | x `S.member` locals -> return 0
    | otherwise -> do
        mxTy <- Ctx.lookup x <$> ask @UCtx
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
              else throwTypeErr l $ InvalidAtomic (NonSimpleVarType x xTy') t
  -- No lambda, `let` or `def` allowed!
  SLam {} -> throwTypeErr l $ InvalidAtomic AtomicDupingThing t
  SLet {} -> throwTypeErr l $ InvalidAtomic AtomicDupingThing t
  -- We should never encounter a TRef since they do not show up in
  -- surface syntax, only as values while evaluating (*after*
  -- typechecking).
  TRef {} -> throwTypeErr l $ CantInfer t
  -- An explicit type annotation doesn't change atomicity
  SAnnotate s _ -> analyzeAtomic locals s
  -- We should never encounter a suspend since it cannot be written
  -- explicitly in the surface syntax.
  SSuspend {} -> throwTypeErr l $ InvalidAtomic AtomicSuspend t
  SImportIn {} -> throwTypeErr l $ InvalidAtomic AtomicImport t

-- | A simple polytype is a simple type with no quantifiers.
isSimpleUPolytype :: UPolytype -> Bool
isSimpleUPolytype (unPoly -> ([], ty)) = isSimpleUType ty
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
  Free.Pure {} -> False
  Free.Free {} -> False
