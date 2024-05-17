{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Abstract syntax for terms of the Swarm programming language.
module Swarm.Language.Syntax (
  -- * Directions
  Direction (..),
  AbsoluteDir (..),
  RelativeDir (..),
  PlanarRelativeDir (..),
  directionSyntax,
  isCardinal,
  allDirs,

  -- * Constants
  Const (..),
  allConst,
  ConstInfo (..),
  ConstDoc (..),
  ConstMeta (..),
  MBinAssoc (..),
  MUnAssoc (..),
  constInfo,
  arity,
  isCmd,
  isUserFunc,
  isOperator,
  isBuiltinFunction,
  isTangible,
  isLong,

  -- * Size limits
  maxSniffRange,
  maxScoutRange,
  maxStrideRange,
  maxPathRange,
  globalMaxVolume,

  -- * SrcLoc
  SrcLoc (..),
  srcLocBefore,
  noLoc,

  -- * Comments
  CommentType (..),
  CommentSituation (..),
  isStandalone,
  Comment (..),
  Comments (..),
  beforeComments,
  afterComments,

  -- * Syntax
  Syntax' (..),
  sLoc,
  sTerm,
  sType,
  sComments,
  Syntax,
  pattern Syntax,
  pattern CSyntax,
  LocVar (..),
  pattern STerm,
  pattern TRequirements,
  pattern TPair,
  pattern TLam,
  pattern TApp,
  pattern (:$:),
  pattern TLet,
  pattern TDef,
  pattern TBind,
  pattern TDelay,
  pattern TRcd,
  pattern TProj,
  pattern TAnnotate,

  -- * Terms
  Var,
  DelayType (..),
  Term' (..),
  Term,
  mkOp,
  mkOp',
  unfoldApps,
  mkTuple,
  unTuple,

  -- * Erasure
  erase,
  eraseS,

  -- * Term traversal
  freeVarsS,
  freeVarsT,
  freeVarsV,
  mapFreeS,
  locVarToSyntax',
  asTree,
  measureAstSize,
) where

import Control.Lens (AsEmpty, Plated (..), Traversal', makeLenses, para, universe, (%~), (^.), pattern Empty)
import Control.Monad (void)
import Data.Aeson.Types hiding (Key)
import Data.Data (Data)
import Data.Data.Lens (uniplate)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set qualified as S
import Data.Text hiding (filter, length, map)
import Data.Tree
import GHC.Generics (Generic)
import Swarm.Language.Direction
import Swarm.Language.Syntax.Constants
import Swarm.Language.Types

------------------------------------------------------------
-- SrcLoc
------------------------------------------------------------

-- | The location of something in the textual source code (recorded as
--   an interval measured in terms of indices into the input stream).
data SrcLoc
  = NoLoc
  | -- | Half-open interval from start (inclusive) to end (exclusive)
    SrcLoc Int Int
  deriving (Eq, Ord, Show, Data, Generic, FromJSON, ToJSON)

-- | @x <> y@ is the smallest 'SrcLoc' that subsumes both @x@ and @y@.
instance Semigroup SrcLoc where
  NoLoc <> l = l
  l <> NoLoc = l
  SrcLoc s1 e1 <> SrcLoc s2 e2 = SrcLoc (min s1 s2) (max e1 e2)

-- | @mempty@ is a special value which means we have no location
--   information.
instance Monoid SrcLoc where
  mempty = NoLoc

-- | Check whether one @SrcLoc@ starts at or before another one,
--   /i.e./ compare their starting indices to see if the first is @<=@
--   the second.
srcLocBefore :: SrcLoc -> SrcLoc -> Bool
srcLocBefore (SrcLoc a _) (SrcLoc b _) = a <= b
srcLocBefore _ _ = False

------------------------------------------------------------
-- Comments
------------------------------------------------------------

-- | Line vs block comments.
data CommentType = LineComment | BlockComment
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Data, ToJSON, FromJSON)

-- | Was a comment all by itself on a line, or did it occur after some
--   other tokens on a line?
data CommentSituation = StandaloneComment | SuffixComment
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Data, ToJSON, FromJSON)

-- | Test whether a comment is a standalone comment or not.
isStandalone :: Comment -> Bool
isStandalone = (== StandaloneComment) . commentSituation

-- | A comment is retained as some text plus metadata (source
--   location, comment type, + comment situation).  While parsing we
--   record all comments out-of-band, for later re-insertion into the
--   AST.
data Comment = Comment
  { commentSrcLoc :: SrcLoc
  , commentType :: CommentType
  , commentSituation :: CommentSituation
  , commentText :: Text
  }
  deriving (Eq, Show, Generic, Data, ToJSON, FromJSON)

-- | Comments which can be attached to a particular AST node.  Some
--   comments come textually before the node and some come after.
data Comments = Comments
  { _beforeComments :: Seq Comment
  , _afterComments :: Seq Comment
  }
  deriving (Eq, Show, Generic, Data, ToJSON, FromJSON)

makeLenses ''Comments

instance Semigroup Comments where
  Comments b1 a1 <> Comments b2 a2 = Comments (b1 <> b2) (a1 <> a2)

instance Monoid Comments where
  mempty = Comments mempty mempty

instance AsEmpty Comments

------------------------------------------------------------
-- Basic terms
------------------------------------------------------------

-- | Different runtime behaviors for delayed expressions.
data DelayType
  = -- | A simple delay, implemented via a (non-memoized) @VDelay@
    --   holding the delayed expression.
    SimpleDelay
  | -- | A memoized delay, implemented by allocating a mutable cell
    --   with the delayed expression and returning a reference to it.
    --   When the @Maybe Var@ is @Just@, a recursive binding of the
    --   variable with a reference to the delayed expression will be
    --   provided while evaluating the delayed expression itself. Note
    --   that there is no surface syntax for binding a variable within
    --   a recursive delayed expression; the only way we can get
    --   @Just@ here is when we automatically generate a delayed
    --   expression while interpreting a recursive @let@ or @def@.
    MemoizedDelay (Maybe Var)
  deriving (Eq, Show, Data, Generic, FromJSON, ToJSON)

-- | A variable with associated source location, used for variable
--   binding sites. (Variable occurrences are a bare TVar which gets
--   wrapped in a Syntax node, so we don't need LocVar for those.)
data LocVar = LV {lvSrcLoc :: SrcLoc, lvVar :: Var}
  deriving (Eq, Ord, Show, Data, Generic, FromJSON, ToJSON)

locVarToSyntax' :: LocVar -> ty -> Syntax' ty
locVarToSyntax' (LV s v) = Syntax' s (TVar v) Empty

-- | Terms of the Swarm language.
data Term' ty
  = -- | The unit value.
    TUnit
  | -- | A constant.
    TConst Const
  | -- | A direction literal.
    TDir Direction
  | -- | An integer literal.
    TInt Integer
  | -- | An antiquoted Haskell variable name of type Integer.
    TAntiInt Text
  | -- | A text literal.
    TText Text
  | -- | An antiquoted Haskell variable name of type Text.
    TAntiText Text
  | -- | A Boolean literal.
    TBool Bool
  | -- | A robot reference.  These never show up in surface syntax, but are
    --   here so we can factor pretty-printing for Values through
    --   pretty-printing for Terms.
    TRobot Int
  | -- | A memory reference.  These likewise never show up in surface syntax,
    --   but are here to facilitate pretty-printing.
    TRef Int
  | -- | Require a specific device to be installed.
    TRequireDevice Text
  | -- | Require a certain number of an entity.
    TRequire Int Text
  | -- | Primitive command to log requirements of a term.  The Text
    --   field is to store the unaltered original text of the term, for use
    --   in displaying the log message (since once we get to execution time the
    --   original term may have been elaborated, e.g. `force` may have been added
    --   around some variables, etc.)
    SRequirements Text (Syntax' ty)
  | -- | A variable.
    TVar Var
  | -- | A pair.
    SPair (Syntax' ty) (Syntax' ty)
  | -- | A lambda expression, with or without a type annotation on the
    --   binder.
    SLam LocVar (Maybe Type) (Syntax' ty)
  | -- | Function application.
    SApp (Syntax' ty) (Syntax' ty)
  | -- | A (recursive) let expression, with or without a type
    --   annotation on the variable. The @Bool@ indicates whether
    --   it is known to be recursive.
    SLet Bool LocVar (Maybe Polytype) (Syntax' ty) (Syntax' ty)
  | -- | A (recursive) definition command, which binds a variable to a
    --   value in subsequent commands. The @Bool@ indicates whether the
    --   definition is known to be recursive.
    SDef Bool LocVar (Maybe Polytype) (Syntax' ty)
  | -- | A monadic bind for commands, of the form @c1 ; c2@ or @x <- c1; c2@.
    SBind (Maybe LocVar) (Syntax' ty) (Syntax' ty)
  | -- | Delay evaluation of a term, written @{...}@.  Swarm is an
    --   eager language, but in some cases (e.g. for @if@ statements
    --   and recursive bindings) we need to delay evaluation.  The
    --   counterpart to @{...}@ is @force@, where @force {t} = t@.
    --   Note that 'Force' is just a constant, whereas 'SDelay' has to
    --   be a special syntactic form so its argument can get special
    --   treatment during evaluation.
    SDelay DelayType (Syntax' ty)
  | -- | Record literals @[x1 = e1, x2 = e2, x3, ...]@ Names @x@
    --   without an accompanying definition are sugar for writing
    --   @x=x@.
    SRcd (Map Var (Maybe (Syntax' ty)))
  | -- | Record projection @e.x@
    SProj (Syntax' ty) Var
  | -- | Annotate a term with a type
    SAnnotate (Syntax' ty) Polytype
  deriving
    ( Eq
    , Show
    , Functor
    , Foldable
    , Data
    , Generic
    , FromJSON
    , ToJSON
    , -- | The Traversable instance for Term (and for Syntax') is used during
      -- typechecking: during intermediate type inference, many of the type
      -- annotations placed on AST nodes will have unification variables in
      -- them. Once we have finished solving everything we need to do a
      -- final traversal over all the types in the AST to substitute away
      -- all the unification variables (and generalize, i.e. stick 'forall'
      -- on, as appropriate).  See the call to 'mapM' in
      -- Swarm.Language.Typecheck.runInfer.
      Traversable
    )

type Term = Term' ()

instance Data ty => Plated (Term' ty) where
  plate = uniplate

------------------------------------------------------------
-- Syntax: annotation on top of Terms with SrcLoc, comments, + type
------------------------------------------------------------

-- | The surface syntax for the language, with location and type annotations.
data Syntax' ty = Syntax'
  { _sLoc :: SrcLoc
  , _sTerm :: Term' ty
  , _sComments :: Comments
  , _sType :: ty
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Generic, FromJSON, ToJSON)

instance Data ty => Plated (Syntax' ty) where
  plate = uniplate

------------------------------------------------------------
-- Pattern synonyms for untyped terms
------------------------------------------------------------

-- | Syntax without type annotations.
type Syntax = Syntax' ()

-- | Raw parsed syntax, without comments or type annotations.
pattern Syntax :: SrcLoc -> Term -> Syntax
pattern Syntax l t = Syntax' l t Empty ()

{-# COMPLETE Syntax #-}

-- | Untyped syntax with assocated comments.
pattern CSyntax :: SrcLoc -> Term -> Comments -> Syntax
pattern CSyntax l t cs = Syntax' l t cs ()

{-# COMPLETE CSyntax #-}

makeLenses ''Syntax'

noLoc :: Term -> Syntax
noLoc = Syntax mempty

-- | Match an untyped term without annotations.
pattern STerm :: Term -> Syntax
pattern STerm t <-
  CSyntax _ t _
  where
    STerm t = Syntax mempty t

pattern TRequirements :: Text -> Term -> Term
pattern TRequirements x t = SRequirements x (STerm t)

-- | Match a TPair without annotations.
pattern TPair :: Term -> Term -> Term
pattern TPair t1 t2 = SPair (STerm t1) (STerm t2)

-- | Match a TLam without annotations.
pattern TLam :: Var -> Maybe Type -> Term -> Term
pattern TLam v ty t <- SLam (lvVar -> v) ty (STerm t)
  where
    TLam v ty t = SLam (LV NoLoc v) ty (STerm t)

-- | Match a TApp without annotations.
pattern TApp :: Term -> Term -> Term
pattern TApp t1 t2 = SApp (STerm t1) (STerm t2)

infixl 0 :$:

-- | Convenient infix pattern synonym for application.
pattern (:$:) :: Term -> Syntax -> Term
pattern (:$:) t1 s2 = SApp (STerm t1) s2

-- | Match a TLet without annotations.
pattern TLet :: Bool -> Var -> Maybe Polytype -> Term -> Term -> Term
pattern TLet r v pt t1 t2 <- SLet r (lvVar -> v) pt (STerm t1) (STerm t2)
  where
    TLet r v pt t1 t2 = SLet r (LV NoLoc v) pt (STerm t1) (STerm t2)

-- | Match a TDef without annotations.
pattern TDef :: Bool -> Var -> Maybe Polytype -> Term -> Term
pattern TDef r v pt t <- SDef r (lvVar -> v) pt (STerm t)
  where
    TDef r v pt t = SDef r (LV NoLoc v) pt (STerm t)

-- | Match a TBind without annotations.
pattern TBind :: Maybe Var -> Term -> Term -> Term
pattern TBind mv t1 t2 <- SBind (fmap lvVar -> mv) (STerm t1) (STerm t2)
  where
    TBind mv t1 t2 = SBind (LV NoLoc <$> mv) (STerm t1) (STerm t2)

-- | Match a TDelay without annotations.
pattern TDelay :: DelayType -> Term -> Term
pattern TDelay m t = SDelay m (STerm t)

-- | Match a TRcd without annotations.
pattern TRcd :: Map Var (Maybe Term) -> Term
pattern TRcd m <- SRcd ((fmap . fmap) _sTerm -> m)
  where
    TRcd m = SRcd ((fmap . fmap) STerm m)

pattern TProj :: Term -> Var -> Term
pattern TProj t x = SProj (STerm t) x

-- | Match a TAnnotate without annotations.
pattern TAnnotate :: Term -> Polytype -> Term
pattern TAnnotate t pt = SAnnotate (STerm t) pt

-- COMPLETE pragma tells GHC using this set of patterns is complete for Term

{-# COMPLETE TUnit, TConst, TDir, TInt, TAntiInt, TText, TAntiText, TBool, TRequireDevice, TRequire, TRequirements, TVar, TPair, TLam, TApp, TLet, TDef, TBind, TDelay, TRcd, TProj, TAnnotate #-}

-- | Make an infix operation (e.g. @2 + 3@) a curried function
--   application (e.g. @((+) 2) 3@).
mkOp :: Const -> Syntax -> Syntax -> Syntax
mkOp c s1@(Syntax l1 _) s2@(Syntax l2 _) = Syntax newLoc newTerm
 where
  -- The new syntax span both terms
  newLoc = l1 <> l2
  -- We don't assign a source location for the operator since it is
  -- usually provided as-is and it is not likely to be useful.
  sop = noLoc (TConst c)
  newTerm = SApp (Syntax l1 $ SApp sop s1) s2

-- | Make an infix operation, discarding any location information
mkOp' :: Const -> Term -> Term -> Term
mkOp' c t1 = TApp (TApp (TConst c) t1)

-- $setup
-- >>> import Control.Lens ((^.))

-- | Turn function application chain into a list.
--
-- >>> syntaxWrap f = fmap (^. sTerm) . f . Syntax NoLoc
-- >>> syntaxWrap unfoldApps (mkOp' Mul (TInt 1) (TInt 2)) -- 1 * 2
-- TConst Mul :| [TInt 1,TInt 2]
unfoldApps :: Syntax' ty -> NonEmpty (Syntax' ty)
unfoldApps trm = NonEmpty.reverse . flip NonEmpty.unfoldr trm $ \case
  Syntax' _ (SApp s1 s2) _ _ -> (s2, Just s1)
  s -> (s, Nothing)

-- | Create a nested tuple out of a list of syntax nodes.
mkTuple :: [Syntax] -> Syntax
mkTuple [] = Syntax NoLoc TUnit -- should never happen
mkTuple [x] = x
mkTuple (x : xs) = let r = mkTuple xs in loc x r $ SPair x r
 where
  loc a b = Syntax $ (a ^. sLoc) <> (b ^. sLoc)

-- | Decompose a nested tuple into a list of components.
unTuple :: Syntax' ty -> [Syntax' ty]
unTuple = \case
  Syntax' _ (SPair s1 s2) _ _ -> s1 : unTuple s2
  s -> [s]

--------------------------------------------------
-- Type erasure

-- | Erase the type annotations from a 'Syntax' or 'Term' tree.
erase :: Functor t => t ty -> t ()
erase = void

-- | Erase all annotations from a 'Syntax' node, turning it into a
--   bare 'Term'.
eraseS :: Syntax' ty -> Term
eraseS (Syntax' _ t _ _) = erase t

------------------------------------------------------------
-- Free variable traversals
------------------------------------------------------------

-- | Traversal over those subterms of a term which represent free
--   variables.  The S suffix indicates that it is a `Traversal' over
--   the `Syntax` nodes (which contain type and source location info)
--   containing free variables inside a larger `Syntax` value.  Note
--   that if you want to get the list of all `Syntax` nodes
--   representing free variables, you can do so via @'toListOf'
--   'freeVarsS'@.
freeVarsS :: forall ty. Traversal' (Syntax' ty) (Syntax' ty)
freeVarsS f = go S.empty
 where
  -- go :: Applicative f => Set Var -> Syntax' ty -> f (Syntax' ty)
  go bound s@(Syntax' l t ty cmts) = case t of
    TUnit -> pure s
    TConst {} -> pure s
    TDir {} -> pure s
    TInt {} -> pure s
    TAntiInt {} -> pure s
    TText {} -> pure s
    TAntiText {} -> pure s
    TBool {} -> pure s
    TRobot {} -> pure s
    TRef {} -> pure s
    TRequireDevice {} -> pure s
    TRequire {} -> pure s
    SRequirements x s1 -> rewrap $ SRequirements x <$> go bound s1
    TVar x
      | x `S.member` bound -> pure s
      | otherwise -> f s
    SLam x xty s1 -> rewrap $ SLam x xty <$> go (S.insert (lvVar x) bound) s1
    SApp s1 s2 -> rewrap $ SApp <$> go bound s1 <*> go bound s2
    SLet r x xty s1 s2 ->
      let bound' = S.insert (lvVar x) bound
       in rewrap $ SLet r x xty <$> go bound' s1 <*> go bound' s2
    SPair s1 s2 -> rewrap $ SPair <$> go bound s1 <*> go bound s2
    SDef r x xty s1 -> rewrap $ SDef r x xty <$> go (S.insert (lvVar x) bound) s1
    SBind mx s1 s2 -> rewrap $ SBind mx <$> go bound s1 <*> go (maybe id (S.insert . lvVar) mx bound) s2
    SDelay m s1 -> rewrap $ SDelay m <$> go bound s1
    SRcd m -> rewrap $ SRcd <$> (traverse . traverse) (go bound) m
    SProj s1 x -> rewrap $ SProj <$> go bound s1 <*> pure x
    SAnnotate s1 pty -> rewrap $ SAnnotate <$> go bound s1 <*> pure pty
   where
    rewrap s' = Syntax' l <$> s' <*> pure ty <*> pure cmts

-- | Like 'freeVarsS', but traverse over the 'Term's containing free
--   variables.  More direct if you don't need to know the types or
--   source locations of the variables.  Note that if you want to get
--   the list of all `Term`s representing free variables, you can do
--   so via @'toListOf' 'freeVarsT'@.
freeVarsT :: forall ty. Traversal' (Syntax' ty) (Term' ty)
freeVarsT = freeVarsS . sTerm

-- | Traversal over the free variables of a term.  Like 'freeVarsS'
--   and 'freeVarsT', but traverse over the variable names themselves.
--   Note that if you want to get the set of all free variable names,
--   you can do so via @'Data.Set.Lens.setOf' 'freeVarsV'@.
freeVarsV :: Traversal' (Syntax' ty) Var
freeVarsV = freeVarsT . (\f -> \case TVar x -> TVar <$> f x; t -> pure t)

-- | Apply a function to all free occurrences of a particular
--   variable.
mapFreeS :: Var -> (Syntax' ty -> Syntax' ty) -> Syntax' ty -> Syntax' ty
mapFreeS x f = freeVarsS %~ (\t -> case t ^. sTerm of TVar y | y == x -> f t; _ -> t)

-- | Transform the AST into a Tree datatype.  Useful for
--   pretty-printing (e.g. via "Data.Tree.drawTree").
asTree :: Data a => Syntax' a -> Tree (Syntax' a)
asTree = para Node

-- | Each constructor is a assigned a value of 1, plus
--   any recursive syntax it entails.
measureAstSize :: Data a => Syntax' a -> Int
measureAstSize = length . universe
