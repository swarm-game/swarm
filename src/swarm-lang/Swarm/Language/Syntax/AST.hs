{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types represeting the surface syntax and terms for Swarm programming language.
module Swarm.Language.Syntax.AST (
  Syntax' (..),
  LetSyntax (..),
  Term' (..),
) where

import Control.Lens (Plated (..), pattern Empty)
import Data.Aeson.Types hiding (Key)
import Data.Data (Data)
import Data.Data.Lens (uniplate)
import Data.Foldable qualified as F
import Data.Map.Strict (Map)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (Doc, Pretty)
import Swarm.Language.Requirements.Type (Requirements)
import Swarm.Language.Syntax.Comments
import Swarm.Language.Syntax.Constants
import Swarm.Language.Syntax.Direction
import Swarm.Language.Syntax.Loc
import Swarm.Language.Types
import Swarm.Pretty (PrettyPrec (..))

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
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Generic)

instance Data ty => Plated (Syntax' ty) where
  plate = uniplate

-- | Pretty-print a syntax node with comments.
instance PrettyPrec (Syntax' ty) where
  prettyPrec p (Syntax' _ t (Comments before after) _) = case before of
    Empty -> t'
    _ ->
      -- Print out any comments before the node, with a blank line before
      mconcat
        [ hardline
        , vsep (map ppr (F.toList before))
        , hardline
        , t'
        ]
   where
    -- Print the node itself, possibly with suffix comments on the same line
    t' = case Seq.viewr after of
      Seq.EmptyR -> prettyPrec p t
      _ Seq.:> lst -> case commentType lst of
        -- Output a newline after a line comment, but not after a block comment
        BlockComment -> tWithComments
        LineComment -> tWithComments <> hardline
     where
      -- The pretty-printed node with suffix comments
      tWithComments = prettyPrec p t <+> hsep (map ppr (F.toList after))

-- | A @let@ expression can be written either as @let x = e1 in e2@ or
--   as @def x = e1 end; e2@. This enumeration simply records which it
--   was so that we can pretty-print appropriatly.
data LetSyntax = LSLet | LSDef
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, Data, ToJSON, FromJSON)

------------------------------------------------------------
-- Term: basic syntax tree
------------------------------------------------------------

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
  | -- | A (recursive) let/def expression, with or without a type
    --   annotation on the variable. The @Bool@ indicates whether
    --   it is known to be recursive.
    --
    --   The @Maybe Requirements@ field is only for annotating the
    --   requirements of a definition after typechecking; there is no
    --   way to annotate requirements in the surface syntax.
    SLet LetSyntax Bool LocVar (Maybe Polytype) (Maybe Requirements) (Syntax' ty) (Syntax' ty)
  | -- | A type synonym definition.  Note that this acts like a @let@
    --   (just like @def@), /i.e./ the @Syntax' ty@ field is the local
    --   context over which the type definition is in scope.
    STydef LocVar Polytype (Maybe TydefInfo) (Syntax' ty)
  | -- | A monadic bind for commands, of the form @c1 ; c2@ or @x <- c1; c2@.
    --
    --   The @Maybe ty@ field is a place to stash the inferred type of
    --   the variable (if any) during type inference.  Once type
    --   inference is complete, during elaboration we will copy the
    --   inferred type into the @Maybe Polytype@ field (since the
    --   @Maybe ty@ field will be erased).
    --
    --   The @Maybe Polytype@ and @Maybe Requirements@ fields is only
    --   for annotating the type of a bind after typechecking; there
    --   is no surface syntax that allows directly annotating a bind
    --   with either one.
    SBind (Maybe LocVar) (Maybe ty) (Maybe Polytype) (Maybe Requirements) (Syntax' ty) (Syntax' ty)
  | -- | Delay evaluation of a term, written @{...}@.  Swarm is an
    --   eager language, but in some cases (e.g. for @if@ statements
    --   and recursive bindings) we need to delay evaluation.  The
    --   counterpart to @{...}@ is @force@, where @force {t} = t@.
    --   Note that 'Force' is just a constant, whereas 'SDelay' has to
    --   be a special syntactic form so its argument can get special
    --   treatment during evaluation.
    SDelay (Syntax' ty)
  | -- | Record literals @[x1 = e1, x2 = e2, x3, ...]@ Names @x@
    --   without an accompanying definition are sugar for writing
    --   @x=x@.
    SRcd (Map Var (Maybe (Syntax' ty)))
  | -- | Record projection @e.x@
    SProj (Syntax' ty) Var
  | -- | Annotate a term with a type
    SAnnotate (Syntax' ty) Polytype
  | -- | Run the given command, then suspend and wait for a new REPL
    --   input.
    SSuspend (Syntax' ty)
  deriving
    ( Eq
    , Show
    , Functor
    , Foldable
    , Data
    , Generic
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

instance Data ty => Plated (Term' ty) where
  plate = uniplate

------------------------------------------------------------
-- Pretty-printing for terms

instance PrettyPrec (Term' ty) where
  prettyPrec p = \case
    TUnit -> "()"
    TConst c -> prettyPrec p c
    TDir d -> ppr d
    TInt n -> pretty n
    TAntiInt v -> "$int:" <> pretty v
    TText s -> fromString (ushow s)
    TAntiText v -> "$str:" <> pretty v
    TBool b -> bool "false" "true" b
    TRobot r -> "<a" <> pretty r <> ">"
    TRef r -> "@" <> pretty r
    TRequireDevice d -> pparens (p > 10) $ "require" <+> ppr (TText d)
    TRequire n e -> pparens (p > 10) $ "require" <+> pretty n <+> ppr (TText e)
    SRequirements _ e -> pparens (p > 10) $ "requirements" <+> ppr e
    TVar s -> pretty s
    SDelay (Syntax' _ (TConst Noop) _ _) -> "{}"
    SDelay t -> group . encloseWithIndent 2 lbrace rbrace $ ppr t
    t@SPair {} -> prettyTuple t
    t@SLam {} ->
      pparens (p > 9) $
        prettyLambdas t
    -- Special handling of infix operators - ((+) 2) 3 --> 2 + 3
    SApp t@(Syntax' _ (SApp (Syntax' _ (TConst c) _ _) l) _ _) r ->
      let ci = constInfo c
          pC = fixity ci
       in case constMeta ci of
            ConstMBinOp assoc ->
              pparens (p > pC) $
                hsep
                  [ prettyPrec (pC + fromEnum (assoc == R)) l
                  , ppr c
                  , prettyPrec (pC + fromEnum (assoc == L)) r
                  ]
            _ -> prettyPrecApp p t r
    SApp t1 t2 -> case t1 of
      Syntax' _ (TConst c) _ _ ->
        let ci = constInfo c
            pC = fixity ci
         in case constMeta ci of
              ConstMUnOp P -> pparens (p > pC) $ ppr t1 <> prettyPrec (succ pC) t2
              ConstMUnOp S -> pparens (p > pC) $ prettyPrec (succ pC) t2 <> ppr t1
              _ -> prettyPrecApp p t1 t2
      _ -> prettyPrecApp p t1 t2
    SLet LSLet _ (LV _ x) mty _ t1 t2 ->
      sep
        [ prettyDefinition "let" x mty t1 <+> "in"
        , ppr t2
        ]
    SLet LSDef _ (LV _ x) mty _ t1 t2 ->
      mconcat $
        sep [prettyDefinition "def" x mty t1, "end"]
          : case t2 of
            Syntax' _ (TConst Noop) _ _ -> []
            _ -> [hardline, hardline, ppr t2]
    STydef (LV _ x) pty _ t1 ->
      mconcat $
        prettyTydef x pty
          : case t1 of
            Syntax' _ (TConst Noop) _ _ -> []
            _ -> [hardline, hardline, ppr t1]
    SBind Nothing _ _ _ t1 t2 ->
      pparens (p > 0) $
        prettyPrec 1 t1 <> ";" <> line <> prettyPrec 0 t2
    SBind (Just (LV _ x)) _ _ _ t1 t2 ->
      pparens (p > 0) $
        pretty x <+> "<-" <+> prettyPrec 1 t1 <> ";" <> line <> prettyPrec 0 t2
    SRcd m -> brackets $ hsep (punctuate "," (map prettyEquality (M.assocs m)))
    SProj t x -> prettyPrec 11 t <> "." <> pretty x
    SAnnotate t pt ->
      pparens (p > 0) $
        prettyPrec 1 t <+> ":" <+> ppr pt
    SSuspend t ->
      pparens (p > 10) $
        "suspend" <+> prettyPrec 11 t

prettyDefinition :: Doc ann -> Var -> Maybe Polytype -> Syntax' ty -> Doc ann
prettyDefinition defName x mty t1 =
  nest 2 . sep $
    [ flatAlt
        (defHead <> group defType <+> eqAndLambdaLine)
        (defHead <> group defType' <+> defEqLambdas)
    , ppr defBody
    ]
 where
  (defBody, defLambdaList) = unchainLambdas t1
  defHead = defName <+> pretty x
  defType = maybe "" (\ty -> ":" <+> flatAlt (line <> indent 2 (ppr ty)) (ppr ty)) mty
  defType' = maybe "" (\ty -> ":" <+> ppr ty) mty
  defEqLambdas = hsep ("=" : map prettyLambda defLambdaList)
  eqAndLambdaLine = if null defLambdaList then "=" else line <> defEqLambdas

prettyTydef :: Var -> Polytype -> Doc ann
prettyTydef x (Forall [] ty) = "tydef" <+> pretty x <+> "=" <+> ppr ty <+> "end"
prettyTydef x (Forall xs ty) = "tydef" <+> pretty x <+> hsep (map pretty xs) <+> "=" <+> ppr ty <+> "end"

prettyPrecApp :: Int -> Syntax' ty -> Syntax' ty -> Doc a
prettyPrecApp p t1 t2 =
  pparens (p > 10) $
    prettyPrec 10 t1 <+> prettyPrec 11 t2

appliedTermPrec :: Term' () -> Int
appliedTermPrec (TApp f _) = case f of
  TConst c -> fixity $ constInfo c
  _ -> appliedTermPrec f
appliedTermPrec _ = 10

prettyTuple :: Term' ty -> Doc a
prettyTuple = tupled . map ppr . unTuple . STerm . erase

prettyLambdas :: Term' ty -> Doc a
prettyLambdas t = hsep (prettyLambda <$> lms) <> softline <> ppr rest
 where
  (rest, lms) = unchainLambdas (STerm (erase t))

unchainLambdas :: Syntax' ty -> (Syntax' ty, [(Var, Maybe Type)])
unchainLambdas = \case
  Syntax' _ (SLam (LV _ x) mty body) _ _ -> ((x, mty) :) <$> unchainLambdas body
  body -> (body, [])

prettyLambda :: (Pretty a1, PrettyPrec a2) => (a1, Maybe a2) -> Doc ann
prettyLambda (x, mty) = "\\" <> pretty x <> maybe "" ((":" <>) . ppr) mty <> "."
