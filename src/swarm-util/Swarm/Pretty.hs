{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Common pretty-printing infrastructure for the Swarm project.
module Swarm.Pretty (
  PrettyPrec (..),
  ppr,
  prettyText,
  prettyTextWidth,
  prettyTextLine,
  prettyString,
  docToText,
  docToTextWidth,
  docToString,
  pparens,
  pparens',
  encloseWithIndent,
  bquote,
  prettyShowLow,
  reportBug,
  Prec (..),
  BulletList (..),
  prettyBinding,
  prettyEquality,
  Wildcard (..),
) where

-- import Control.Lens.Combinators (pattern Empty)
-- import Control.Monad.Free (Free (..))
-- import Data.Bool (bool)
-- import Data.Fix
-- import Data.Foldable qualified as F
-- import Data.List.NonEmpty ((<|))
-- import Data.List.NonEmpty qualified as NE
-- import Data.Map.Strict qualified as M
-- import Data.Sequence qualified as Seq
-- import Data.Set (Set)
-- import Data.Set qualified as S
-- import Data.String (fromString)
import Data.Text (Text)

-- import Data.Text qualified as T
import Prettyprinter

-- import Prettyprinter.Render.String qualified as RS
-- import Prettyprinter.Render.Text qualified as RT

-- import Swarm.Effect.Unify (UnificationError (..))
-- import Swarm.Language.Capability
-- import Swarm.Language.Context
-- import Swarm.Language.Kindcheck (KindError (..))
-- import Swarm.Language.Parser.Util (getLocRange)
-- import Swarm.Language.Syntax
-- import Swarm.Language.Syntax.Direction
-- import Swarm.Language.Typecheck
-- import Swarm.Language.Types
-- import Swarm.Util (number, showEnum, showLowT, unsnocNE)
-- import Text.Show.Unicode (ushow)
-- import Witch

------------------------------------------------------------
-- PrettyPrec class + utilities

-- | Type class for things that can be pretty-printed, given a
--   precedence level of their context.
class PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann -- can replace with custom ann type later if desired

instance PrettyPrec Text where
  prettyPrec _ = pretty

-- | Pretty-print a thing, with a context precedence level of zero.
ppr :: (PrettyPrec a) => a -> Doc ann
ppr = prettyPrec 0

-- | Render a pretty-printed document as @Text@.
docToText :: Doc a -> Text
docToText = RT.renderStrict . layoutPretty defaultLayoutOptions

-- | Render a pretty-printed document as @Text@.
--   This function consumes number of allowed characters in a
--   line before introducing a line break. In other words, it
--   expects the space of the layouter to be supplied.
docToTextWidth :: Doc a -> Int -> Text
docToTextWidth doc layoutWidth =
  RT.renderStrict $ layoutPretty (LayoutOptions (AvailablePerLine layoutWidth 1.0)) doc

-- | Pretty-print something and render it as @Text@.
prettyText :: (PrettyPrec a) => a -> Text
prettyText = docToText . ppr

-- | Pretty-print something and render it as @Text@.
--   This is different than @prettyText@ in the sense that it also
--   consumes number of allowed characters in a line before introducing
--   a line break.
prettyTextWidth :: (PrettyPrec a) => a -> Int -> Text
prettyTextWidth = docToTextWidth . ppr

-- | Pretty-print something and render it as (preferably) one line @Text@.
prettyTextLine :: (PrettyPrec a) => a -> Text
prettyTextLine = RT.renderStrict . layoutPretty (LayoutOptions Unbounded) . group . ppr

-- | Render a pretty-printed document as a @String@.
docToString :: Doc a -> String
docToString = RS.renderString . layoutPretty defaultLayoutOptions

-- | Pretty-print something and render it as a @String@.
prettyString :: (PrettyPrec a) => a -> String
prettyString = docToString . ppr

-- | Optionally surround a document with parentheses depending on the
--   @Bool@ argument and if it does not fit on line, indent the lines,
--   with the parens on separate lines.
pparens :: Bool -> Doc ann -> Doc ann
pparens True = group . encloseWithIndent 2 lparen rparen
pparens False = id

-- | Same as pparens but does not indent the lines. Only encloses
--   the document with parantheses.
pparens' :: Bool -> Doc ann -> Doc ann
pparens' True = group . enclose lparen rparen
pparens' False = id

encloseWithIndent :: Int -> Doc ann -> Doc ann -> Doc ann -> Doc ann
encloseWithIndent i l r = nest i . enclose (l <> line') (nest (-2) $ line' <> r)

-- | Surround a document with backticks.
bquote :: Doc ann -> Doc ann
bquote = group . enclose "`" "`"

-- | Turn a 'Show' instance into a @Doc@, lowercasing it in the
--   process.
prettyShowLow :: Show a => a -> Doc ann
prettyShowLow = pretty . showLowT

-- | An invitation to report an error as a bug.
reportBug :: Doc ann
reportBug = "This should never happen; please report this as a bug: https://github.com/swarm-game/swarm/issues/new"

--------------------------------------------------
-- Bullet lists

data Prec a = Prec Int a

data BulletList i = BulletList
  { bulletListHeader :: forall a. Doc a
  , bulletListItems :: [i]
  }

instance (PrettyPrec i) => PrettyPrec (BulletList i) where
  prettyPrec _ (BulletList hdr items) =
    nest 2 . vcat $ hdr : map (("-" <+>) . ppr) items

--------------------------------------------------
-- Term- and type-printing utilities: bindings, equalities, wildcards,
-- etc.

prettyBinding :: (Pretty a, PrettyPrec b) => (a, b) -> Doc ann
prettyBinding (x, ty) = pretty x <> ":" <+> ppr ty

prettyEquality :: (Pretty a, PrettyPrec b) => (a, Maybe b) -> Doc ann
prettyEquality (x, Nothing) = pretty x
prettyEquality (x, Just t) = pretty x <+> "=" <+> ppr t

-- | We can use the 'Wildcard' value to replace unification variables
--   when we don't care about them, e.g. to print out the shape of a
--   type like @(_ -> _) * _@
data Wildcard = Wildcard
  deriving (Eq, Ord, Show)

instance PrettyPrec Wildcard where
  prettyPrec _ _ = "_"

-- XXX WORKING HERE --- BELOW STUFF MUST BE DISTRIBUTED

------------------------------------------------------------
-- Error messages

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

-- | Filter the TCStack of extravagant Binds.
filterTCStack :: TCStack -> TCStack
filterTCStack tcStack = case tcStack of
  [] -> []
  t@(LocatedTCFrame _ (TCLet _)) : _ -> [t]
  t@(LocatedTCFrame _ TCBindR) : xs -> t : filterTCStack xs
  t@(LocatedTCFrame _ TCBindL) : xs -> t : filterTCStack xs

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
    LambdaArgMismatch (getJoin -> (ty1, ty2)) ->
      "Lambda argument has type annotation" <+> pprCode ty2 <> ", but expected argument type" <+> pprCode ty1
    FieldsMismatch (getJoin -> (expFs, actFs)) ->
      fieldMismatchMsg expFs actFs
    EscapedSkolem x ->
      "Skolem variable" <+> pretty x <+> "would escape its scope"
    UnboundVar x ->
      "Unbound variable" <+> pretty x
    DefNotTopLevel t ->
      "Definitions may only be at the top level:" <+> pprCode t
    CantInfer t ->
      vsep
        [ "Couldn't infer the type of term:" <+> pprCode t
        , reportBug
        ]
    CantInferProj t ->
      "Can't infer the type of a record projection:" <+> pprCode t
    UnknownProj x t ->
      "Record does not have a field with name" <+> pretty x <> ":" <+> pprCode t
    InvalidAtomic reason t ->
      "Invalid atomic block:" <+> ppr reason <> ":" <+> pprCode t
    Impredicative ->
      "Unconstrained unification type variables encountered, likely due to an impredicative type. This is a known bug; for more information see https://github.com/swarm-game/swarm/issues/351 ."
   where
    pprCode :: PrettyPrec a => a -> Doc ann
    pprCode = bquote . ppr

instance PrettyPrec UnificationError where
  prettyPrec _ = \case
    Infinite x uty ->
      vsep
        [ "Encountered infinite type" <+> ppr x <+> "=" <+> ppr uty <> "."
        , "Swarm will not infer recursive types; if you want a recursive type, add an explicit type annotation."
        ]
    UnifyErr ty1 ty2 ->
      "Can't unify" <+> ppr ty1 <+> "and" <+> ppr ty2
    UndefinedUserType ty ->
      "Undefined user type" <+> ppr ty
    UnexpandedRecTy ty ->
      vsep
        [ "Unexpanded recursive type" <+> ppr ty <+> "encountered in unifyF."
        , reportBug
        ]

instance PrettyPrec Arity where
  prettyPrec _ (Arity a) = pretty a

instance PrettyPrec KindError where
  prettyPrec _ = \case
    ArityMismatch c a tys ->
      nest 2 . vsep $
        [ "Kind error:"
        , hsep
            [ ppr c
            , "requires"
            , pretty a
            , "type"
            , pretty (number a "argument" <> ",")
            , "but was given"
            , pretty (length tys)
            ]
        ]
          ++ ["in the type:" <+> ppr (TyConApp c tys) | not (null tys)]
    UndefinedTyCon tc _ty -> "Undefined type" <+> ppr tc
    TrivialRecTy x ty ->
      nest 2 . vsep $
        [ "Encountered trivial recursive type" <+> ppr (TyRec x ty)
        , "Did you forget to use" <+> pretty x <+> "in the body of the type?"
        ]
    VacuousRecTy x ty ->
      nest 2 . vsep $
        [ "Encountered vacuous recursive type" <+> ppr (TyRec x ty)
        , "Recursive types must be productive, i.e. must not expand to themselves."
        ]

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

-- | Check whether a type contains any unification variables at all.
hasAnyUVars :: UType -> Bool
hasAnyUVars = ucata (const True) or

-- | Check whether a type consists of a top-level type constructor
--   immediately applied to unification variables.
isTopLevelConstructor :: UType -> Maybe (TypeF ())
isTopLevelConstructor = \case
  Free (TyRcdF m) | all isPure m -> Just (TyRcdF M.empty)
  UTyConApp c ts | all isPure ts -> Just (TyConF c [])
  _ -> Nothing

isPure :: Free f a -> Bool
isPure (Pure {}) = True
isPure _ = False

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
  TCUser t -> pretty t

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
  prettyFieldSet = hsep . punctuate "," . map (bquote . pretty) . S.toList

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

instance PrettyPrec LocatedTCFrame where
  prettyPrec p (LocatedTCFrame _ f) = prettyPrec p f

instance PrettyPrec TCFrame where
  prettyPrec _ = \case
    TCLet x -> "While checking the definition of" <+> pretty x
    TCBindL -> "While checking the left-hand side of a semicolon"
    TCBindR -> "While checking the right-hand side of a semicolon"
