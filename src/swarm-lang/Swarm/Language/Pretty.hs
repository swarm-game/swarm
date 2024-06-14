{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Pretty-printing for the Swarm language.
module Swarm.Language.Pretty where

import Control.Lens.Combinators (pattern Empty)
import Control.Monad.Free (Free (..))
import Data.Bool (bool)
import Data.Fix
import Data.Foldable qualified as F
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Render.String qualified as RS
import Prettyprinter.Render.Text qualified as RT
import Swarm.Effect.Unify (UnificationError (..))
import Swarm.Language.Capability
import Swarm.Language.Context
import Swarm.Language.Kindcheck (KindError (..))
import Swarm.Language.Parser.Util (getLocRange)
import Swarm.Language.Syntax
import Swarm.Language.Syntax.Direction
import Swarm.Language.Typecheck
import Swarm.Language.Types
import Swarm.Util (number, showEnum, showLowT, unsnocNE)
import Witch

------------------------------------------------------------
-- PrettyPrec class + utilities

-- | Type class for things that can be pretty-printed, given a
--   precedence level of their context.
class PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann -- can replace with custom ann type later if desired

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

------------------------------------------------------------
-- PrettyPrec instances for terms, types, etc.

instance PrettyPrec Text where
  prettyPrec _ = pretty

instance PrettyPrec BaseTy where
  prettyPrec _ = pretty . drop 1 . show

instance PrettyPrec IntVar where
  prettyPrec _ = pretty . mkVarName "u"

-- | We can use the 'Wildcard' value to replace unification variables
--   when we don't care about them, e.g. to print out the shape of a
--   type like @(_ -> _) * _@
data Wildcard = Wildcard
  deriving (Eq, Ord, Show)

instance PrettyPrec Wildcard where
  prettyPrec _ _ = "_"

instance PrettyPrec TyCon where
  prettyPrec _ = \case
    TCBase b -> ppr b
    TCCmd -> "Cmd"
    TCDelay -> "Delay"
    TCSum -> "Sum"
    TCProd -> "Prod"
    TCFun -> "Fun"
    TCUser t -> pretty t

-- | Split a function type chain, so that we can pretty print
--   the type parameters aligned on each line when they don't fit.
class UnchainableFun t where
  unchainFun :: t -> NE.NonEmpty t

instance UnchainableFun Type where
  unchainFun (a :->: ty) = a <| unchainFun ty
  unchainFun ty = pure ty

instance UnchainableFun (Free TypeF ty) where
  unchainFun (Free (TyConF TCFun [ty1, ty2])) = ty1 <| unchainFun ty2
  unchainFun ty = pure ty

instance (PrettyPrec (t (Fix t))) => PrettyPrec (Fix t) where
  prettyPrec p = prettyPrec p . unFix

instance (PrettyPrec (t (Free t v)), PrettyPrec v) => PrettyPrec (Free t v) where
  prettyPrec p (Free t) = prettyPrec p t
  prettyPrec p (Pure v) = prettyPrec p v

instance (UnchainableFun t, PrettyPrec t, SubstRec t) => PrettyPrec (TypeF t) where
  prettyPrec p = \case
    TyVarF v -> pretty v
    TyRcdF m -> brackets $ hsep (punctuate "," (map prettyBinding (M.assocs m)))
    -- Special cases for type constructors with special syntax.
    -- Always use parentheses around sum and product types, see #1625
    TyConF TCSum [ty1, ty2] ->
      pparens (p > 0) $
        prettyPrec 2 ty1 <+> "+" <+> prettyPrec 2 ty2
    TyConF TCProd [ty1, ty2] ->
      pparens (p > 0) $
        prettyPrec 2 ty1 <+> "*" <+> prettyPrec 2 ty2
    TyConF TCDelay [ty] -> braces $ ppr ty
    TyConF TCFun [ty1, ty2] ->
      let (iniF, lastF) = unsnocNE $ ty1 <| unchainFun ty2
          funs = (prettyPrec 2 <$> iniF) <> [prettyPrec 1 lastF]
          inLine l r = l <+> "->" <+> r
          multiLine l r = l <+> "->" <> softline <> r
       in pparens' (p > 1) . align $
            flatAlt (concatWith multiLine funs) (concatWith inLine funs)
    TyRecF x ty ->
      pparens (p > 0) $
        "rec" <+> pretty x <> "." <+> prettyPrec 0 (substRec (TyVarF x) ty NZ)
    -- This case shouldn't be possible, since TyRecVar should only occur inside a TyRec,
    -- and pretty-printing the TyRec (above) will substitute a variable name for
    -- any bound TyRecVars before recursing.
    TyRecVarF i -> pretty (show (natToInt i))
    -- Fallthrough cases for type constructor application.  Handles base
    -- types, Cmd, user-defined types, or ill-kinded things like 'Int
    -- Bool'.
    TyConF c [] -> ppr c
    TyConF c tys -> pparens (p > 9) $ ppr c <+> hsep (map (prettyPrec 10) tys)

instance PrettyPrec Polytype where
  prettyPrec _ (Forall [] t) = ppr t
  prettyPrec _ (Forall xs t) = hsep ("∀" : map pretty xs) <> "." <+> ppr t

instance PrettyPrec UPolytype where
  prettyPrec _ (Forall [] t) = ppr t
  prettyPrec _ (Forall xs t) = hsep ("∀" : map pretty xs) <> "." <+> ppr t

instance (PrettyPrec t) => PrettyPrec (Ctx t) where
  prettyPrec _ Empty = emptyDoc
  prettyPrec _ (assocs -> bs) = brackets (hsep (punctuate "," (map prettyBinding bs)))

prettyBinding :: (Pretty a, PrettyPrec b) => (a, b) -> Doc ann
prettyBinding (x, ty) = pretty x <> ":" <+> ppr ty

instance PrettyPrec Direction where
  prettyPrec _ = pretty . directionSyntax

instance PrettyPrec Capability where
  prettyPrec _ c = pretty $ T.toLower (from (NE.tail $ showEnum c))

instance PrettyPrec Const where
  prettyPrec p c = pparens (p > fixity (constInfo c)) $ pretty . syntax . constInfo $ c

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

instance PrettyPrec Comment where
  prettyPrec _ (Comment _ LineComment _ txt) = "//" <> pretty txt
  prettyPrec _ (Comment _ BlockComment _ txt) = "/*" <> pretty txt <> "*/"

instance PrettyPrec (Term' ty) where
  prettyPrec p = \case
    TUnit -> "()"
    TConst c -> prettyPrec p c
    TDir d -> ppr d
    TInt n -> pretty n
    TAntiInt v -> "$int:" <> pretty v
    TText s -> fromString (show s)
    TAntiText v -> "$str:" <> pretty v
    TBool b -> bool "false" "true" b
    TRobot r -> "<a" <> pretty r <> ">"
    TRef r -> "@" <> pretty r
    TRequireDevice d -> pparens (p > 10) $ "require" <+> ppr @Term (TText d)
    TRequire n e -> pparens (p > 10) $ "require" <+> pretty n <+> ppr @Term (TText e)
    SRequirements _ e -> pparens (p > 10) $ "requirements" <+> ppr e
    TVar s -> pretty s
    SDelay _ (Syntax' _ (TConst Noop) _ _) -> "{}"
    SDelay _ t -> group . encloseWithIndent 2 lbrace rbrace $ ppr t
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
        prettyDefinition "def" x mty t1 <+> "end"
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

prettyEquality :: (Pretty a, PrettyPrec b) => (a, Maybe b) -> Doc ann
prettyEquality (x, Nothing) = pretty x
prettyEquality (x, Just t) = pretty x <+> "=" <+> ppr t

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

appliedTermPrec :: Term -> Int
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
    , ppr (BulletList "" tcStack)
    ]
 where
  teLoc = case l of
    SrcLoc s e -> (showLoc . fst $ getLocRange code (s, e)) <> ": "
    NoLoc -> emptyDoc
  showLoc (r, c) = pretty r <> ":" <> pretty c

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
