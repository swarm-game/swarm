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
import Control.Unification
import Control.Unification.IntVar
import Data.Bool (bool)
import Data.Functor.Fixedpoint (Fix, unFix)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Render.String qualified as RS
import Prettyprinter.Render.Text qualified as RT
import Swarm.Language.Capability
import Swarm.Language.Context
import Swarm.Language.Parse (getLocRange)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck
import Swarm.Language.Types
import Swarm.Util (showLowT)
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

-- | Pretty-print something and render it as @Text@.
prettyText :: (PrettyPrec a) => a -> Text
prettyText = docToText . ppr

-- | Render a pretty-printed document as a @String@.
docToString :: Doc a -> String
docToString = RS.renderString . layoutPretty defaultLayoutOptions

-- | Pretty-print something and render it as a @String@.
prettyString :: (PrettyPrec a) => a -> String
prettyString = docToString . ppr

-- | Optionally surround a document with parentheses depending on the
--   @Bool@ argument.
pparens :: Bool -> Doc ann -> Doc ann
pparens True = group . parens
pparens False = id

-- | Surround a document with backticks.
bquote :: Doc ann -> Doc ann
bquote d = "`" <> d <> "`"

-- | Turn a 'Show' instance into a @Doc@, lowercasing it in the
--   process.
prettyShowLow :: Show a => a -> Doc ann
prettyShowLow = pretty . showLowT

--------------------------------------------------
-- Bullet lists

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
  prettyPrec _ BVoid = "void"
  prettyPrec _ BUnit = "unit"
  prettyPrec _ BInt = "int"
  prettyPrec _ BDir = "dir"
  prettyPrec _ BText = "text"
  prettyPrec _ BBool = "bool"
  prettyPrec _ BActor = "actor"
  prettyPrec _ BKey = "key"

instance PrettyPrec IntVar where
  prettyPrec _ = pretty . mkVarName "u"

-- | We can use the 'Wildcard' value to replace unification variables
--   when we don't care about them, e.g. to print out the shape of a
--   type like @(_ -> _) * _@
data Wildcard = Wildcard
  deriving (Eq, Ord, Show)

instance PrettyPrec Wildcard where
  prettyPrec _ _ = "_"

instance (PrettyPrec (t (Fix t))) => PrettyPrec (Fix t) where
  prettyPrec p = prettyPrec p . unFix

instance (PrettyPrec (t (UTerm t v)), PrettyPrec v) => PrettyPrec (UTerm t v) where
  prettyPrec p (UTerm t) = prettyPrec p t
  prettyPrec p (UVar v) = prettyPrec p v

instance (PrettyPrec t) => PrettyPrec (TypeF t) where
  prettyPrec _ (TyBaseF b) = ppr b
  prettyPrec _ (TyVarF v) = pretty v
  prettyPrec p (TySumF ty1 ty2) =
    pparens (p > 1) $
      prettyPrec 2 ty1 <+> "+" <+> prettyPrec 1 ty2
  prettyPrec p (TyProdF ty1 ty2) =
    pparens (p > 2) $
      prettyPrec 3 ty1 <+> "*" <+> prettyPrec 2 ty2
  prettyPrec p (TyCmdF ty) = pparens (p > 9) $ "cmd" <+> prettyPrec 10 ty
  prettyPrec _ (TyDelayF ty) = braces $ ppr ty
  prettyPrec p (TyFunF ty1 ty2) =
    pparens (p > 0) $
      prettyPrec 1 ty1 <+> "->" <+> prettyPrec 0 ty2
  prettyPrec _ (TyRcdF m) = brackets $ hsep (punctuate "," (map prettyBinding (M.assocs m)))

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
  prettyPrec _ c = pretty $ T.toLower (from (tail $ show c))

instance PrettyPrec Const where
  prettyPrec p c = pparens (p > fixity (constInfo c)) $ pretty . syntax . constInfo $ c

instance PrettyPrec (Syntax' ty) where
  prettyPrec p = prettyPrec p . eraseS

instance PrettyPrec Term where
  prettyPrec _ TUnit = "()"
  prettyPrec p (TConst c) = prettyPrec p c
  prettyPrec _ (TDir d) = ppr d
  prettyPrec _ (TInt n) = pretty n
  prettyPrec _ (TAntiInt v) = "$int:" <> pretty v
  prettyPrec _ (TText s) = fromString (show s)
  prettyPrec _ (TAntiText v) = "$str:" <> pretty v
  prettyPrec _ (TBool b) = bool "false" "true" b
  prettyPrec _ (TRobot r) = "<a" <> pretty r <> ">"
  prettyPrec _ (TRef r) = "@" <> pretty r
  prettyPrec p (TRequireDevice d) = pparens (p > 10) $ "require" <+> ppr @Term (TText d)
  prettyPrec p (TRequire n e) = pparens (p > 10) $ "require" <+> pretty n <+> ppr @Term (TText e)
  prettyPrec p (TRequirements _ e) = pparens (p > 10) $ "requirements" <+> ppr e
  prettyPrec _ (TVar s) = pretty s
  prettyPrec _ (TDelay _ t) = group . braces $ line' <> nest 2 (ppr t) <> line'
  prettyPrec _ t@TPair {} = prettyTuple t
  prettyPrec _ t@(TLam {}) = prettyLambdas t
  -- Special handling of infix operators - ((+) 2) 3 --> 2 + 3
  prettyPrec p (TApp t@(TApp (TConst c) l) r) =
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
  prettyPrec p (TApp t1 t2) = case t1 of
    TConst c ->
      let ci = constInfo c
          pC = fixity ci
       in case constMeta ci of
            ConstMUnOp P -> pparens (p > pC) $ ppr t1 <> prettyPrec (succ pC) t2
            ConstMUnOp S -> pparens (p > pC) $ prettyPrec (succ pC) t2 <> ppr t1
            _ -> prettyPrecApp p t1 t2
    _ -> prettyPrecApp p t1 t2
  prettyPrec _ (TLet _ x mty t1 t2) =
    group . nest 2 . vsep $
      [ hsep $
          ["let", pretty x]
          ++ maybe [] (\ty -> [":", ppr ty]) mty
          ++ ["=", ppr t1, "in"]
      , ppr t2
      ]
  prettyPrec _ (TDef _ x mty t1) =
    let (t1rest, t1lams) = unchainLambdas t1 in
    group . vsep $
      [ nest 2 $ vsep
        [ hsep $
          [ "def", pretty x]
          ++ maybe [] (\ty -> [":", ppr ty]) mty
          ++ ["="]
          ++ map prettyLambda t1lams
        , ppr t1rest
        ]
      , "end"
      ]
  prettyPrec p (TBind Nothing t1 t2) =
    pparens (p > 0) $
      prettyPrec 1 t1 <> ";" <> line <> prettyPrec 0 t2
  prettyPrec p (TBind (Just x) t1 t2) =
    pparens (p > 0) $
      pretty x <+> "<-" <+> prettyPrec 1 t1 <> ";" <> line <> prettyPrec 0 t2
  prettyPrec _ (TRcd m) = brackets $ hsep (punctuate "," (map prettyEquality (M.assocs m)))
  prettyPrec _ (TProj t x) = prettyPrec 11 t <> "." <> pretty x
  prettyPrec p (TAnnotate t pt) =
    pparens (p > 0) $
      prettyPrec 1 t <+> ":" <+> ppr pt

prettyEquality :: (Pretty a, PrettyPrec b) => (a, Maybe b) -> Doc ann
prettyEquality (x, Nothing) = pretty x
prettyEquality (x, Just t) = pretty x <+> "=" <+> ppr t

prettyTuple :: Term -> Doc a
prettyTuple = tupled . map ppr . unnestTuple
 where
  unnestTuple (TPair t1 t2) = t1 : unnestTuple t2
  unnestTuple t = [t]

prettyPrecApp :: Int -> Term -> Term -> Doc a
prettyPrecApp p t1 t2 =
  pparens (p > 10) $
    prettyPrec 10 t1 <+> prettyPrec 11 t2

appliedTermPrec :: Term -> Int
appliedTermPrec (TApp f _) = case f of
  TConst c -> fixity $ constInfo c
  _ -> appliedTermPrec f
appliedTermPrec _ = 10

prettyLambdas :: Term -> Doc a
prettyLambdas t = hsep (prettyLambda <$> lms) <> softline <> ppr rest
 where
  (rest, lms) = unchainLambdas t

unchainLambdas :: Term -> (Term, [(Var, Maybe Type)])
unchainLambdas = \case
  TLam x mty body -> ((x, mty) :) <$> unchainLambdas body
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
  prettyPrec _ (UnifyErr ty1 ty2) =
    "Can't unify" <+> ppr ty1 <+> "and" <+> ppr ty2
  prettyPrec _ (Mismatch Nothing (getJoin -> (ty1, ty2))) =
    "Type mismatch: expected" <+> ppr ty1 <> ", but got" <+> ppr ty2
  prettyPrec _ (Mismatch (Just t) (getJoin -> (ty1, ty2))) =
    nest 2 . vcat $
      [ "Type mismatch:"
      , "From context, expected" <+> bquote (ppr t) <+> "to" <+> typeDescription Expected ty1 <> ","
      , "but it" <+> typeDescription Actual ty2
      ]
  prettyPrec _ (LambdaArgMismatch (getJoin -> (ty1, ty2))) =
    "Lambda argument has type annotation" <+> bquote (ppr ty2) <> ", but expected argument type" <+> bquote (ppr ty1)
  prettyPrec _ (FieldsMismatch (getJoin -> (expFs, actFs))) = fieldMismatchMsg expFs actFs
  prettyPrec _ (EscapedSkolem x) =
    "Skolem variable" <+> pretty x <+> "would escape its scope"
  prettyPrec _ (UnboundVar x) =
    "Unbound variable" <+> pretty x
  prettyPrec _ (Infinite x uty) =
    "Infinite type:" <+> ppr x <+> "=" <+> ppr uty
  prettyPrec _ (DefNotTopLevel t) =
    "Definitions may only be at the top level:" <+> ppr t
  prettyPrec _ (CantInfer t) =
    "Couldn't infer the type of term (this shouldn't happen; please report this as a bug!):" <+> ppr t
  prettyPrec _ (CantInferProj t) =
    "Can't infer the type of a record projection:" <+> ppr t
  prettyPrec _ (UnknownProj x t) =
    "Record does not have a field with name" <+> pretty x <> ":" <+> ppr t
  prettyPrec _ (InvalidAtomic reason t) =
    "Invalid atomic block:" <+> ppr reason <> ":" <+> ppr t
  prettyPrec _ Impredicative =
    "Unconstrained unification type variables encountered, likely due to an impredicative type. This is a known bug; for more information see https://github.com/swarm-game/swarm/issues/351 ."

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
isTopLevelConstructor (UTyCmd (UVar {})) = Just $ TyCmdF ()
isTopLevelConstructor (UTyDelay (UVar {})) = Just $ TyDelayF ()
isTopLevelConstructor (UTySum (UVar {}) (UVar {})) = Just $ TySumF () ()
isTopLevelConstructor (UTyProd (UVar {}) (UVar {})) = Just $ TyProdF () ()
isTopLevelConstructor (UTyFun (UVar {}) (UVar {})) = Just $ TyFunF () ()
isTopLevelConstructor _ = Nothing

-- | Return an English noun phrase describing things with the given
--   top-level type constructor.
tyNounPhrase :: TypeF () -> Doc a
tyNounPhrase = \case
  TyBaseF b -> baseTyNounPhrase b
  TyVarF {} -> "a type variable"
  TyCmdF {} -> "a command"
  TyDelayF {} -> "a delayed expression"
  TySumF {} -> "a sum"
  TyProdF {} -> "a pair"
  TyFunF {} -> "a function"
  TyRcdF {} -> "a record"

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
  prettyPrec _ (TooManyTicks n) = "block could take too many ticks (" <> pretty n <> ")"
  prettyPrec _ AtomicDupingThing = "def, let, and lambda are not allowed"
  prettyPrec _ (NonSimpleVarType _ ty) = "reference to variable with non-simple type" <+> ppr ty
  prettyPrec _ NestedAtomic = "nested atomic block"
  prettyPrec _ LongConst = "commands that can take multiple ticks to execute are not allowed"

instance PrettyPrec LocatedTCFrame where
  prettyPrec p (LocatedTCFrame _ f) = prettyPrec p f

instance PrettyPrec TCFrame where
  prettyPrec _ (TCDef x) = "While checking the definition of" <+> pretty x
  prettyPrec _ TCBindL = "While checking the left-hand side of a semicolon"
  prettyPrec _ TCBindR = "While checking the right-hand side of a semicolon"
