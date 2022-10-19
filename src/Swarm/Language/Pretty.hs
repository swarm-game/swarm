{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Swarm.Language.Pretty
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Pretty-printing for the Swarm language.
module Swarm.Language.Pretty where

import Control.Lens.Combinators (pattern Empty)
import Control.Unification
import Control.Unification.IntVar
import Data.Bool (bool)
import Data.Functor.Fixedpoint (Fix, unFix)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Render.String qualified as RS
import Prettyprinter.Render.Text qualified as RT
import Swarm.Language.Capability
import Swarm.Language.Context
import Swarm.Language.Number (Number (..))
import Swarm.Language.Syntax
import Swarm.Language.Typecheck
import Swarm.Language.Types
import Witch

-- | Type class for things that can be pretty-printed, given a
--   precedence level of their context.
class PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann -- can replace with custom ann type later if desired

-- | Pretty-print a thing, with a context precedence level of zero.
ppr :: PrettyPrec a => a -> Doc ann
ppr = prettyPrec 0

-- | Pretty-print something and render it as @Text@.
prettyText :: PrettyPrec a => a -> Text
prettyText = RT.renderStrict . layoutPretty defaultLayoutOptions . ppr

-- | Pretty-print something and render it as a @String@.
prettyString :: PrettyPrec a => a -> String
prettyString = RS.renderString . layoutPretty defaultLayoutOptions . ppr

-- | Optionally surround a document with parentheses depending on the
--   @Bool@ argument.
pparens :: Bool -> Doc ann -> Doc ann
pparens True = parens
pparens False = id

instance PrettyPrec BaseTy where
  prettyPrec _ BUnit = "unit"
  prettyPrec _ BInt = "int"
  prettyPrec _ BDir = "dir"
  prettyPrec _ BText = "text"
  prettyPrec _ BBool = "bool"
  prettyPrec _ BRobot = "robot"

instance PrettyPrec IntVar where
  prettyPrec _ = pretty . mkVarName "u"

instance PrettyPrec (t (Fix t)) => PrettyPrec (Fix t) where
  prettyPrec p = prettyPrec p . unFix

instance (PrettyPrec (t (UTerm t v)), PrettyPrec v) => PrettyPrec (UTerm t v) where
  prettyPrec p (UTerm t) = prettyPrec p t
  prettyPrec p (UVar v) = prettyPrec p v

instance PrettyPrec t => PrettyPrec (TypeF t) where
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

instance PrettyPrec Polytype where
  prettyPrec _ (Forall [] t) = ppr t
  prettyPrec _ (Forall xs t) = hsep ("∀" : map pretty xs) <> "." <+> ppr t

instance PrettyPrec UPolytype where
  prettyPrec _ (Forall [] t) = ppr t
  prettyPrec _ (Forall xs t) = hsep ("∀" : map pretty xs) <> "." <+> ppr t

instance PrettyPrec t => PrettyPrec (Ctx t) where
  prettyPrec _ Empty = emptyDoc
  prettyPrec _ (assocs -> bs) = brackets (hsep (punctuate "," (map prettyBinding bs)))
   where
    prettyBinding (x, ty) = pretty x <> ":" <+> ppr ty

instance PrettyPrec Direction where
  prettyPrec _ = pretty . dirSyntax . dirInfo

instance PrettyPrec Capability where
  prettyPrec _ c = pretty $ T.toLower (from (tail $ show c))

instance PrettyPrec Const where
  prettyPrec p c = pparens (p > fixity (constInfo c)) $ pretty . syntax . constInfo $ c

instance PrettyPrec Term where
  prettyPrec _ TUnit = "()"
  prettyPrec p (TConst c) = prettyPrec p c
  prettyPrec _ (TDir d) = ppr d
  prettyPrec p (TInt n) = case n of
    Integer i -> pretty i
    NegInfinity -> prettyPrec p (TApp (TConst Neg) (TConst Infinity))
    PosInfinity -> prettyPrec p (TConst Infinity)
  prettyPrec _ (TAntiInt v) = "$int:" <> pretty v
  prettyPrec _ (TText s) = fromString (show s)
  prettyPrec _ (TAntiText v) = "$str:" <> pretty v
  prettyPrec _ (TBool b) = bool "false" "true" b
  prettyPrec _ (TRobot r) = "<r" <> pretty r <> ">"
  prettyPrec _ (TRef r) = "@" <> pretty r
  prettyPrec p (TRequireDevice d) = pparens (p > 10) $ "require" <+> ppr (TText d)
  prettyPrec p (TRequire n e) = pparens (p > 10) $ "require" <+> pretty n <+> ppr (TText e)
  prettyPrec _ (TVar s) = pretty s
  prettyPrec _ (TDelay _ t) = braces $ ppr t
  prettyPrec _ t@TPair {} = prettyTuple t
  prettyPrec _ (TLam x mty body) =
    "\\" <> pretty x <> maybe "" ((":" <>) . ppr) mty <> "." <+> ppr body
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
    hsep $
      ["let", pretty x]
        ++ maybe [] (\ty -> [":", ppr ty]) mty
        ++ ["=", ppr t1, "in", ppr t2]
  prettyPrec _ (TDef _ x mty t1) =
    hsep $
      ["def", pretty x]
        ++ maybe [] (\ty -> [":", ppr ty]) mty
        ++ ["=", ppr t1, "end"]
  prettyPrec p (TBind Nothing t1 t2) =
    pparens (p > 0) $
      prettyPrec 1 t1 <> ";" <+> prettyPrec 0 t2
  prettyPrec p (TBind (Just x) t1 t2) =
    pparens (p > 0) $
      pretty x <+> "<-" <+> prettyPrec 1 t1 <> ";" <+> prettyPrec 0 t2

prettyTuple :: Term -> Doc a
prettyTuple = pparens True . hsep . punctuate "," . map ppr . unnestTuple
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

instance PrettyPrec TypeErr where
  prettyPrec _ (Mismatch _ ty1 ty2) =
    "Can't unify" <+> ppr ty1 <+> "and" <+> ppr ty2
  prettyPrec _ (EscapedSkolem _ x) =
    "Skolem variable" <+> pretty x <+> "would escape its scope"
  prettyPrec _ (UnboundVar _ x) =
    "Unbound variable" <+> pretty x
  prettyPrec _ (Infinite x uty) =
    "Infinite type:" <+> ppr x <+> "=" <+> ppr uty
  prettyPrec _ (DefNotTopLevel _ t) =
    "Definitions may only be at the top level:" <+> ppr t
  prettyPrec _ (CantInfer _ t) =
    "Couldn't infer the type of term (this shouldn't happen; please report this as a bug!):" <+> ppr t
  prettyPrec _ (InvalidAtomic _ reason t) =
    "Invalid atomic block:" <+> ppr reason <> ":" <+> ppr t

instance PrettyPrec InvalidAtomicReason where
  prettyPrec _ (TooManyTicks n) = "block could take too many ticks (" <> pretty n <> ")"
  prettyPrec _ AtomicDupingThing = "def, let, and lambda are not allowed"
  prettyPrec _ (NonSimpleVarType _ ty) = "reference to variable with non-simple type" <+> ppr ty
  prettyPrec _ NestedAtomic = "nested atomic block"
  prettyPrec _ LongConst = "commands that can take multiple ticks to execute are not allowed"
