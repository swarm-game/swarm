-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
import Data.Bool (bool)
import Data.Functor.Fixedpoint (Fix, unFix)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import qualified Prettyprinter.Render.String as RS
import qualified Prettyprinter.Render.Text as RT
import Witch

import Control.Unification
import Control.Unification.IntVar

import Swarm.Language.Capability
import Swarm.Language.Context
import Swarm.Language.Syntax
import Swarm.Language.Typecheck
import Swarm.Language.Types

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
  prettyPrec _ BUnit = "()"
  prettyPrec _ BInt = "int"
  prettyPrec _ BDir = "dir"
  prettyPrec _ BString = "string"
  prettyPrec _ BBool = "bool"

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
  prettyPrec p (TyProdF ty1 ty2) =
    pparens (p > 2) $
      prettyPrec 3 ty1 <+> "*" <+> prettyPrec 2 ty2
  prettyPrec p (TyCmdF ty) = pparens (p > 9) $ "cmd" <+> prettyPrec 10 ty
  prettyPrec p (TyFunF ty1 ty2) =
    pparens (p > 0) $
      prettyPrec 1 ty1 <+> "->" <+> prettyPrec 0 ty2

instance PrettyPrec Polytype where
  prettyPrec _ (Forall [] t) = ppr t
  prettyPrec _ (Forall xs t) = hsep ("∀" : map pretty xs) <> "." <+> ppr t

instance PrettyPrec t => PrettyPrec (Ctx t) where
  prettyPrec _ Empty = emptyDoc
  prettyPrec _ (assocs -> bs) = brackets (hsep (punctuate "," (map prettyBinding bs)))
   where
    prettyBinding (x, ty) = pretty x <> ":" <+> ppr ty

instance PrettyPrec Direction where
  prettyPrec _ Lft = "left"
  prettyPrec _ Rgt = "right"
  prettyPrec _ Back = "back"
  prettyPrec _ Fwd = "forward"
  prettyPrec _ North = "north"
  prettyPrec _ South = "south"
  prettyPrec _ East = "east"
  prettyPrec _ West = "west"
  prettyPrec _ Down = "down"

instance PrettyPrec Capability where
  prettyPrec _ c = pretty $ T.toLower (from (tail $ show c))

instance PrettyPrec Const where
  prettyPrec p c = pparens (p > fixity (constInfo c)) $ pretty . syntax . constInfo $ c

instance PrettyPrec Term where
  prettyPrec _ TUnit = "()"
  prettyPrec p (TConst c) = prettyPrec p c
  prettyPrec _ (TDir d) = ppr d
  prettyPrec _ (TInt n) = pretty n
  prettyPrec _ (TAntiInt v) = "$int:" <> pretty v
  prettyPrec _ (TString s) = fromString (show s)
  prettyPrec _ (TAntiString v) = "$str:" <> pretty v
  prettyPrec _ (TBool b) = bool "false" "true" b
  prettyPrec _ (TVar s) = pretty s
  prettyPrec p (TDelay t) = pparens (p > 10) $ "delay" <+> prettyPrec 11 t
  prettyPrec _ (SPair t1 t2) = pparens True $ ppr t1 <> "," <+> ppr t2
  prettyPrec _ (SLam x mty body) =
    "\\" <> pretty x <> maybe "" ((":" <>) . ppr) mty <> "." <+> ppr body
  -- Special handling of infix operators - ((+) 2) 3 --> 2 + 3
  prettyPrec p (SApp t@(SApp (TConst c) l) r) =
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
  prettyPrec p (SApp t1 t2) = case t1 of
    TConst c ->
      let ci = constInfo c
          pC = fixity ci
       in case constMeta ci of
            ConstMUnOp P -> pparens (p > pC) $ ppr t1 <> prettyPrec (succ pC) t2
            ConstMUnOp S -> pparens (p > pC) $ prettyPrec (succ pC) t2 <> ppr t1
            _ -> prettyPrecApp p t1 t2
    _ -> prettyPrecApp p t1 t2
  prettyPrec _ (SLet x mty t1 t2) =
    hsep $
      ["let", pretty x]
        ++ maybe [] (\ty -> [":", ppr ty]) mty
        ++ ["=", ppr t1, "in", ppr t2]
  prettyPrec _ (SDef x mty t1) =
    hsep $
      ["def", pretty x]
        ++ maybe [] (\ty -> [":", ppr ty]) mty
        ++ ["=", ppr t1, "end"]
  prettyPrec p (SBind Nothing t1 t2) =
    pparens (p > 0) $
      prettyPrec 1 t1 <> ";" <+> prettyPrec 0 t2
  prettyPrec p (SBind (Just x) t1 t2) =
    pparens (p > 0) $
      pretty x <+> "<-" <+> prettyPrec 1 t1 <> ";" <+> prettyPrec 0 t2

prettyPrecApp :: Int -> Term -> Term -> Doc a
prettyPrecApp p t1 t2 =
  pparens (p > 10) $
    prettyPrec 10 t1 <+> prettyPrec 11 t2

appliedTermPrec :: Term -> Int
appliedTermPrec (SApp f _) = case f of
  TConst c -> fixity $ constInfo c
  _ -> appliedTermPrec f
appliedTermPrec _ = 10

instance PrettyPrec TypeErr where
  prettyPrec _ (Mismatch _ ty1 ty2) =
    "Can't unify" <+> ppr ty1 <+> "and" <+> ppr ty2
  prettyPrec _ (UnboundVar _ x) =
    "Unbound variable" <+> pretty x
  prettyPrec _ (Infinite x uty) =
    "Infinite type:" <+> ppr x <+> "=" <+> ppr uty
  prettyPrec _ (DefNotTopLevel _ t) =
    "Definitions may only be at the top level:" <+> ppr t
