-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Pretty
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Pretty-printing for the Swarm language.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Swarm.Language.Pretty where

import           Control.Lens.Combinators    (pattern Empty)
import           Data.Bool                   (bool)
import           Data.Functor.Fixedpoint     (Fix, unFix)
import           Data.String                 (fromString)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Prettyprinter
import qualified Prettyprinter.Render.String as RS
import qualified Prettyprinter.Render.Text   as RT
import           Witch

import           Control.Unification
import           Control.Unification.IntVar

import           Swarm.Language.Capability
import           Swarm.Language.Context
import           Swarm.Language.Syntax
import           Swarm.Language.Typecheck
import           Swarm.Language.Types


-- | Type class for things that can be pretty-printed, given a
--   precedence level of their context.
class PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann   -- can replace with custom ann type later if desired

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
pparens True  = parens
pparens False = id

instance PrettyPrec BaseTy where
  prettyPrec _ BUnit   = "()"
  prettyPrec _ BInt    = "int"
  prettyPrec _ BDir    = "dir"
  prettyPrec _ BString = "string"
  prettyPrec _ BBool   = "bool"

instance PrettyPrec IntVar where
  prettyPrec _ = pretty . mkVarName "u"

instance PrettyPrec (t (Fix t)) => PrettyPrec (Fix t) where
  prettyPrec p = prettyPrec p . unFix

instance (PrettyPrec (t (UTerm t v)), PrettyPrec v) => PrettyPrec (UTerm t v) where
  prettyPrec p (UTerm t) = prettyPrec p t
  prettyPrec p (UVar v)  = prettyPrec p v

instance PrettyPrec t => PrettyPrec (TypeF t) where
  prettyPrec _ (TyBaseF b)     = ppr b
  prettyPrec _ (TyVarF v)      = pretty v
  prettyPrec p (TyProdF ty1 ty2)  = pparens (p > 2) $
    prettyPrec 3 ty1 <+> "*" <+> prettyPrec 2 ty2
  prettyPrec p (TyCmdF ty) = pparens (p > 9) $ "cmd" <+> prettyPrec 10 ty
  prettyPrec p (TyFunF ty1 ty2)    = pparens (p > 0) $
    prettyPrec 1 ty1 <+> "->" <+> prettyPrec 0 ty2

instance PrettyPrec Polytype where
  prettyPrec _ (Forall [] t) = ppr t
  prettyPrec _ (Forall xs t) = hsep ("forall" : map pretty xs) <> "." <+> ppr t

instance PrettyPrec t => PrettyPrec (Ctx t) where
  prettyPrec _ Empty          = emptyDoc
  prettyPrec _ (assocs -> bs) = brackets (hsep (punctuate "," (map prettyBinding bs)))
    where
      prettyBinding (x,ty) = pretty x <> ":" <+> ppr ty

instance PrettyPrec Direction where
  prettyPrec _ Lft   = "left"
  prettyPrec _ Rgt   = "right"
  prettyPrec _ Back  = "back"
  prettyPrec _ Fwd   = "forward"
  prettyPrec _ North = "north"
  prettyPrec _ South = "south"
  prettyPrec _ East  = "east"
  prettyPrec _ West  = "west"

instance PrettyPrec Capability where
  prettyPrec _ c = pretty $ T.toLower (from (tail $ show c))

instance PrettyPrec Const where
  prettyPrec _ Noop      = "{}"
  prettyPrec p (Cmp c)   = prettyPrec p c
  prettyPrec p (Arith c) = prettyPrec p c
  prettyPrec _ Neg       = "-"
  prettyPrec _ c         = pretty $ T.toLower (from (show c))

instance PrettyPrec CmpConst where
  prettyPrec _ CmpEq  = "=="
  prettyPrec _ CmpNeq = "/="
  prettyPrec _ CmpLt  = "<"
  prettyPrec _ CmpGt  = ">"
  prettyPrec _ CmpLeq = "<="
  prettyPrec _ CmpGeq = ">="

instance PrettyPrec ArithConst where
  prettyPrec _ Add = "+"
  prettyPrec _ Sub = "-"
  prettyPrec _ Mul = "*"
  prettyPrec _ Div = "/"
  prettyPrec _ Exp = "^"

instance PrettyPrec Term where
  prettyPrec _ TUnit         = "()"
  prettyPrec _ (TConst c)    = ppr c
  prettyPrec _ (TDir d)      = ppr d
  prettyPrec _ (TInt n)      = pretty n
  prettyPrec _ (TString s)   = fromString (show s)
  prettyPrec _ (TBool b)     = bool "false" "true" b
  prettyPrec _ (TVar s)      = pretty s
  prettyPrec p (TDelay t)    = pparens (p > 10) $ "delay" <+> prettyPrec 11 t
  prettyPrec _ (TPair t1 t2) = pparens True $ ppr t1 <> "," <+> ppr t2
  prettyPrec _ (TLam x mty body) =
    "\\" <> pretty x <> maybe "" ((":" <>) . ppr) mty <> "." <+> ppr body
  prettyPrec p (TApp t1 t2)  = pparens (p > 10) $
    prettyPrec 10 t1 <+> prettyPrec 11 t2
  prettyPrec _ (TLet x mty t1 t2) =
    hsep $
      ["let", pretty x] ++
      maybe [] (\ty -> [":", ppr ty]) mty ++
      ["=", ppr t1, "in", ppr t2]
  prettyPrec _ (TDef x mty t1) =
    hsep $
      ["def", pretty x] ++
      maybe [] (\ty -> [":", ppr ty]) mty ++
      ["=", ppr t1, "end"]
  prettyPrec p (TBind Nothing t1 t2) = pparens (p > 0) $
    prettyPrec 1 t1 <> ";" <+> prettyPrec 0 t2
  prettyPrec p (TBind (Just x) t1 t2) = pparens (p > 0) $
    pretty x <+> "<-" <+> prettyPrec 1 t1  <> ";" <+> prettyPrec 0 t2

instance PrettyPrec TypeErr where
  prettyPrec _ (Mismatch ty1 ty2) =
    "Can't unify" <+> ppr ty1 <+> "and" <+> ppr ty2

  prettyPrec _ (UnboundVar x) =
    "Unbound variable" <+> pretty x

  prettyPrec _ (Infinite x uty) =
    "Infinite type:" <+> ppr x <+> "=" <+> ppr uty

  prettyPrec _ (DefNotTopLevel t) =
    "Definitions may only be at the top level:" <+> ppr t
