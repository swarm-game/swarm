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
{-# LANGUAGE TypeSynonymInstances #-}

module Swarm.Language.Pretty where

import           Data.Bool                   (bool)
import           Data.String                 (fromString)
import           Data.Text                   (Text)
import           Prettyprinter
import qualified Prettyprinter.Render.String as RS
import qualified Prettyprinter.Render.Text   as RT

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

instance PrettyPrec Type where
  prettyPrec _ TyUnit         = "()"
  prettyPrec _ TyInt          = "int"
  prettyPrec _ TyDir          = "dir"
  prettyPrec _ TyString       = "string"
  prettyPrec _ TyBool         = "bool"
  prettyPrec p (ty1 :*: ty2)  = pparens (p > 2) $
    prettyPrec 3 ty1 <+> "*" <+> prettyPrec 2 ty2
  prettyPrec p (TyCmd ty)     = pparens (p > 9) $ "cmd" <+> prettyPrec 10 ty
  prettyPrec p (ty1 :->: ty2) = pparens (p > 0) $
    prettyPrec 1 ty1 <+> "->" <+> prettyPrec 0 ty2

instance PrettyPrec Direction where
  prettyPrec _ Lft   = "left"
  prettyPrec _ Rgt   = "right"
  prettyPrec _ Back  = "back"
  prettyPrec _ Fwd   = "forward"
  prettyPrec _ North = "north"
  prettyPrec _ South = "south"
  prettyPrec _ East  = "east"
  prettyPrec _ West  = "west"

instance PrettyPrec Const where
  prettyPrec _ Wait      = "wait"
  prettyPrec _ Halt      = "halt"
  prettyPrec _ Return    = "return"
  prettyPrec _ Noop      = "{}"
  prettyPrec _ Move      = "move"
  prettyPrec _ Turn      = "turn"
  prettyPrec _ Grab      = "grab"
  prettyPrec _ Place     = "place"
  prettyPrec _ Give      = "give"
  prettyPrec _ Build     = "build"
  prettyPrec _ Run       = "run"
  prettyPrec _ GetX      = "getX"
  prettyPrec _ GetY      = "getY"
  prettyPrec _ Random    = "random"
  prettyPrec _ Say       = "say"
  prettyPrec _ View      = "view"
  prettyPrec _ Appear    = "appear"
  prettyPrec _ If        = "if"
  prettyPrec _ Fst       = "fst"
  prettyPrec _ Snd       = "snd"
  prettyPrec _ Force     = "force"
  prettyPrec p (Cmp c)   = prettyPrec p c
  prettyPrec p (Arith c) = prettyPrec p c

instance PrettyPrec CmpConst where
  prettyPrec _ CmpEq  = "=="
  prettyPrec _ CmpNeq = "/="
  prettyPrec _ CmpLt  = "<"
  prettyPrec _ CmpGt  = ">"
  prettyPrec _ CmpLeq = "<="
  prettyPrec _ CmpGeq = ">="

instance PrettyPrec ArithConst where
  prettyPrec _ Neg = "-"
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
  prettyPrec p (TApp _ t1 t2)  = pparens (p > 10) $
    prettyPrec 10 t1 <+> prettyPrec 11 t2
  prettyPrec _ (TLet x mty t1 t2) =
    sep $
      ["let", pretty x] ++
      maybe [] (\ty -> [":", ppr ty]) mty ++
      ["=", ppr t1, "in", ppr t2]
  prettyPrec p (TBind Nothing _ t1 t2) = pparens (p > 0) $
    prettyPrec 1 t1 <> ";" <+> prettyPrec 0 t2
  prettyPrec p (TBind (Just x) _ t1 t2) = pparens (p > 0) $
    pretty x <+> "<-" <+> prettyPrec 1 t1  <> ";" <+> prettyPrec 0 t2

instance PrettyPrec ATerm where
  prettyPrec p = prettyPrec p . mapTerm' (\(ID x) -> Just x)

instance PrettyPrec UTerm where
  prettyPrec p = prettyPrec p . mapTerm' (\NONE -> Nothing)

instance PrettyPrec TypeErr where
  prettyPrec _ (NotFunTy t ty) =
    sep
    [ "Expecting a function type, but"
    , ppr t
    , "has type"
    , ppr ty
    , "instead."
    ]
  prettyPrec _ (NotPairTy t ty) =
    sep
    [ "Expecting a pair type, but"
    , ppr t
    , "has type"
    , ppr ty
    , "instead."
    ]
  prettyPrec _ (NotCmdTy t ty) =
    sep
    [ "Expecting a command type, but"
    , ppr t
    , "has type"
    , ppr ty
    , "instead."
    ]
  prettyPrec _ (NonCmdTyExpected t ty) =
    sep
    [ "Expecting type", ppr ty
    , "but", ppr t, "is a command."
    ]
  prettyPrec _ (NonPairTyExpected t ty) =
    sep
    [ "Expecting type", ppr ty
    , "but", ppr t, "is a pair."
    ]
  prettyPrec _ (Mismatch t expected inferred) =
    vsep
      [ "Type mismatch when checking expression" <+> squotes (ppr t)
      , "Expected type:" <+> ppr expected
      , "Actual type:" <+> ppr inferred
      ]
  prettyPrec _ (UnboundVar x) =
    "Unbound variable" <+> pretty x
  prettyPrec _ (CantInfer t) =
    "Can't infer the type of" <+> ppr t
