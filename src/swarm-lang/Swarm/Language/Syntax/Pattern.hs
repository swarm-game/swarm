{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Pattern synonyms for untyped terms
module Swarm.Language.Syntax.Pattern (
  Syntax,
  sLoc,
  sTerm,
  sType,
  sComments,
  pattern Syntax,
  pattern CSyntax,
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
  Term,
  noLoc,
) where

import Control.Lens (makeLenses, pattern Empty)
import Data.Map.Strict (Map)
import Data.Text hiding (filter, length, map)
import Swarm.Language.Syntax.AST
import Swarm.Language.Syntax.Comments
import Swarm.Language.Syntax.Loc
import Swarm.Language.Types

-- | Syntax without type annotations.
type Syntax = Syntax' ()

type Term = Term' ()

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

{-# COMPLETE TUnit, TConst, TDir, TInt, TAntiInt, TText, TAntiText, TBool, TRequireDevice, TRequire, TRequirements, TVar, TPair, TLam, TApp, TLet, TDef, TTydef, TBind, TDelay, TRcd, TProj, TAnnotate #-}
