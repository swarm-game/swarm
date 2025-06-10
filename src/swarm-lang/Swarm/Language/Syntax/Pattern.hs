{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Pattern synonyms for untyped terms
module Swarm.Language.Syntax.Pattern (
  sLoc,
  sTerm,
  sType,
  sComments,
  pattern RSyntax,
  pattern CSyntax,
  pattern RTerm,
  pattern TRequirements,
  pattern TPair,
  pattern TLam,
  pattern TApp,
  pattern (:$:),
  pattern TLet,
  pattern TTydef,
  pattern TBind,
  pattern TDelay,
  pattern TRcd,
  pattern TProj,
  pattern TAnnotate,
  pattern TSuspend,
  pattern TImportIn,
  pattern TParens,
  noLoc,
) where

import Control.Lens (makeLenses, pattern Empty)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Swarm.Language.Phase
import Swarm.Language.Requirements.Type (Requirements)
import Swarm.Language.Syntax.AST
import Swarm.Language.Syntax.Comments
import Swarm.Language.Syntax.Import (ImportLoc)
import Swarm.Language.TDVar
import Swarm.Language.Types
import Swarm.Util.SrcLoc

makeLenses 'Syntax

-- | Raw parsed syntax, without comments or type annotations.
pattern RSyntax :: SrcLoc -> Term Raw -> Syntax Raw
pattern RSyntax l t <- Syntax l t _ ()
  where
    RSyntax l t = Syntax l t Empty ()

{-# COMPLETE RSyntax #-}

-- | Untyped syntax with assocated comments.
pattern CSyntax :: SrcLoc -> Term Raw -> Comments -> Syntax Raw
pattern CSyntax l t cs = Syntax l t cs ()

{-# COMPLETE CSyntax #-}

noLoc :: Term Raw -> Syntax Raw
noLoc = RSyntax mempty

-- | Match an untyped term without annotations.
pattern RTerm :: Term Raw -> Syntax Raw
pattern RTerm t <-
  CSyntax _ t _
  where
    RTerm t = RSyntax mempty t

pattern TRequirements :: Text -> Term Raw -> Term Raw
pattern TRequirements x t = SRequirements x (RTerm t)

-- | Match a TPair without annotations.
pattern TPair :: Term Raw -> Term Raw -> Term Raw
pattern TPair t1 t2 = SPair (RTerm t1) (RTerm t2)

-- | Match a TLam without annotations.
pattern TLam :: Var -> Maybe Type -> Term Raw -> Term Raw
pattern TLam v ty t <- SLam (lvVar -> v) ty (RTerm t)
  where
    TLam v ty t = SLam (LV NoLoc v) ty (RTerm t)

-- | Match a TApp without annotations.
pattern TApp :: Term Raw -> Term Raw -> Term Raw
pattern TApp t1 t2 = SApp (RTerm t1) (RTerm t2)

infixl 0 :$:

-- | Convenient infix pattern synonym for application.
pattern (:$:) :: Term Raw -> Syntax Raw -> Term Raw
pattern (:$:) t1 s2 = SApp (RTerm t1) s2

-- | Match a TLet without annotations.
pattern TLet :: LetSyntax -> Bool -> Var -> Maybe RawPolytype -> Maybe Polytype -> Maybe Requirements -> Term Raw -> Term Raw -> Term Raw
pattern TLet ls r v mty mpty mreq t1 t2 <- SLet ls r (lvVar -> v) mty mpty mreq (RTerm t1) (RTerm t2)
  where
    TLet ls r v mty mpty mreq t1 t2 = SLet ls r (LV NoLoc v) mty mpty mreq (RTerm t1) (RTerm t2)

-- | Match a STydef without annotations.
pattern TTydef :: TDVar -> Polytype -> Maybe TydefInfo -> Term Raw -> Term Raw
pattern TTydef v ty mtd t1 <- STydef (lvVar -> v) ty mtd (RTerm t1)
  where
    TTydef v ty mtd t1 = STydef (LV NoLoc v) ty mtd (RTerm t1)

-- | Match a TBind without annotations.
pattern TBind :: Maybe Var -> Maybe Polytype -> Maybe Requirements -> Term Raw -> Term Raw -> Term Raw
pattern TBind mv mty mreq t1 t2 <- SBind (fmap lvVar -> mv) _ mty mreq (RTerm t1) (RTerm t2)
  where
    TBind mv mty mreq t1 t2 = SBind (LV NoLoc <$> mv) Nothing mty mreq (RTerm t1) (RTerm t2)

-- | Match a TDelay without annotations.
pattern TDelay :: Term Raw -> Term Raw
pattern TDelay t = SDelay (RTerm t)

-- | Match a TRcd without annotations.
pattern TRcd :: Map Var (Maybe (Term Raw)) -> Term Raw
pattern TRcd m <- SRcd ((fmap . fmap) _sTerm -> m)
  where
    TRcd m = SRcd ((fmap . fmap) RTerm m)

pattern TProj :: Term Raw -> Var -> Term Raw
pattern TProj t x = SProj (RTerm t) x

-- | Match a TAnnotate without annotations.
pattern TAnnotate :: Term Raw -> RawPolytype -> Term Raw
pattern TAnnotate t pt = SAnnotate (RTerm t) pt

-- | Match a TSuspend without annotations.
pattern TSuspend :: Term Raw -> Term Raw
pattern TSuspend t = SSuspend (RTerm t)

-- | Match a TImportIn without annotations.
pattern TImportIn :: ImportLoc -> Term Raw -> Term Raw
pattern TImportIn loc t = SImportIn loc (RTerm t)

-- | Match a TParens without annotations.
pattern TParens :: Term Raw -> Term Raw
pattern TParens t = SParens (RTerm t)

-- COMPLETE pragma tells GHC using this set of patterns is complete for Term Raw

{-# COMPLETE TUnit, TConst, TDir, TInt, TAntiInt, TText, TAntiText, TBool, TRequire, TStock, TRequirements, TVar, TPair, TLam, TApp, TLet, TTydef, TBind, TDelay, TRcd, TProj, TAnnotate, TSuspend, TImportIn, TParens #-}
