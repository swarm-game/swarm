{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Term elaboration which happens after type checking.
module Swarm.Language.Elaborate where

import Control.Lens (transform, (%~), (^.))
import Swarm.Language.Syntax
import Swarm.Language.Types

-- | Perform some elaboration / rewriting on a fully type-annotated
--   term.  This currently performs such operations as rewriting @if@
--   expressions and recursive let expressions to use laziness
--   appropriately.  In theory it could also perform rewriting for
--   overloaded constants depending on the actual type they are used
--   at, but currently that sort of thing tends to make type inference
--   fall over.
elaborate :: Syntax' Polytype -> Syntax' Polytype
elaborate =
  -- Wrap all *free* variables in 'Force'.  Free variables must be
  -- referring to a previous definition, which are all wrapped in
  -- 'TDelay'.
  (freeVarsS %~ \s -> Syntax' (s ^. sLoc) (SApp sForce s) (s ^. sType))
    -- Now do additional rewriting on all subterms.
    . transform rewrite
 where
  rewrite :: Syntax' Polytype -> Syntax' Polytype
  rewrite (Syntax' l t ty) = Syntax' l (rewriteTerm t) ty

  rewriteTerm :: Term' Polytype -> Term' Polytype
  rewriteTerm = \case
    -- For recursive let bindings, rewrite any occurrences of x to
    -- (force x).  When interpreting t1, we will put a binding (x |->
    -- delay t1) in the context.
    SLet True x ty t1 t2 -> SLet True x ty (wrapForce (lvVar x) t1) (wrapForce (lvVar x) t2)
    -- Rewrite any recursive occurrences of x inside t1 to (force x).
    -- When a TDef is encountered at runtime its body will immediately
    -- be wrapped in a VDelay. However, to make this work we also need
    -- to wrap all free variables in any term with 'force' --- since
    -- any such variables must in fact refer to things previously
    -- bound by 'def'.
    SDef True x ty t1 -> SDef True x ty (wrapForce (lvVar x) t1)
    -- Rewrite @f $ x@ to @f x@.
    SApp (Syntax' _ (SApp (Syntax' _ (TConst AppF) _) l) _) r -> SApp l r
    -- Leave any other subterms alone.
    t -> t

wrapForce :: Var -> Syntax' Polytype -> Syntax' Polytype
wrapForce x = mapFreeS x (\s@(Syntax' l _ ty) -> Syntax' l (SApp sForce s) ty)

-- Note, TyUnit is not the right type, but I don't want to bother

sForce :: Syntax' Polytype
sForce = Syntax' NoLoc (TConst Force) (Forall ["a"] (TyDelay (TyVar "a") :->: TyVar "a"))
