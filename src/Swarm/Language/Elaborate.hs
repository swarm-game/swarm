-- |
-- Module      :  Swarm.Language.Elaborate
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
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
  (fvT %~ \s -> Syntax' (s ^. sLoc) (SApp (TConst Force) s) (s ^. sType))
    -- Now do additional rewriting on all subterms.
    . transform rewrite
 where
  rewrite :: Syntax' Polytype -> Syntax' Polytype
  rewrite s@(Syntax' l t ty) = Syntax' l (rewriteTerm t) ty

  rewriteTerm :: Term' Polytype -> Term' Polytype
  -- For recursive let bindings, rewrite any occurrences of x to
  -- (force x).  When interpreting t1, we will put a binding (x |->
  -- delay t1) in the context.
  rewriteTerm (TLet True x ty t1 t2) = TLet True x ty (wrapForce x t1) (wrapForce x t2)
  -- Rewrite any recursive occurrences of x inside t1 to (force x).
  -- When a TDef is encountered at runtime its body will immediately
  -- be wrapped in a VDelay. However, to make this work we also need
  -- to wrap all free variables in any term with 'force' --- since
  -- any such variables must in fact refer to things previously
  -- bound by 'def'.
  rewriteTerm (TDef True x ty t1) = TDef True x ty (mapFree1 x (TApp (TConst Force)) t1)
  -- Rewrite @f $ x@ to @f x@.
  rewriteTerm (TApp (TApp (TConst AppF) r) l) = TApp r l
  -- Leave any other subterms alone.
  rewriteTerm t = t

wrapForce :: Var -> Syntax' Polytype -> Syntax' Polytype
wrapForce x = mapFree1 x (\s@(Syntax' l _ ty) -> Syntax' l (SApp (Syntax' mempty (TConst Force) _) s) ty) -- XXX

-- (TApp (TConst Force))
