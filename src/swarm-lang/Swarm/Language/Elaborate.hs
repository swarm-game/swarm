{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Term elaboration which happens after type checking.
module Swarm.Language.Elaborate where

import Control.Applicative ((<|>))
import Control.Lens (transform, (%~), (^.), pattern Empty)
import Swarm.Language.Syntax
import Swarm.Language.Types

-- | Perform some elaboration / rewriting on a fully type-annotated
--   term.  This currently performs such operations as rewriting @if@
--   expressions and recursive let expressions to use laziness
--   appropriately.  It also inserts types and requirements on bound
--   variables so they will be available at runtime.
--
--   In theory it could also perform rewriting for overloaded
--   constants depending on the actual type they are used at, but
--   currently that sort of thing tends to make type inference fall
--   over.
elaborate :: TSyntax -> TSyntax
elaborate =
  -- XXX Maybe pass the current environment into elaborate, so we can tell
  -- _which_ free variables need to be wrapped in Force?

  -- Wrap all *free* variables in 'Force'.  Free variables must be
  -- referring to a previous definition, which are all wrapped in
  -- 'TDelay'.
  (freeVarsS %~ \s -> Syntax' (s ^. sLoc) (SApp sForce s) (s ^. sComments) (s ^. sType))
    -- Now do additional rewriting on all subterms.
    . transform rewrite
 where
  rewrite :: TSyntax -> TSyntax
  rewrite (Syntax' l t cs ty) = Syntax' l (rewriteTerm t) cs ty

  rewriteTerm :: TTerm -> TTerm
  rewriteTerm = \case
    -- For recursive let bindings, rewrite any occurrences of x to
    -- (force x).  When interpreting t1, we will put a binding (x |->
    -- delay t1) in the context.
    --
    -- Here we also take inferred types for variables bound by let or
    -- bind and stuff them into the term itself, so that we will still
    -- have access to them at runtime, after type annotations on the
    -- AST are erased.  We need them at runtime so we can keep track
    -- of the types of variables in scope, for use in typechecking
    -- additional terms entered at the REPL.
    --
    -- We assume requirements for these variables have already been
    -- filled in during typechecking.  The reason we need to wait
    -- until now to fill in the types is that we have to wait until
    -- solving, substitution, and generalization are complete.
    --
    -- Eventually, once requirements analysis is part of typechecking,
    -- we'll infer them both at typechecking time then fill them in
    -- during elaboration here.
    SLet ls r x mty mreq t1 t2 ->
      let wrap
            | r = wrapForce (lvVar x) -- wrap in 'force' if recursive
            | otherwise = id
       in SLet ls r x (mty <|> Just (t1 ^. sType)) mreq (wrap t1) (wrap t2)
    SBind x (Just ty) _ mreq c1 c2 -> SBind x Nothing (Just ty) mreq c1 c2
    -- Rewrite @f $ x@ to @f x@.
    SApp (Syntax' _ (SApp (Syntax' _ (TConst AppF) _ _) l) _ _) r -> SApp l r
    -- Leave any other subterms alone.
    t -> t

wrapForce :: Var -> TSyntax -> TSyntax
wrapForce x = mapFreeS x (\s@(Syntax' l _ ty cs) -> Syntax' l (SApp sForce s) ty cs)

-- Note, TyUnit is not the right type, but I don't want to bother

sForce :: TSyntax
sForce = Syntax' NoLoc (TConst Force) Empty (Forall ["a"] (TyDelay (TyVar "a") :->: TyVar "a"))

-- | Insert a special 'suspend' primitive at the very end of an erased
--   term which must have a command type.
insertSuspend :: Term -> Term
insertSuspend t = case t of
  -- Primitive things which have type Cmd Unit: p => (p ; suspend ())
  TRequireDevice {} -> thenSuspend
  TRequire {} -> thenSuspend
  TRequirements {} -> thenSuspend
  -- Recurse through let, bind, and annotate
  TLet ls r x mty mreq t1 t2 -> TLet ls r x mty mreq t1 (insertSuspend t2)
  TBind mx mty mreq c1 c2 -> TBind mx mty mreq c1 (insertSuspend c2)
  TAnnotate t1 ty -> TAnnotate (insertSuspend t1) ty
  -- Replace return with suspend
  TApp (TConst Return) t1 -> TSuspend t1
  -- Anything else: p => (__res__ <- p; suspend __res__)
  --
  -- Note that since we don't put type + reqs annotations on
  -- __res__, it won't get added to the type environment and hence
  -- won't be in scope for use after the suspend.  But we pick a
  -- weird unlikely-to-be-used name just to be safe.
  t' -> TBind (Just "__res__") Nothing Nothing t' (TSuspend (TVar "__res__"))
 where
  thenSuspend = TBind Nothing Nothing Nothing t (TSuspend TUnit)
