{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Term elaboration which happens after type checking.
module Swarm.Language.Elaborate where

import Control.Lens (transform, (^.))
import Swarm.Language.Module (Module (..))
import Swarm.Language.Phase
import Swarm.Language.Syntax
import Unsafe.Coerce (unsafeCoerce)

elaborateModule :: Module Typed -> Module Elaborated
elaborateModule (Module t ctx imps time prov) = Module (fmap elaborate t) ctx imps time prov

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
elaborate :: Syntax Typed -> Syntax Elaborated
elaborate = unsafeCoerce $ transform rewrite
 where
  -- unsafeCoerce is safe here because of the invariant that type
  -- families must yield equal results on 'Typed' and 'Elaborated'; the
  -- distinction between 'Typed' and 'Elaborated' is ONLY to serve as a
  -- type-level flag that we have performed elaboration.
  --
  -- The reason we need unsafeCoerce is that all the generic rewriting
  -- utilities (such as 'transform') are necessarily type-preserving.
  -- We could write our own version of 'transform' that recurses
  -- through the entire 'Syntax', applying a type-changing 'rewrite'
  -- everywhere it is applicable and reconstructing the AST everywhere
  -- else in order to change the type.  But that would be a huge pain.

  rewrite :: Syntax Typed -> Syntax Typed
  rewrite (Syntax l t cs ty) = Syntax l (rewriteTerm t) cs ty

  rewriteTerm :: Term Typed -> Term Typed
  rewriteTerm = \case
    -- Here we take inferred types for variables bound by def or
    -- bind (but NOT let) and stuff them into the term itself, so that
    -- we will still have access to them at runtime, after type
    -- annotations on the AST are erased.  We need them at runtime so
    -- we can keep track of the types of variables in scope, for use
    -- in typechecking additional terms entered at the REPL.  The
    -- reason we do not do this for 'let' is so that 'let' introduces
    -- truly local bindings which will not be available for use in
    -- later REPL terms.
    --
    -- We assume requirements for these variables have already been
    -- filled in during typechecking.  The reason we need to wait
    -- until now to fill in the types is that we have to wait until
    -- solving, substitution, and generalization are complete.
    --
    -- Eventually, once requirements analysis is part of typechecking,
    -- we'll infer them both at typechecking time then fill them in
    -- during elaboration here.
    SLet ls r x mty _ mreq t1 t2 ->
      let mpty = case ls of
            LSDef -> Just (t1 ^. sType)
            LSLet -> Nothing
       in SLet ls r x mty mpty mreq t1 t2
    SBind x (Just ty) _ mreq c1 c2 -> SBind x Nothing (Just ty) mreq c1 c2
    -- Rewrite @f $ x@ to @f x@.
    SApp (Syntax _ (SApp (Syntax _ (TConst AppF) _ _) l) _ _) r -> SApp l r
    -- Leave any other subterms alone.
    t -> t

-- | Insert a special 'suspend' primitive at the very end of an erased
--   term which must have a command type.
insertSuspend :: Term Resolved -> Term Resolved
insertSuspend t = case t of
  -- Primitive things which have type Cmd Unit: p => (p ; suspend ())
  TRequire {} -> thenSuspend
  TStock {} -> thenSuspend
  TRequirements {} -> thenSuspend
  -- Recurse through def, tydef, bind, annotate, and import.
  TLet ls r x mty mpty mreq t1 t2 -> TLet ls r x mty mpty mreq t1 (insertSuspend t2)
  TTydef x pty mtd t1 -> TTydef x pty mtd (insertSuspend t1)
  TBind mx mty mreq c1 c2 -> TBind mx mty mreq c1 (insertSuspend c2)
  TAnnotate t1 ty -> TAnnotate (insertSuspend t1) ty
  TImportIn loc t1 -> TImportIn loc (insertSuspend t1)
  -- Replace pure or noop with suspend
  TApp (TConst Pure) t1 -> TSuspend t1
  TConst Noop -> TSuspend TUnit
  -- Anything else: p => (__res__ <- p; suspend __res__)
  --
  -- Note that since we don't put type + reqs annotations on
  -- __res__, it won't get added to the type environment and hence
  -- won't be in scope for use after the suspend.  But we pick a
  -- weird unlikely-to-be-used name just to be safe.
  t' -> TBind (Just "__res__") Nothing Nothing t' (TSuspend (TVar "__res__"))
 where
  thenSuspend = TBind Nothing Nothing Nothing t (TSuspend TUnit)
