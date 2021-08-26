{-# LANGUAGE OverloadedStrings #-}

module Swarm.Elaborate where

import           Swarm.AST
import           Swarm.Types

elaborate :: Type -> ATerm -> ATerm
elaborate = bottomUp rewrite
  where
    -- if cond thn els ---> force (if cond (delay thn) (delay els))
    rewrite _ (TApp resTy (TApp _ (TApp condTy (TConst If) cond) thn) els)
      = TApp resTy (TConst Force) (TApp resTy (TApp resTy (TApp condTy (TConst If) cond) (TDelay thn)) (TDelay els))

    -- Note, the below rule isn't actually used at the moment, but
    -- leaving it here just to illustrate the idea.  The idea was to
    -- have build be overloaded, and rewrite it based on the type.
    -- That part actually worked great; the problem is that having the
    -- type of build not be inferrable was a problem for type
    -- inference.

    -- build : string -> cmd ()  --->  \s.str. build(run(s))
    rewrite (TyString :->: _) (TConst Build)
      = TLam "s" (ID TyString)
          (TApp (ID (TyCmd TyUnit)) (TConst Build)
            (TApp (ID TyString) (TConst Run) (TVar "s")))

    rewrite _ t = t
