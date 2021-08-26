module Swarm.Elaborate where

import           Swarm.AST
import           Swarm.Types

elaborate :: Type -> ATerm -> ATerm
elaborate = bottomUp rewrite
  where
    rewrite _ (TApp resTy (TApp _ (TApp condTy (TConst If) cond) thn) els)
      = TApp resTy (TConst Force) (TApp resTy (TApp resTy (TApp condTy (TConst If) cond) (TDelay thn)) (TDelay els))
    rewrite _ t = t
