{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Unify where

import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.State
import Data.Equivalence.Monad
import Data.Map (Map)
import Data.Map qualified as M

type Var = String

data TyF t = TyBool | TyInt | t :->: t | t :*: t
  deriving (Eq, Ord, Show)

match :: TyF t -> TyF t -> Maybe (TyF (t, t))
match = undefined

type Ty = Free TyF Var

-- Use EquivT from
-- https://hackage.haskell.org/package/equivalence-0.4.1/docs/Data-Equivalence-Monad.html
-- to maintain equivalence classes of variables.

-- Equivalence class descriptors are intended to be a monoid.  Need to
-- keep track of a separate mapping from variable equivalence classes
-- to types.  How to make sure it stays up-to-date with a canonical
-- label for each class?
--
-- Just use variable names as descriptors, with First monoid.  Every time
-- we unify two variables...

data UnifyError = Occurs | NotUnify

(=:=) :: Ty -> Ty -> StateT (Map ClassID Ty) (ExceptT UnifyError (EquivM s Var Var)) Ty
(=:=) = \cases
  (Pure x) (Pure y) -> do
    
  (Pure x) ty -> 
  ty (Pure y) -> undefined
  (Free f) (Free g) -> undefined
