module Swarm.AST where

import           Data.Text

data Direction
  = Lt
  | Rt
  | Back
  | Fwd
  | North
  | South
  | East
  | West
  deriving (Eq, Ord, Show, Read)

data Term
  = TConst Const
--  | TVar Text
  | TDir Direction
  | TInt Integer
--  | TString Text
--  | TLam Text Type Expr
  | TApp Expr Expr
  | TBind Expr Expr

data Const
  = Wait
  | Move
  | Turn
  | Harvest
  | Repeat
  | Build
