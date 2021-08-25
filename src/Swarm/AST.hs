module Swarm.AST where

import           Data.Text

import           Swarm.Types

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
  | TDir Direction
  | TInt Integer
  | TString Text
  | TVar Text
  | TLam Text (Maybe Type) Term
  | TApp Term Term
  | TBind Term Term
  | TNop
  deriving (Eq, Ord, Show)

-- | Built-in function and command constants.
data Const
  = Wait
  | Move
  | Turn
  | Harvest
  | Repeat    -- XXX get rid of repeat, encode it as a function within the language?
  | Build
  | Run
  deriving (Eq, Ord, Show)

-- | The arity of a constant.
constArity :: Const -> Int
constArity Wait    = 0
constArity Move    = 0
constArity Turn    = 1
constArity Harvest = 0
constArity Repeat  = 2
constArity Build   = 1
constArity Run     = 1
