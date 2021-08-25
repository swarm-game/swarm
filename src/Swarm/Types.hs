module Swarm.Types where

data Type
  = TyUnit
  | TyInt
  | TyString
  | TyDir
  | TyCmd                   -- Later this may have a result type attached
  | Type :->: Type
  deriving (Eq, Ord, Show)

infixr 1 :->:
