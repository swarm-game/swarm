module Swarm.Types where

data Type
  = TyUnit
  | TyInt
  | TyString
  | TyDir
  | TyCmd Type
  | Type :->: Type
  deriving (Eq, Ord, Show)

infixr 1 :->:
