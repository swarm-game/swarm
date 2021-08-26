module Swarm.Types where

data Type
  = TyUnit
  | TyInt
  | TyString
  | TyDir
  | TyBool
  | TyCmd Type
  | Type :->: Type
  deriving (Eq, Ord, Show)

infixr 1 :->:
