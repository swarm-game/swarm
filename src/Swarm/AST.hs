module Swarm.AST where

data Direction
  = Lt
  | Rt
  | Around
  | North
  | South
  | East
  | West
  deriving (Eq, Ord, Show, Read)

data Command
  = Wait
  | Move
  | Turn Direction
  | Harvest
  | Block Program
  | Repeat Integer Command
  | Build Command
  deriving (Eq, Ord, Show)

type Program = [Command]
