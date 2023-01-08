{-# LANGUAGE OverloadedStrings #-}

module Swarm.Language.LSP.VarUsage where

import Data.Maybe ( maybeToList )
import Swarm.Language.Syntax

data VarUsageType
  = Lambda
  | Let
  | Def
  | Bind
  | Reference

data VarUsage = VarUsage SrcLoc VarUsageType Var

getVars :: Syntax -> [VarUsage]
getVars (Syntax pos t) = case t of
  TUnit -> []
  TConst {} -> []
  TDir {} -> []
  TInt {} -> []
  TAntiInt {} -> []
  TText {} -> []
  TAntiText {} -> []
  TBool {} -> []
  TRobot {} -> []
  TRef {} -> []
  TRequireDevice {} -> []
  TRequire {} -> []
  TVar x -> pure $ u Reference x
  SLam x _ s -> u Lambda x : getVars s
  SApp s1 s2 -> getVars s1 <> getVars s2
  SLet _ x _ s1 s2 -> u Let x : getVars s1 <> getVars s2
  SPair s1 s2 -> getVars s1 <> getVars s2
  SDef _ x _ s-> u Def x : getVars s
  SBind x s1 s2 -> maybeToList (fmap (u Bind) x) <> getVars s1 <> getVars s2
  SDelay _ s -> getVars s
  where
    u = VarUsage pos
