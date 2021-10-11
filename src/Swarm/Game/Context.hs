-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Swarm.Game.Context (
  -- * Data types
  Ctx,
  VarCtx (..),
  UseCase (..),
  VarContextStore (..),

  -- ** Data constructors
  emptyVarContextStore,

  -- ** Inter-convert functions
  coerceToParseCommand,
  coerceToCapabilityCheck,
  coerceToEvaluateTerm,
) where

import Control.Applicative hiding (empty)
import Data.Data
import Data.Functor.Identity
import Data.Map
import Data.Set hiding (empty)
import Swarm.Game.Value
import Swarm.Language.Capability
import Swarm.Language.Types

-- | A context is a mapping from variable names to things.
newtype Ctx t = Ctx {unCtx :: Map Var t}
  deriving (Eq, Show, Functor, Foldable, Traversable, Data)

-- | Different uses for variable context
data UseCase
  = -- | Command parsing
    ParseCommand
  | -- | Checking capabilities for a definition
    CapabilityCheck
  | -- | Evaluating a Term
    EvaluateTerm
  | -- | Special context for base robot
    BaseRobot
  deriving (Eq, Ord, Show)

-- A variable context stores multiple information about
-- about the variable. The type of these things depends
-- on the 'UseCase' for which the variable context is
-- used.
data VarCtx u = VarCtx
  { -- Set of capaibilities required to compute a variable
    varCaps :: CapsForPhase u (Set Capability)
  , -- Type of the value the variable stores
    varType :: TypeForPhase u Polytype
  , -- Value of the variable
    varVal :: ValForPhase u Value
  }

-- convert variable context from 'BaseRobot' to 'ParseCommand' use case
coerceToParseCommand :: VarCtx 'BaseRobot -> VarCtx 'ParseCommand
coerceToParseCommand v = VarCtx {varCaps = Just . runIdentity . varCaps $ v, varType = varType v, varVal = Just . runIdentity . varVal $ v}

-- convert variable context from 'BaseRobot' to 'CapabilityCheck' use case
coerceToCapabilityCheck :: VarCtx 'BaseRobot -> VarCtx 'CapabilityCheck
coerceToCapabilityCheck v = VarCtx {varCaps = varCaps v, varType = Const (), varVal = Just . runIdentity . varVal $ v}

-- convert variable context from 'BaseRobot' to 'EvaluateTerm' use case
coerceToEvaluateTerm :: VarCtx 'BaseRobot -> VarCtx 'EvaluateTerm
coerceToEvaluateTerm v = VarCtx {varCaps = Const (), varType = Const (), varVal = varVal v}

type family CapsForPhase (u :: UseCase) :: * -> * where
  CapsForPhase 'ParseCommand = Maybe
  CapsForPhase 'CapabilityCheck = Identity
-- capabilities need not be accessed when
-- evaluating a term even if they already exist
  CapsForPhase 'EvaluateTerm = Const ()
  CapsForPhase 'BaseRobot = Identity

type family TypeForPhase (u :: UseCase) :: * -> * where
  TypeForPhase 'ParseCommand = Identity
  TypeForPhase 'CapabilityCheck = Const ()
  TypeForPhase 'EvaluateTerm = Const ()
  TypeForPhase 'BaseRobot = Identity

type family ValForPhase (u :: UseCase) :: * -> * where
  ValForPhase 'ParseCommand = Maybe
  ValForPhase 'CapabilityCheck = Maybe
  ValForPhase 'EvaluateTerm = Identity
  ValForPhase 'BaseRobot = Identity

-- Map variable names to variable context
newtype VarContextStore = VarContextStore (Map Var (VarCtx 'BaseRobot))

-- create empty variable context store
emptyVarContextStore :: VarContextStore
emptyVarContextStore = VarContextStore Data.Map.empty
