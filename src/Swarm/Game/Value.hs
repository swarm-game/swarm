-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.Value
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Values and environments used for interpreting the Swarm language.
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Swarm.Game.Value
  ( -- * Values

    Value(..), prettyValue, valueToTerm

    -- * Environments

  , Env, empty, singleton, (!!!), union, addBinding

  ) where

import           Data.List             (foldl')
import           Data.Map              (Map)
import qualified Data.Map              as M
import qualified Data.Set              as S
import           Data.Text             (Text)

import           Swarm.Language.Pretty (prettyText)
import           Swarm.Language.Syntax
import           Witch                 (from)

-- | A /value/ is a term that cannot (or does not) take any more
--   evaluation steps on its own.
data Value where
  -- | The unit value.
  VUnit   :: Value

  -- | An integer.
  VInt    :: Integer -> Value

  -- | A literal string.
  VString :: Text -> Value

  -- | A direction.
  VDir    :: Direction -> Value

  -- | A boolean.
  VBool   :: Bool -> Value

  -- | A pair.
  VPair   :: Value -> Value -> Value

  -- | A /closure/, representing a lambda term along with an
  --   environment containing bindings for any free variables in the
  --   body of the lambda.
  VClo    :: Var -> Term -> Env -> Value

  -- | An application of a constant to some value arguments,
  --   potentially waiting for more arguments.  If a constant
  --   application is fully saturated (as defined by its 'arity'),
  --   whether it is a value or not depends on whether or not it
  --   represents a command (as defined by 'isCmd').  If a command
  --   (e.g. 'Build'), it is a value, and awaits an 'Swarm.Game.CEK.FExec' frame
  --   which will cause it to execute.  Otherwise (e.g. 'If'), it is
  --   not a value, and will immediately reduce.
  VCApp   :: Const -> [Value] -> Value

  -- | A definition, which is not evaluated until executed.
  VDef :: Var -> Term -> Env -> Value

  -- | The result of a command, consisting of the result of the
  --   command as well as an environment of bindings from 'TDef'
  --   commands.
  VResult :: Value -> Env -> Value

  -- | An unevaluated bind expression, waiting to be executed, of the
  --   form /i.e./ @c1 ; c2@ or @x <- c1; c2@.  We also store an 'Env'
  --   in which to interpret the commands.
  VBind   :: Maybe Var -> Term -> Term -> Env -> Value

  -- | A delayed term, along with its environment. If a term would
  --   otherwise be evaluated but we don't want it to be (/e.g./ as in
  --   the case of arguments to an 'if', or a recursive binding), we
  --   can stick a 'TDelay' on it, which turns it into a value.
  --   Delayed terms won't be evaluated until 'Force' is applied to
  --   them.
  VDelay  :: Term -> Env -> Value
  deriving (Eq, Show)

-- XXX write a more principled pretty-printer, i.e. actually make a
-- PrettyPrec instance
-- | Pretty-print a value.
prettyValue :: Value -> Text
prettyValue = prettyText . valueToTerm

-- | Inject a value back into an unannotated term.
valueToTerm :: Value -> Term
valueToTerm VUnit            = TUnit
valueToTerm (VInt n)         = TInt n
valueToTerm (VString s)      = TString s
valueToTerm (VDir d)         = TDir d
valueToTerm (VBool b)        = TBool b
valueToTerm (VPair v1 v2)    = TPair (valueToTerm v1) (valueToTerm v2)
valueToTerm (VClo x t e)     =
  M.foldrWithKey
    (\y v -> TLet y Nothing (valueToTerm v))
    (TLam x Nothing t)
    (M.restrictKeys e (S.delete x (fv t)))
valueToTerm (VCApp c vs)     = foldl' TApp (TConst c) (reverse (map valueToTerm vs))
valueToTerm (VDef x t _)     = TDef x Nothing t
valueToTerm (VResult v _)    = valueToTerm v
valueToTerm (VBind mx c1 c2 _) = TBind mx c1 c2
valueToTerm (VDelay t _)     = TDelay t

-- | An environment is a mapping from variable names to values.
type Env = Map Var Value

infix 9 !!!

-- | Unsafely look up variables in an environment, with a slightly
--   more informative error message than '(Data.Map.!)' to aid
--   debugging just in case something goes wrong.  But in theory, if
--   the type checker is doing its job and there are no bugs, a lookup
--   error will never happen.
(!!!) :: Env -> Var -> Value
e !!! x = case M.lookup x e of
  Nothing -> error $ from x ++ " is not a key in the environment!"
  Just v  -> v

-- | The empty environment.
empty :: Env
empty = M.empty

singleton :: Var -> Value -> Env
singleton = M.singleton

-- | Extend an environment, adding a binding of the given variable name to
--   the given value.
addBinding :: Var -> Value -> Env -> Env
addBinding = M.insert

-- | Left-biased union.
union :: Env -> Env -> Env
union = M.union
