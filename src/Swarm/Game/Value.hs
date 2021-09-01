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

  , Env, emptyEnv, (!!!), addBinding

  ) where

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
  VClo    :: Text -> UTerm -> Env -> Value

  -- | An application of a constant to some value arguments,
  --   potentially waiting for more arguments.  If a constant
  --   application is fully saturated (as defined by its 'arity'),
  --   whether it is a value or not depends on whether or not it
  --   represents a command (as defined by 'isCmd').  If a command
  --   (e.g. 'Build'), it is a value, and awaits an 'Swarm.Game.CEK.FExec' frame
  --   which will cause it to execute.  Otherwise (e.g. 'If'), it is
  --   not a value, and will immediately reduce.
  VCApp   :: Const -> [Value] -> Value

  -- | A bind where the first component has been reduced to a value,
  --   /i.e./ @v ; c@ or @x <- v; c@.  We also store an 'Env' in which
  --   to interpret the second component of the bind.
  VBind   :: Value -> Maybe Var -> UTerm -> Env -> Value

  -- | A delayed term, along with its environment. If a term would
  --   otherwise be evaluated but we don't want it to be (/e.g./ as in
  --   the case of arguments to an 'if', or a recursive binding), we
  --   can stick a 'TDelay' on it, which turns it into a value.
  --   Delayed terms won't be evaluated until 'Force' is applied to
  --   them.
  VDelay  :: UTerm -> Env -> Value
  deriving (Eq, Ord, Show)

-- XXX write a more principled pretty-printer, i.e. actually make a
-- PrettyPrec instance
-- | Pretty-print a value.
prettyValue :: Value -> Text
prettyValue = prettyText . valueToTerm

-- | Inject a value back into an unannotated term.
valueToTerm :: Value -> UTerm
valueToTerm VUnit            = TUnit
valueToTerm (VInt n)         = TInt n
valueToTerm (VString s)      = TString s
valueToTerm (VDir d)         = TDir d
valueToTerm (VBool b)        = TBool b
valueToTerm (VPair v1 v2)    = TPair (valueToTerm v1) (valueToTerm v2)
valueToTerm (VClo x t e)     =
  M.foldrWithKey
    (\y v -> TLet y NONE (valueToTerm v))
    (TLam x NONE t)
    (M.restrictKeys e (S.delete x (fv t)))
valueToTerm (VCApp c vs)     = foldl (TApp NONE) (TConst c) (reverse (map valueToTerm vs))
valueToTerm (VBind v mx t _) = TBind mx NONE (valueToTerm v) t
valueToTerm (VDelay t _)     = TDelay t

-- | An environment is a mapping from variable names to values.
type Env = Map Var Value

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
emptyEnv :: Env
emptyEnv = M.empty

-- | Extend an environment, adding a binding of the given variable name to
--   the given value.
addBinding :: Var -> Value -> Env -> Env
addBinding = M.insert
