-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      :  Swarm.Game.Value
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Values and environments used for interpreting the Swarm language.
module Swarm.Game.Value (
  -- * Values
  Value (..),
  pattern VLeft,
  pattern VRight,
  prettyValue,
  valueToTerm,

  -- * Environments
  Env,
) where

import Data.Bool (bool)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set.Lens (setOf)
import Data.Text (Text)
import Prelude hiding (Left, Right)

import Swarm.Language.Context
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax

-- | A /value/ is a term that cannot (or does not) take any more
--   evaluation steps on its own.
data Value where
  -- | The unit value.
  VUnit :: Value
  -- | An integer.
  VInt :: Integer -> Value
  -- | A literal string.
  VString :: Text -> Value
  -- | A direction.
  VDir :: Direction -> Value
  -- | A boolean.
  VBool :: Bool -> Value
  -- | An injection into a sum type.  False = Left, True = Right.
  VInj :: Bool -> Value -> Value
  -- | A pair.
  VPair :: Value -> Value -> Value
  -- | A /closure/, representing a lambda term along with an
  --   environment containing bindings for any free variables in the
  --   body of the lambda.
  VClo :: Var -> Term -> Env -> Value
  -- | An application of a constant to some value arguments,
  --   potentially waiting for more arguments.  If a constant
  --   application is fully saturated (as defined by its 'arity'),
  --   whether it is a value or not depends on whether or not it
  --   represents a command (as defined by 'isCmd').  If a command
  --   (e.g. 'Build'), it is a value, and awaits an 'Swarm.Game.CEK.FExec' frame
  --   which will cause it to execute.  Otherwise (e.g. 'If'), it is
  --   not a value, and will immediately reduce.
  VCApp :: Const -> [Value] -> Value
  -- | A definition, which is not evaluated until executed.
  VDef :: Var -> Term -> Env -> Value
  -- | The result of a command, consisting of the result of the
  --   command as well as an environment of bindings from 'TDef'
  --   commands.
  VResult :: Value -> Env -> Value
  -- | An unevaluated bind expression, waiting to be executed, of the
  --   form /i.e./ @c1 ; c2@ or @x <- c1; c2@.  We also store an 'Env'
  --   in which to interpret the commands.
  VBind :: Maybe Var -> Term -> Term -> Env -> Value
  -- | A delayed term, along with its environment. If a term would
  --   otherwise be evaluated but we don't want it to be (/e.g./ as in
  --   the case of arguments to an 'if', or a recursive binding), we
  --   can stick a 'TDelay' on it, which turns it into a value.
  --   Delayed terms won't be evaluated until 'Force' is applied to
  --   them.
  --
  --   Delayed terms are also how we implement recursion.  If the
  --   variable is @Just@, it indicates a recursive binding.  When the
  --   @Term@ is evaluated, it should be evaluated in the given
  --   environment /plus/ a binding of the variable to the entire
  --   @VDelay@ itself.
  VDelay :: Maybe Var -> Term -> Env -> Value
  deriving (Eq, Show)

pattern VLeft, VRight :: Value -> Value
pattern VLeft v = VInj False v
pattern VRight v = VInj True v

-- | Pretty-print a value.
prettyValue :: Value -> Text
prettyValue = prettyText . valueToTerm

-- | Inject a value back into a term.
valueToTerm :: Value -> Term
valueToTerm VUnit = TUnit
valueToTerm (VInt n) = TInt n
valueToTerm (VString s) = TString s
valueToTerm (VDir d) = TDir d
valueToTerm (VBool b) = TBool b
valueToTerm (VInj s v) = TApp (TConst (bool Left Right s)) (valueToTerm v)
valueToTerm (VPair v1 v2) = TPair (valueToTerm v1) (valueToTerm v2)
valueToTerm (VClo x t e) =
  M.foldrWithKey
    (\y v -> TLet y Nothing (valueToTerm v))
    (TLam x Nothing t)
    (M.restrictKeys (unCtx e) (S.delete x (setOf fv t)))
valueToTerm (VCApp c vs) = foldl' TApp (TConst c) (reverse (map valueToTerm vs))
valueToTerm (VDef x t _) = TDef x Nothing t
valueToTerm (VResult v _) = valueToTerm v
valueToTerm (VBind mx c1 c2 _) = TBind mx c1 c2
valueToTerm (VDelay _ t _) = TDelay t

-- | An environment is a mapping from variable names to values.
type Env = Ctx Value
