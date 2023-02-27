{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Values and environments used for interpreting the Swarm language.
module Swarm.Language.Value (
  -- * Values
  Value (..),
  stripVResult,
  prettyValue,
  valueToTerm,

  -- * Environments
  Env,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bool (bool)
import Data.List (foldl')
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Set.Lens (setOf)
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Language.Context
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax
import Prelude

-- | A /value/ is a term that cannot (or does not) take any more
--   evaluation steps on its own.
data Value where
  -- | The unit value.
  VUnit :: Value
  -- | An integer.
  VInt :: Integer -> Value
  -- | Literal text.
  VText :: Text -> Value
  -- | A direction.
  VDir :: Direction -> Value
  -- | A boolean.
  VBool :: Bool -> Value
  -- | A reference to a robot.
  VRobot :: Int -> Value
  -- | An injection into a sum type.  False = left, True = right.
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
  --   (e.g. 'Build'), it is a value, and awaits an 'Swarm.Game.CESK.FExec' frame
  --   which will cause it to execute.  Otherwise (e.g. 'If'), it is
  --   not a value, and will immediately reduce.
  VCApp :: Const -> [Value] -> Value
  -- | A definition, which does not take effect until executed.
  --   The @Bool@ indicates whether the definition is recursive.
  VDef :: Bool -> Var -> Term -> Env -> Value
  -- | The result of a command, consisting of the result of the
  --   command as well as an environment of bindings from 'TDef'
  --   commands.
  VResult :: Value -> Env -> Value
  -- | An unevaluated bind expression, waiting to be executed, of the
  --   form /i.e./ @c1 ; c2@ or @x <- c1; c2@.  We also store an 'Env'
  --   in which to interpret the commands.
  VBind :: Maybe Var -> Term -> Term -> Env -> Value
  -- | A (non-recursive) delayed term, along with its environment. If
  --   a term would otherwise be evaluated but we don't want it to be
  --   (/e.g./ as in the case of arguments to an 'if', or a recursive
  --   binding), we can stick a 'TDelay' on it, which turns it into a
  --   value.  Delayed terms won't be evaluated until 'Force' is
  --   applied to them.
  VDelay :: Term -> Env -> Value
  -- | A reference to a memory cell in the store.
  VRef :: Int -> Value
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Ensure that a value is not wrapped in 'VResult'.
stripVResult :: Value -> Value
stripVResult (VResult v _) = stripVResult v
stripVResult v = v

-- | Pretty-print a value.
prettyValue :: Value -> Text
prettyValue = prettyText . valueToTerm

-- | Inject a value back into a term.
valueToTerm :: Value -> Term
valueToTerm VUnit = TUnit
valueToTerm (VInt n) = TInt n
valueToTerm (VText s) = TText s
valueToTerm (VDir d) = TDir d
valueToTerm (VBool b) = TBool b
valueToTerm (VRobot r) = TRobot r
valueToTerm (VInj s v) = TApp (TConst (bool Inl Inr s)) (valueToTerm v)
valueToTerm (VPair v1 v2) = TPair (valueToTerm v1) (valueToTerm v2)
valueToTerm (VClo x t e) =
  M.foldrWithKey
    (\y v -> TLet False y Nothing (valueToTerm v))
    (TLam x Nothing t)
    (M.restrictKeys (unCtx e) (S.delete x (setOf freeVarsV (Syntax' NoLoc t ()))))
valueToTerm (VCApp c vs) = foldl' TApp (TConst c) (reverse (map valueToTerm vs))
valueToTerm (VDef r x t _) = TDef r x Nothing t
valueToTerm (VResult v _) = valueToTerm v
valueToTerm (VBind mx c1 c2 _) = TBind mx c1 c2
valueToTerm (VDelay t _) = TDelay SimpleDelay t
valueToTerm (VRef n) = TRef n

-- | An environment is a mapping from variable names to values.
type Env = Ctx Value
