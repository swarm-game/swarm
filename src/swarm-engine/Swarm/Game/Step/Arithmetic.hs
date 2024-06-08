{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Arithmetic and Comparison commands
module Swarm.Game.Step.Arithmetic where

import Control.Applicative (Applicative (..))
import Control.Carrier.State.Lazy
import Control.Effect.Error
import Control.Monad (zipWithM)
import Data.Function (on)
import Data.Map qualified as M
import Data.Text qualified as T
import Swarm.Game.Exception
import Swarm.Game.Step.Util
import Swarm.Language.Syntax
import Swarm.Language.Value
import Witch (From (from))
import Prelude hiding (Applicative (..), lookup)

------------------------------------------------------------
-- Comparison
------------------------------------------------------------

-- | Evaluate the application of a comparison operator.  Returns
--   @Nothing@ if the application does not make sense.
evalCmp :: Has (Throw Exn) sig m => Const -> Value -> Value -> m Bool
evalCmp c v1 v2 = decideCmp c $ compareValues v1 v2
 where
  decideCmp = \case
    Eq -> fmap (== EQ)
    Neq -> fmap (/= EQ)
    Lt -> fmap (== LT)
    Gt -> fmap (== GT)
    Leq -> fmap (/= GT)
    Geq -> fmap (/= LT)
    _ -> const . throwError . Fatal . T.append "evalCmp called on bad constant " . from $ show c

-- | Compare two values, returning an 'Ordering' if they can be
--   compared, or @Nothing@ if they cannot.
compareValues :: Has (Throw Exn) sig m => Value -> Value -> m Ordering
compareValues v1 = case v1 of
  VUnit -> \case VUnit -> return EQ; v2 -> incompatCmp VUnit v2
  VInt n1 -> \case VInt n2 -> return (compare n1 n2); v2 -> incompatCmp v1 v2
  VText t1 -> \case VText t2 -> return (compare t1 t2); v2 -> incompatCmp v1 v2
  VDir d1 -> \case VDir d2 -> return (compare d1 d2); v2 -> incompatCmp v1 v2
  VBool b1 -> \case VBool b2 -> return (compare b1 b2); v2 -> incompatCmp v1 v2
  VRobot r1 -> \case VRobot r2 -> return (compare r1 r2); v2 -> incompatCmp v1 v2
  VInj s1 v1' -> \case
    VInj s2 v2' ->
      case compare s1 s2 of
        EQ -> compareValues v1' v2'
        o -> return o
    v2 -> incompatCmp v1 v2
  VPair v11 v12 -> \case
    VPair v21 v22 ->
      (<>) <$> compareValues v11 v21 <*> compareValues v12 v22
    v2 -> incompatCmp v1 v2
  VRcd m1 -> \case
    VRcd m2 -> mconcat <$> (zipWithM compareValues `on` M.elems) m1 m2
    v2 -> incompatCmp v1 v2
  VKey kc1 -> \case
    VKey kc2 -> return (compare kc1 kc2)
    v2 -> incompatCmp v1 v2
  VClo {} -> incomparable v1
  VCApp {} -> incomparable v1
  VBind {} -> incomparable v1
  VDelay {} -> incomparable v1
  VRef {} -> incomparable v1
  VRequirements {} -> incomparable v1
  VExc {} -> incomparable v1

-- | Values with different types were compared; this should not be
--   possible since the type system should catch it.
incompatCmp :: Has (Throw Exn) sig m => Value -> Value -> m a
incompatCmp v1 v2 =
  throwError $
    Fatal $
      T.unwords ["Incompatible comparison of ", prettyValue v1, "and", prettyValue v2]

-- | Values were compared of a type which cannot be compared
--   (e.g. functions, etc.).
incomparable :: Has (Throw Exn) sig m => Value -> Value -> m a
incomparable v1 v2 =
  throwError $
    cmdExn
      Lt
      ["Comparison is undefined for ", prettyValue v1, "and", prettyValue v2]

------------------------------------------------------------
-- Arithmetic
------------------------------------------------------------

-- | Evaluate the application of an arithmetic operator, returning
--   an exception in the case of a failing operation, or in case we
--   incorrectly use it on a bad 'Const' in the library.
evalArith :: Has (Throw Exn) sig m => Const -> Integer -> Integer -> m Integer
evalArith = \case
  Add -> ok (+)
  Sub -> ok (-)
  Mul -> ok (*)
  Div -> safeDiv
  Exp -> safeExp
  c -> \_ _ -> throwError $ Fatal $ T.append "evalArith called on bad constant " (from (show c))
 where
  ok f x y = return $ f x y

-- | Perform an integer division, but return @Nothing@ for division by
--   zero.
safeDiv :: Has (Throw Exn) sig m => Integer -> Integer -> m Integer
safeDiv _ 0 = throwError $ cmdExn Div $ pure "Division by zero"
safeDiv a b = return $ a `div` b

-- | Perform exponentiation, but return @Nothing@ if the power is negative.
safeExp :: Has (Throw Exn) sig m => Integer -> Integer -> m Integer
safeExp a b
  | b < 0 = throwError $ cmdExn Exp $ pure "Negative exponent"
  | otherwise = return $ a ^ b
