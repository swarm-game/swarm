-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Kind checking for the Swarm language.
module Swarm.Language.Kindcheck (
  KindError (..),
  checkPolytypeKind,
  checkKind,
) where

import Control.Algebra (Has)
import Control.Effect.Throw (Throw, throwError)
import Data.Fix (Fix (..))
import Swarm.Language.Types (Poly (..), Polytype, TyCon, Type, TypeF (..), getArity, tcArity)

-- | Kind checking errors that can occur.  For now, the only possible
--   error is an arity mismatch error.
data KindError
  = ArityMismatch TyCon [Type]
  deriving (Eq, Show)

-- | Check that a polytype is well-kinded.
checkPolytypeKind :: Has (Throw KindError) sig m => Polytype -> m ()
checkPolytypeKind (Forall _ t) = checkKind t

-- | Check that a type is well-kinded. For now, we don't allow
--   higher-kinded types, *i.e.* all kinds will be of the form @Type
--   -> Type -> ... -> Type@ which can be represented by a number (the
--   arity); every type constructor must also be fully applied. So, we
--   only have to check that each type constructor is applied to the
--   correct number of type arguments.  In the future, we might very
--   well want to generalize to arbitrary higher kinds (e.g. @(Type ->
--   Type) -> Type@ etc.) which would require generalizing this
--   checking code a bit.
checkKind :: Has (Throw KindError) sig m => Type -> m ()
checkKind (Fix (TyConF c tys)) = case compare (length tys) (getArity (tcArity c)) of
  EQ -> mapM_ checkKind tys
  _ -> throwError $ ArityMismatch c tys
checkKind (Fix (TyVarF _)) = return ()
checkKind (Fix (TyRcdF m)) = mapM_ checkKind m
