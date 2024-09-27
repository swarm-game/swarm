{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: This module defines an effect signature for
-- computations that support doing unification.  The intention is for
-- code needing unification to use the operations defined in this
-- module, and then import 'Swarm.Effect.Unify.Fast' to dispatch the
-- 'Unification' effects.
module Swarm.Effect.Unify where

import Control.Algebra
import Data.Kind (Type)
import Data.Set (Set)
import Prettyprinter
import Swarm.Language.Types hiding (Type)
import Swarm.Pretty (PrettyPrec (..), ppr, reportBug)

-- | Data type representing available unification operations.
data Unification (m :: Type -> Type) k where
  Unify :: UType -> UType -> Unification m (Either UnificationError UType)
  ApplyBindings :: UType -> Unification m UType
  FreshIntVar :: Unification m IntVar
  FreeUVars :: UType -> Unification m (Set IntVar)

-- | Unify two types, returning a type equal to both, or a 'UnificationError' if
--   the types definitely do not unify.
(=:=) :: Has Unification sig m => UType -> UType -> m (Either UnificationError UType)
t1 =:= t2 = send (Unify t1 t2)

-- | Substitute for all the unification variables that are currently
--   bound.  It is guaranteed that any unification variables remaining
--   in the result are not currently bound, /i.e./ we have learned no
--   information about them.
applyBindings :: Has Unification sig m => UType -> m UType
applyBindings = send . ApplyBindings

-- | Compute the set of free unification variables of a type (after
--   substituting away any which are already bound).
freeUVars :: Has Unification sig m => UType -> m (Set IntVar)
freeUVars = send . FreeUVars

-- | Generate a fresh unification variable.
freshIntVar :: Has Unification sig m => m IntVar
freshIntVar = send FreshIntVar

-- | An error that occurred while running the unifier.
data UnificationError where
  -- | Occurs check failure, i.e. the solution to some unification
  --   equations was an infinite term.
  Infinite :: IntVar -> UType -> UnificationError
  -- | Mismatch error between the given terms.
  UnifyErr :: TypeF UType -> TypeF UType -> UnificationError
  -- | Encountered an undefined/unknown type constructor.
  UndefinedUserType :: UType -> UnificationError
  -- | Encountered an unexpanded recursive type in unifyF.  This
  --   should never happen.
  UnexpandedRecTy :: TypeF UType -> UnificationError
  deriving (Show)

instance PrettyPrec UnificationError where
  prettyPrec _ = \case
    Infinite x uty ->
      vsep
        [ "Encountered infinite type" <+> ppr x <+> "=" <+> ppr uty <> "."
        , "Swarm will not infer recursive types; if you want a recursive type, add an explicit type annotation."
        ]
    UnifyErr ty1 ty2 ->
      "Can't unify" <+> ppr ty1 <+> "and" <+> ppr ty2
    UndefinedUserType ty ->
      "Undefined user type" <+> ppr ty
    UnexpandedRecTy ty ->
      vsep
        [ "Unexpanded recursive type" <+> ppr ty <+> "encountered in unifyF."
        , reportBug
        ]
