-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Types
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types for the Swarm programming language.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE PatternSynonyms #-}

module Swarm.Language.Types
  ( BaseTy(..), TypeF(..), Type, UType, Polytype, UPolytype
  , pattern TyBase, pattern TyVar
  , pattern TyUnit, pattern TyInt, pattern TyString, pattern TyDir, pattern TyBool
  , pattern (:*:), pattern (:->:)
  , pattern TyCmd, pattern Cmd
  , Var, Ctx
  ) where

import           Control.Lens.Combinators   (pattern Empty)
import           Control.Unification
import           Control.Unification.IntVar
import           Data.Functor.Fixedpoint
import           Data.Map                   (Map)
import           Data.Text                  (Text)

data BaseTy
  = BUnit             -- ^ The unit type, with a single inhabitant.
  | BInt              -- ^ Signed, arbitrary-size integers.
  | BString           -- ^ Unicode strings.
  | BDir              -- ^ Directions.
  | BBool             -- ^ Booleans.
  deriving (Eq, Ord, Show)

-- | A data type representing types in the Swarm programming language.
data TypeF t
  = TyBaseF BaseTy -- ^ Base types.
  | TyVarF Var     -- ^ Type variables.
  | TyCmdF t Ctx   -- ^ Commands, with return type and output context
                   --   (from 'Def' commands). Note that commands form a
                   --   monad.
  | TyProdF t t    -- ^ Product type.
  | TyFunF t t     -- ^ Function type.
  deriving (Eq, Ord, Show)

type Type = Fix TypeF
type UType = UTerm TypeF IntVar

data Poly t = Forall [Var] t deriving (Functor)
type Polytype = Poly Type
type UPolytype = Poly UType

pattern TyBase :: BaseTy -> Type
pattern TyBase b = Fix (TyBaseF b)

pattern TyVar :: Var -> Type
pattern TyVar v = Fix (TyVarF v)

pattern TyUnit :: Type
pattern TyUnit   = Fix (TyBaseF BUnit)

pattern TyInt :: Type
pattern TyInt    = Fix (TyBaseF BInt)

pattern TyString :: Type
pattern TyString = Fix (TyBaseF BString)

pattern TyDir :: Type
pattern TyDir    = Fix (TyBaseF BDir)

pattern TyBool :: Type
pattern TyBool   = Fix (TyBaseF BBool)

infixl 6 :*:

pattern (:*:) :: Type -> Type -> Type
pattern ty1 :*: ty2 = Fix (TyProdF ty1 ty2)

infixr 1 :->:

pattern (:->:) :: Type -> Type -> Type
pattern ty1 :->: ty2 = Fix (TyFunF ty1 ty2)

pattern TyCmd :: Type -> Ctx -> Type
pattern TyCmd ty1 ctx = Fix (TyCmdF ty1 ctx)

pattern Cmd :: Type -> Type
pattern Cmd ty = Fix (TyCmdF ty Empty)

{-# COMPLETE TyCmd, (:*:), (:->:), TyBase #-}

-- | We use 'Text' values to represent variables.
type Var = Text

-- | A context is a mapping from variable names to their types.
type Ctx = Map Var Type
