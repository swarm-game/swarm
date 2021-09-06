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

{-# LANGUAGE PatternSynonyms #-}

module Swarm.Language.Types
  ( BaseTy(..), Type(.., TyCmd, TyUnit, TyInt, TyString, TyDir, TyBool), Var, Ctx
  ) where

import           Control.Lens.Combinators (pattern Empty)
import           Data.Map                 (Map)
import           Data.Text                (Text)

data BaseTy
  = BUnit             -- ^ The unit type, with a single inhabitant.
  | BInt              -- ^ Signed, arbitrary-size integers.
  | BString           -- ^ Unicode strings.
  | BDir              -- ^ Directions.
  | BBool             -- ^ Booleans.
  deriving (Eq, Ord, Show)

-- | A data type representing types in the Swarm programming language.
data Type
  = TyBase BaseTy      -- ^ Base types.
  | TyCmd' Type Ctx    -- ^ Commands, with return type. Note that
                       --   commands form a monad.
  | Type :*: Type      -- ^ Product type.
  | Type :->: Type     -- ^ Function type.
  deriving (Eq, Ord, Show)

pattern TyUnit :: Type
pattern TyUnit   = TyBase BUnit

pattern TyInt :: Type
pattern TyInt    = TyBase BInt

pattern TyString :: Type
pattern TyString = TyBase BString

pattern TyDir :: Type
pattern TyDir    = TyBase BDir

pattern TyBool :: Type
pattern TyBool   = TyBase BBool

{-# COMPLETE TyCmd', (:*:), (:->:), TyUnit, TyInt, TyString, TyDir, TyBool #-}

infixr 1 :->:

pattern TyCmd :: Type -> Type
pattern TyCmd ty = TyCmd' ty Empty

-- | We use 'Text' values to represent variables.
type Var = Text

-- | A context is a mapping from variable names to their types.
type Ctx = Map Var Type
