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
  ( Type(.., TyCmd), Var, Ctx
  ) where

import           Control.Lens.Combinators (pattern Empty)
import           Data.Map                 (Map)
import           Data.Text                (Text)

-- | A data type representing types in the Swarm programming language.
data Type
  = TyUnit             -- ^ The unit type, with a single inhabitant.
  | TyInt              -- ^ Signed, arbitrary-size integers.
  | TyString           -- ^ Unicode strings.
  | TyDir              -- ^ Directions.
  | TyBool             -- ^ Booleans.
  | TyCmd' Type Ctx    -- ^ Commands, with return type. Note that
                       --   commands form a monad.
  | Type :*: Type      -- ^ Product type.
  | Type :->: Type     -- ^ Function type.
  deriving (Eq, Ord, Show)

infixr 1 :->:

pattern TyCmd :: Type -> Type
pattern TyCmd ty = TyCmd' ty Empty

-- | We use 'Text' values to represent variables.
type Var = Text

-- | A context is a mapping from variable names to their types.
type Ctx = Map Var Type
