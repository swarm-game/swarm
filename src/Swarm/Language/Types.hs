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

module Swarm.Language.Types where

-- | A data type representing types in the Swarm programming language.
data Type
  = TyUnit             -- ^ The unit type, with a single inhabitant.
  | TyInt              -- ^ Signed, arbitrary-size integers.
  | TyString           -- ^ Unicode strings.
  | TyDir              -- ^ Directions.
  | TyBool             -- ^ Booleans.
  | TyCmd Type         -- ^ Commands, with return type. Note that
                       --   commands form a monad.
  | Type :->: Type     -- ^ Functions.
  deriving (Eq, Ord, Show)

infixr 1 :->:
