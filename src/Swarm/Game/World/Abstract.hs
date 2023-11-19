{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Explicitly type-preserving bracket abstraction, a la Oleg Kiselyov.
-- Turn elaborated, type-indexed terms into variableless, type-indexed
-- terms with only constants and application.
--
-- For more information, see:
--
--   https://byorgey.wordpress.com/2023/07/13/compiling-to-intrinsically-typed-combinators/
module Swarm.Game.World.Abstract where

import Data.Kind (Type)
import Swarm.Game.World.Typecheck (Applicable (..), Const (..), HasConst (..), Idx (..), TTerm (..), ($$.), (.$$), (.$$.))

------------------------------------------------------------
-- Bracket abstraction
------------------------------------------------------------

--------------------------------------------------
-- Closed terms

-- | Closed, fully abstracted terms.  All computation is represented
--   by combinators.  This is the ultimate target for the bracket
--   abstraction operation.
data BTerm :: Type -> Type where
  BApp :: BTerm (a -> b) -> BTerm a -> BTerm b
  BConst :: Const a -> BTerm a

deriving instance Show (BTerm t)

instance Applicable BTerm where
  ($$) = BApp

instance HasConst BTerm where
  embed = BConst

--------------------------------------------------
-- Open terms

-- | These explicitly open terms are an intermediate stage in the
--   bracket abstraction algorithm, /i.e./ they represent terms which have
--   been only partially abstracted.
data OTerm :: [Type] -> Type -> Type where
  -- Embedded closed term.
  E :: BTerm a -> OTerm g a
  -- Reference to the innermost/top environment variable, i.e. Z
  V :: OTerm (a ': g) a
  -- Internalize the topmost env variable as a function argument
  N :: OTerm g (a -> b) -> OTerm (a ': g) b
  -- Ignore the topmost env variable
  W :: OTerm g b -> OTerm (a ': g) b

instance HasConst (OTerm g) where
  embed = E . embed

-- | Bracket abstraction: convert the 'TTerm' to an 'OTerm', then
--   project out the embedded 'BTerm'.  GHC can see this is total
--   since 'E' is the only constructor that can produce an 'OTerm'
--   with an empty environment.
bracket :: TTerm '[] a -> BTerm a
bracket t = case conv t of
  E t' -> t'

-- | Type-preserving conversion from 'TTerm' to 'OTerm' ('conv' + the
--   'Applicable' instance).  Taken directly from Kiselyov.
conv :: TTerm g a -> OTerm g a
conv (TVar VZ) = V
conv (TVar (VS x)) = W (conv (TVar x))
conv (TLam t) = case conv t of
  V -> E (BConst I)
  E d -> E (K .$$ d)
  N e -> e
  W e -> K .$$ e
conv (TApp t1 t2) = conv t1 $$ conv t2
conv (TConst c) = embed c

instance Applicable (OTerm g) where
  ($$) :: OTerm g (a -> b) -> OTerm g a -> OTerm g b
  W e1 $$ W e2 = W (e1 $$ e2)
  W e $$ E d = W (e $$ E d)
  E d $$ W e = W (E d $$ e)
  W e $$ V = N e
  V $$ W e = N (E (C .$$. I) $$ e)
  W e1 $$ N e2 = N (B .$$ e1 $$ e2)
  N e1 $$ W e2 = N (C .$$ e1 $$ e2)
  N e1 $$ N e2 = N (S .$$ e1 $$ e2)
  N e $$ V = N (S .$$ e $$. I)
  V $$ N e = N (E (S .$$. I) $$ e)
  E d $$ N e = N (E (B .$$ d) $$ e)
  E d $$ V = N (E d)
  V $$ E d = N (E (C .$$. I $$ d))
  N e $$ E d = N (E (C .$$. C $$ d) $$ e)
  E d1 $$ E d2 = E (d1 $$ d2)

-- There are only 15 cases above: GHC can tell that V $$ V is
-- impossible (it would be ill-typed)!
