{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Compiling Swarm world description DSL to native Haskell terms, via
-- combinators.
--
-- XXX for more info see ...
module Swarm.Game.World.Compile where

import Data.Kind (Type)
import Swarm.Game.World.Syntax
import Swarm.Game.World.Typecheck

------------------------------------------------------------
-- Bracket abstraction
------------------------------------------------------------

-- Explicitly type-preserving bracket abstraction, a la Oleg Kiselyov.
-- See:
--
--   http://okmij.org/ftp/tagless-final/ski.pdf
--   http://okmij.org/ftp/tagless-final/skconv.ml

--------------------------------------------------
-- Closed terms

-- | Closed, fully abstracted terms.  All computation is represented
--   by combinators.  This is the target for the bracket abstraction
--   operation.
data BTerm :: Type -> Type where
  BApp :: BTerm (a -> b) -> BTerm a -> BTerm b
  BConst :: Const a -> BTerm a

-- Direct interpreter for BTerm, for debugging/comparison.
interpBTerm :: BTerm ty -> ty
interpBTerm (BApp f x) = interpBTerm f (interpBTerm x)
interpBTerm (BConst c) = interpConst c

deriving instance Show (BTerm t)

instance Applicable BTerm where
  ($$) = BApp

instance HasConst BTerm where
  embed = BConst

--------------------------------------------------
-- Open terms

-- These explicitly open terms are an intermediate stage in the
-- bracket abstraction algorithm.
data OTerm :: [Type] -> Type -> Type where
  -- Embedded closed term.
  OC :: BTerm a -> OTerm g a
  -- Reference to the innermost/top environment variable, i.e. Z
  OV :: OTerm (a ': g) a
  -- Internalize the topmost env variable as a function argument
  ON :: OTerm g (a -> b) -> OTerm (a ': g) b
  -- Ignore the topmost env variable
  OW :: OTerm g b -> OTerm (a ': g) b

instance HasConst (OTerm g) where
  embed = OC . embed

-- Bracket abstraction: convert the TTerm to an OTerm, then project
-- out the embedded BTerm.  GHC can see this is total since OC is the
-- only constructor that can produce an OTerm with an empty
-- environment.
bracket :: TTerm '[] a -> BTerm a
bracket t = case conv t of
  OC t' -> t'

-- Type-preserving conversion from TTerm to OTerm (conv + the
-- Applicable instance).  Taken directly from Kiselyov.
conv :: TTerm g a -> OTerm g a
conv (TVar VZ) = OV
conv (TVar (VS x)) = OW (conv (TVar x))
conv (TLam t) = case conv t of
  OV -> OC (BConst I)
  OC d -> OC (K .$ d)
  ON e -> e
  OW e -> K .$ e
conv (TApp t1 t2) = conv t1 $$ conv t2
conv (TConst c) = embed c

instance Applicable (OTerm g) where
  OW e1 $$ OW e2 = OW (e1 $$ e2)
  OW e $$ OC d = OW (e $$ OC d)
  OC d $$ OW e = OW (OC d $$ e)
  OW e $$ OV = ON e
  OV $$ OW e = ON (OC (C .$. I) $$ e)
  OW e1 $$ ON e2 = ON (B .$ e1 $$ e2)
  ON e1 $$ OW e2 = ON (C .$ e1 $$ e2)
  ON e1 $$ ON e2 = ON (S .$ e1 $$ e2)
  ON e $$ OV = ON (S .$ e $. I)
  OV $$ ON e = ON (OC (S .$. I) $$ e)
  OC d $$ ON e = ON (OC (B .$ d) $$ e)
  OC d $$ OV = ON (OC d)
  OV $$ OC d = ON (OC (C .$. I $$ d))
  ON e $$ OC d = ON (OC (C .$. C $$ d) $$ e)
  OC d1 $$ OC d2 = OC (d1 $$ d2)

-- GHC can tell that OV $$ OV is impossible (it would be ill-typed)

------------------------------------------------------------
-- Compiling

-- Compiling to the host language. i.e. we co-opt the host language
-- into doing evaluation for us.

-- XXX more efficient than directly interpreting BTerms?  Should try
-- benchmarking.

data CTerm a where
  CFun :: (CTerm a -> CTerm b) -> CTerm (a -> b)
  CConst :: a -> CTerm a -- only for non-functions!

instance Applicable CTerm where
  CFun f $$ x = f x
  -- above only gives a non-exhaustive warning since we can't express the constraint
  -- that CConst isn't allowed contain a function
  _ $$ _ = error "impossible! bad call to CTerm.$$"

compile :: BTerm a -> CTerm a
compile (BApp b1 b2) = compile b1 $$ compile b2
compile (BConst c) = compileConst c

compileConst :: Const a -> CTerm a
compileConst = \case
  (CLit i) -> CConst i
  CFI -> unary fromIntegral
  CIf -> CFun $ \(CConst b) -> CFun $ \t -> CFun $ \e -> if b then t else e
  CNot -> unary not
  CNeg -> unary negate
  CAnd -> binary (&&)
  COr -> binary (||)
  CAdd -> binary (+)
  CSub -> binary (-)
  CMul -> binary (*)
  CDiv -> binary (/)
  CIDiv -> binary div
  CMod -> binary mod
  CEq -> binary (==)
  CNeq -> binary (/=)
  CLt -> binary (<)
  CLeq -> binary (<=)
  CGt -> binary (>)
  CGeq -> binary (>=)
  CMask -> undefined
  CSeed -> undefined
  CCoord ax -> undefined
  CHash -> undefined
  CPerlin -> undefined
  CReflect r -> undefined
  CRot r -> undefined
  COver -> binary (<+>)
  CEmpty -> CConst empty
  K -> CFun $ \x -> CFun $ const x
  S -> CFun $ \f -> CFun $ \g -> CFun $ \x -> f $$ x $$ (g $$ x)
  I -> CFun id
  B -> CFun $ \f -> CFun $ \g -> CFun $ \x -> f $$ (g $$ x)
  C -> CFun $ \f -> CFun $ \x -> CFun $ \y -> f $$ y $$ x

unary :: (a -> b) -> CTerm (a -> b)
unary op = CFun $ \(CConst x) -> CConst (op x)

binary :: (a -> b -> c) -> CTerm (a -> b -> c)
binary op = CFun $ \(CConst x) -> CFun $ \(CConst y) -> CConst (op x y)

runCTerm :: CTerm a -> a
runCTerm (CConst a) = a
runCTerm (CFun f) = runCTerm . f . CConst
