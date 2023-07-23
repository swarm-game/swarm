{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Compiling Swarm world description DSL to native Haskell terms, via
-- combinators.
--
-- XXX for more info see ...
module Swarm.Game.World.Compile where

import Data.ByteString (ByteString)
import Data.Hash.Murmur (murmur3)
import Data.Tagged (Tagged (unTagged))
import Numeric.Noise.Perlin (noiseValue, perlin)
import Swarm.Game.Location (pattern Location)
import Swarm.Game.World.Abstract (BTerm (..))
import Swarm.Game.World.Coords (Coords (..), coordsToLoc)
import Swarm.Game.World.Gen (Seed)
import Swarm.Game.World.Syntax
import Swarm.Game.World.Typecheck
import Witch (from)
import Witch.Encoding qualified as Encoding

------------------------------------------------------------
-- Compiling

-- Compiling to the host language. i.e. we co-opt the host language
-- into doing evaluation for us.

-- XXX more efficient than directly interpreting BTerms?  Should try
-- benchmarking.

data CTerm a where
  CFun :: (CTerm a -> CTerm b) -> CTerm (a -> b)
  CConst :: NotFun a => a -> CTerm a

instance Applicable CTerm where
  CFun f $$ x = f x

compile :: Seed -> BTerm a -> CTerm a
compile seed (BApp b1 b2) = compile seed b1 $$ compile seed b2
compile seed (BConst c) = compileConst seed c

compileConst :: Seed -> Const a -> CTerm a
compileConst seed = \case
  CLit i -> CConst i
  CCell c -> CConst c
  CFI -> unary fromIntegral
  CIf -> CFun $ \(CConst b) -> CFun $ \t -> CFun $ \e -> if b then t else e
  CNot -> unary not
  CNeg -> unary negate
  CAbs -> unary abs
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
  CMask -> compileMask
  CSeed -> CConst (fromIntegral seed)
  CCoord ax -> CFun $ \(CConst (coordsToLoc -> Location x y)) -> CConst (fromIntegral (case ax of X -> x; Y -> y))
  CHash -> compileHash
  CPerlin -> compilePerlin
  CReflect _r -> undefined
  CRot _r -> undefined
  COver -> binary (<+>)
  CEmpty -> CConst empty
  K -> CFun $ \x -> CFun $ const x
  S -> CFun $ \f -> CFun $ \g -> CFun $ \x -> f $$ x $$ (g $$ x)
  I -> CFun id
  B -> CFun $ \f -> CFun $ \g -> CFun $ \x -> f $$ (g $$ x)
  C -> CFun $ \f -> CFun $ \x -> CFun $ \y -> f $$ y $$ x
  Φ -> CFun $ \c -> CFun $ \f -> CFun $ \g -> CFun $ \x -> c $$ (f $$ x) $$ (g $$ x)

unary :: (NotFun a, NotFun b) => (a -> b) -> CTerm (a -> b)
unary op = CFun $ \(CConst x) -> CConst (op x)

binary :: (NotFun a, NotFun b, NotFun c) => (a -> b -> c) -> CTerm (a -> b -> c)
binary op = CFun $ \(CConst x) -> CFun $ \(CConst y) -> CConst (op x y)

-- Note we could desugar 'mask p a -> if p a empty' but that would
-- require an explicit 'empty' node, whose type can't be inferred.
compileMask :: (NotFun a, Empty a) => CTerm (World Bool -> World a -> World a)
compileMask = CFun $ \p -> CFun $ \a -> CFun $ \ix ->
  case p $$ ix of
    CConst b -> if b then a $$ ix else CConst empty

compileHash :: CTerm (Coords -> Integer)
compileHash = CFun $ \(CConst (Coords ix)) -> CConst (fromIntegral (h ix))
 where
  h = murmur3 0 . unTagged . from @String @(Encoding.UTF_8 ByteString) . show

compilePerlin :: CTerm (Integer -> Integer -> Double -> Double -> World Double)
compilePerlin =
  CFun $ \(CConst s) ->
    CFun $ \(CConst o) ->
      CFun $ \(CConst k) ->
        CFun $ \(CConst p) ->
          let noise = perlin (fromIntegral s) (fromIntegral o) k p
           in CFun $ \(CConst (Coords ix)) -> CConst (sample ix noise)
 where
  sample (i, j) noise = noiseValue noise (fromIntegral i / 2, fromIntegral j / 2, 0)
