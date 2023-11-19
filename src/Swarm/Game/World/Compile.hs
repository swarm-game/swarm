{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Compiling abstracted combinator expressions ('BTerm') to native
-- Haskell terms.  This can supposedly be more efficient than directly
-- interpreting 'BTerm's, but some benchmarking is probably needed to
-- decide whether we want this or not.
--
-- For more info, see:
--
--   https://byorgey.wordpress.com/2023/07/13/compiling-to-intrinsically-typed-combinators/
module Swarm.Game.World.Compile where

import Data.ByteString (ByteString)
import Data.Hash.Murmur (murmur3)
import Data.Kind (Constraint)
import Data.Tagged (Tagged (unTagged))
import Numeric.Noise.Perlin (noiseValue, perlin)
import Swarm.Game.Location (pattern Location)
import Swarm.Game.World.Abstract (BTerm (..))
import Swarm.Game.World.Coords (Coords (..), coordsToLoc)
import Swarm.Game.World.Gen (Seed)
import Swarm.Game.World.Interpret (interpReflect, interpRot)
import Swarm.Game.World.Syntax (Axis (..), Rot, World)
import Swarm.Game.World.Typecheck (Applicable (..), Const (..), Empty (..), NotFun, Over (..))
import Witch (from)
import Witch.Encoding qualified as Encoding

data CTerm a where
  CFun :: (CTerm a -> CTerm b) -> CTerm (a -> b)
  CConst :: (NotFun a) => a -> CTerm a

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
  CReflect ax -> compileReflect ax
  CRot rot -> compileRot rot
  COver -> binary (<!>)
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

compileReflect :: Axis -> CTerm (World a -> World a)
compileReflect ax = CFun $ \w -> CFun $ \(CConst c) -> w $$ CConst (interpReflect ax c)

compileRot :: Rot -> CTerm (World a -> World a)
compileRot rot = CFun $ \w -> CFun $ \(CConst c) -> w $$ CConst (interpRot rot c)

type family NoFunParams a :: Constraint where
  NoFunParams (a -> b) = (NotFun a, NoFunParams b)
  NoFunParams _ = ()

-- | Interpret a compiled term into the host language.
runCTerm :: (NoFunParams a) => CTerm a -> a
runCTerm (CConst a) = a
runCTerm (CFun f) = runCTerm . f . CConst
