{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Interpreter for the Swarm world description DSL.
module Swarm.Game.World.DSL.Interpret (
  interpBTerm,
  interpConst,
) where

import Data.ByteString (ByteString)
import Data.Hash.Murmur (murmur3)
import Data.Tagged (unTagged)
import Numeric.Noise.Perlin (noiseValue, perlin)
import Swarm.Game.Location (pattern Location)
import Swarm.Game.World.Coords (Coords (..), coordsToLoc, locToCoords)
import Swarm.Game.World.DSL.Abstract (BTerm (..))
import Swarm.Game.World.DSL.Gen (Seed)
import Swarm.Game.World.DSL.Syntax (Axis (..))
import Swarm.Game.World.DSL.Typecheck (Const (..), Empty (..), Over (..))
import Witch (from)
import Witch.Encoding qualified as Encoding

-- | Interpret an abstracted term into the host language.
interpBTerm :: Seed -> BTerm a -> a
interpBTerm seed (BApp f x) = interpBTerm seed f (interpBTerm seed x)
interpBTerm seed (BConst c) = interpConst seed c

-- | Interpret a constant into the host language.
interpConst :: Seed -> Const a -> a
interpConst seed = \case
  CLit a -> a
  CCell c -> c
  CIf -> \b t e -> if b then t else e
  CNot -> not
  CNeg -> negate
  CAbs -> abs
  CAnd -> (&&)
  COr -> (||)
  CAdd -> (+)
  CSub -> (-)
  CMul -> (*)
  CDiv -> (/)
  CIDiv -> div
  CMod -> mod
  CEq -> (==)
  CNeq -> (/=)
  CLt -> (<)
  CLeq -> (<=)
  CGt -> (>)
  CGeq -> (>=)
  CMask -> \b x c -> if b c then x c else empty
  CSeed -> fromIntegral seed
  CCoord ax -> \(coordsToLoc -> Location x y) -> fromIntegral (case ax of X -> x; Y -> y)
  CHash -> \(Coords ix) -> fromIntegral . murmur3 (fromIntegral seed) . unTagged . from @String @(Encoding.UTF_8 ByteString) . show $ ix
  CPerlin -> \s o k p ->
    let noise = perlin (fromIntegral s) (fromIntegral o) k p
        sample (i, j) = noiseValue noise (fromIntegral i / 2, fromIntegral j / 2, 0)
     in \(Coords ix) -> sample ix
  CFI -> fromInteger
  COver -> (<!>)
  CIMap -> \wx wy a c -> a (locToCoords (Location (fromIntegral (wx c)) (fromIntegral (wy c))))
  K -> const
  S -> (<*>)
  I -> id
  B -> (.)
  C -> flip
  Φ -> liftA2
