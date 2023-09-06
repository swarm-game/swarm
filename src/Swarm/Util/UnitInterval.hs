-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Creation and utilities for tge unit interval
module Swarm.Util.UnitInterval (
  UnitInterval,
  getValue,
  mkInterval,
  safeIndex,
) where

newtype UnitInterval a = UnitInterval
  { getValue :: a
  }

-- | Guarantees that the stored interval falls on the range
--   @[0, 1]@. It is up to clients to ensure that the promotion
--   to this type is lossless.
mkInterval :: (Ord a, Num a) => a -> UnitInterval a
mkInterval = UnitInterval . max 0 . min 1

safeIndex :: RealFrac a => UnitInterval a -> [b] -> b
safeIndex (UnitInterval alpha) xs =
  xs !! floor (alpha * fromIntegral (length xs - 1))
