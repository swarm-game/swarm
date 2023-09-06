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

import Prelude hiding ((!!))
import Data.List.NonEmpty (NonEmpty, (!!))

newtype UnitInterval a = UnitInterval
  { getValue :: a
  }

-- | Guarantees that the stored interval falls on the range
--   @[0, 1]@. It is up to clients to ensure that the promotion
--   to this type is lossless.
mkInterval :: (Ord a, Num a) => a -> UnitInterval a
mkInterval = UnitInterval . max 0 . min 1

-- | Since '(!!)' is partial, here is "proof" that it is safe:
-- If 'dutyCycleRatio' is 1, then the maximum value of 'dutyCycleAttrIdx' is
--    --   one less than the length of 'meterAttributeNames' (i.e., a valid index).
--
-- See also: 'Swarm.Util.indexWrapNonEmpty'.
safeIndex :: RealFrac a =>
  -- | alpha
  UnitInterval a ->
    NonEmpty b ->
      b
safeIndex (UnitInterval alpha) xs =
  xs !! floor (alpha * fromIntegral (length xs - 1))
