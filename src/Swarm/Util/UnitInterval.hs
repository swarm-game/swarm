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

import Data.List.NonEmpty (NonEmpty, (!!))
import Prelude hiding ((!!))

newtype UnitInterval a = UnitInterval
  { getValue :: a
  }

-- | Guarantees that the stored interval falls on the range
--   @[0, 1]@. It is up to clients to ensure that the promotion
--   to this type is lossless.
mkInterval :: (Ord a, Num a) => a -> UnitInterval a
mkInterval = UnitInterval . max 0 . min 1

-- | Since '(!!)' is partial, here is "proof" that it is safe:
-- If "alpha" is its maximum value of @1@, then the maximum value
-- of the computed index shall be one less than the length of the
-- list (i.e., a valid index).
--
-- See also: 'Swarm.Util.indexWrapNonEmpty'.
safeIndex ::
  RealFrac a =>
  -- | alpha
  UnitInterval a ->
  NonEmpty b ->
  b
safeIndex (UnitInterval alpha) xs =
  xs !! floor (alpha * fromIntegral (length xs - 1))
