{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Orphan JSON instances for Location and Heading

-- |
-- Module      :  Swarm.Util.Location
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Locations and headings.
module Swarm.Util.Location (Location, pattern Location, Heading, origin, Affine (..)) where

import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.Int (Int32)
import Data.Yaml (FromJSON (parseJSON), ToJSON (toJSON))
import Linear (V2 (..))
import Linear.Affine (Affine (..), Point (..), origin)

-- | A Location is a pair of (x,y) coordinates, both up to 32 bits.
--   The positive x-axis points east and the positive y-axis points
--   north.
type Location = Point V2 Int32

-- | A convenient way to pattern-match on 'Location' values.
pattern Location :: Int32 -> Int32 -> Location
pattern Location x y = P (V2 x y)

{-# COMPLETE Location #-}

-- | A @Heading@ is a 2D vector, with 32-bit coordinates.
--
--   'Location' and 'Heading' are both represented using types from
--   the @linear@ package, so they can be manipulated using a large
--   number of operators from that package.  For example:
--
--   * Two headings can be added with '^+^'.
--   * The difference between two 'Location's is a 'Heading' (via '.-.').
--   * A 'Location' plus a 'Heading' is another 'Location' (via '.^+').
type Heading = V2 Int32

deriving instance ToJSON (V2 Int32)
deriving instance FromJSON (V2 Int32)

deriving instance FromJSONKey (V2 Int32)
deriving instance ToJSONKey (V2 Int32)

instance FromJSON Location where
  parseJSON = fmap P . parseJSON

instance ToJSON Location where
  toJSON (P v) = toJSON v
