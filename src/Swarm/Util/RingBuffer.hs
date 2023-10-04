-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Creation and utilities for the unit interval
module Swarm.Util.RingBuffer (
  RingBuffer,
  BufferSize (..),
  getValues,
  insert,
  mkRingBuffer,
) where

import Data.Aeson
import Data.Sequence as S
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD

-- | Isomorphic to the 'Maybe' type
data BufferSize = Infinite | Finite Int

mkRingBuffer :: BufferSize -> RingBuffer a
mkRingBuffer = RingBuffer mempty

data RingBuffer a = RingBuffer (Seq a) BufferSize

instance (ToJSON a) => ToJSON (RingBuffer a) where
  toJSON (RingBuffer xs _) = toJSON xs

instance ToSample (RingBuffer a) where
  toSamples _ = SD.noSamples

getValues :: RingBuffer a -> Seq a
getValues (RingBuffer xs _) = xs

insert :: a -> RingBuffer a -> RingBuffer a
insert x (RingBuffer xs lim) = RingBuffer inserted lim
 where
  inserted = x :<| popped
  popped = case lim of
    Infinite -> xs
    Finite maxSize -> case xs of
      Empty -> xs
      front :|> _ ->
        if S.length xs < maxSize
          then xs
          else front
