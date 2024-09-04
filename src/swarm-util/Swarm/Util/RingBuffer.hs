-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A rolling window of items for the purpose of maintaining
-- a bounded-size debugging log of recent events.
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
import Data.Foldable (toList)

-- | Isomorphic to the 'Maybe' type
data BufferSize = Infinite | Finite Int

mkRingBuffer :: BufferSize -> RingBuffer a
mkRingBuffer = RingBuffer mempty

data RingBuffer a = RingBuffer (Seq a) BufferSize

instance (ToJSON a) => ToJSON (RingBuffer a) where
  toJSON (RingBuffer xs _) = toJSON xs

instance ToSample (RingBuffer a) where
  toSamples _ = SD.noSamples

instance (Show a) => Show (RingBuffer a) where
  show rb = show $ toList $ getValues rb

instance Eq (RingBuffer a) where
  _ == _ = True

getValues :: RingBuffer a -> Seq a
getValues (RingBuffer xs _) = xs

insert :: a -> RingBuffer a -> RingBuffer a
insert x (RingBuffer xs lim) = RingBuffer inserted lim
 where
  inserted = popped :|> x
  popped = case lim of
    Infinite -> xs
    Finite maxSize -> case xs of
      Empty -> xs
      _ :<| back ->
        if S.length xs < maxSize
          then xs
          else back
