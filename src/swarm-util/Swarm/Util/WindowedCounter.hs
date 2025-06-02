{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Sliding window for activity monitoring.
module Swarm.Util.WindowedCounter (
  WindowedCounter,
  Offsettable (..),

  -- * Construction
  mkWindow,

  -- * Querying
  getOccupancy,

  -- * Maintenance
  insert,
  discardGarbage,
) where

import Data.Aeson
import Data.Set (Set)
import Data.Set qualified as Set
import Swarm.Util.UnitInterval
import Prelude hiding (length)

-- | Values that can be offset by an integral amount
class Offsettable a where
  offsetBy :: Int -> a -> a

-- | A "sliding window" of a designated span that supports insertion
-- of tick "timestamps" that represent some state of interest during that tick.
-- This data structure supports efficient querying of the ratio of
-- {ticks for which that state existed}
-- to the
-- {total number of ticks spanned by the window}.
--
-- The primary use case is in displaying the "activity level" of a robot.
--
-- == Efficiency considerations
--
-- The data retention of the window shall be maintained externally by
-- invoking the 'discardGarbage' function. However, we should not
-- unconditionally invoke this function upon each game tick.
--
-- For efficiency, we do not want to iterate over every robot
-- upon every tick; we only want to "visit" a robot if it is actually
-- doing work that tick.
-- Because of this, there may be some ticks in which the oldest element
-- that is still stored falls outside of the nominal retention window
-- while a robot is inactive.
--
-- One might think we could perform garbage collection whenever we execute queries.
-- However, in the context in which the view powered by the query is generated, we
-- are not permitted to mutate the "state" of the game
-- (the type signature of the rendering function is @AppState -> Widget Name@).
--
-- Therefore, when we perform "queries" on the window, we must apply some
-- filtering to exclude the "stragglers"; data members that have already fallen outside
-- the window but have not yet been "garbage collected".
-- We use a 'Set' to allow this filtering to be performed in @O(log n)@ time.
--
-- In the worst case, the entire dataset may "age out" without being garbage collected,
-- so that an @O(log n)@ filtering operation might be performed upon every "frame refresh"
-- of the UI view.
-- However, we also store the largest element of the window separately from the 'Set' so that
-- we can compare against it for a @O(1)@ short-circuited path once every member ages out.
--
-- The maximum number of elements ever stored in the 'Set' will be the width of the nominal
-- span, even after some protracted failure to "garbage collect".
data WindowedCounter a = WindowedCounter
  { _members :: Set a
  , _lastLargest :: Maybe a
  -- ^ NOTE: It is possible that '_lastLargest' may not exist in the 'Set'.
  , _nominalSpan :: Int
  -- ^ Data retention window. This value is guaranteed positive by the smart constructor.
  }
  deriving (Eq, Show)

-- | Automatically deriving 'FromJSON' circumvents the protection offered by "smart constructors",
-- and the 'ToJSON' instance may expose internal details.
-- Therefore, we write our own custom implementations.
--
-- This 'ToJSON' instance is strictly for diagnostic purposes, and we can reveal
-- a bit more information than is used for parsing.
instance (ToJSON a) => ToJSON (WindowedCounter a) where
  toJSON (WindowedCounter membersSet _lastLargest nominalSpan) =
    object
      [ "members" .= toJSON membersSet
      , "span" .= nominalSpan
      ]

-- | We discard any "internal state" revealed by the 'ToJSON' instance and
-- just use the "span" so that we can rely on any guarantees offered by the
-- smart constructor, no matter the origin of the JSON.
--
-- Discarding the internal state is OK, because it is not integral to gameplay;
-- it is merely informational as a live indicator in the UI.
instance FromJSON (WindowedCounter a) where
  parseJSON = withObject "WindowedCounter" $ \v -> do
    s <- v .: "span"
    return $ mkWindow s

-- | NOTE: We take the absolute value of the "window span" argument
-- so that we can make guarantees about the output of 'getOccupancy'.
mkWindow ::
  -- | window span
  Int ->
  WindowedCounter a
mkWindow = WindowedCounter Set.empty Nothing . abs

-- | Return the ratio of {members in the window} to the {integral span
-- represented by the window}.
--
-- The "current time" should be at least as large as the largest
-- element of the window.
--
-- A fully-contiguous collection of ticks would have an occupancy ratio of @1@.
--
-- == Unit interval guarantee
-- The returned ratio is /guaranteed/ to lie on the unit interval, because:
--
-- * Both the numerator and denominator of the ratio are guaranteed positive, and
-- * 'discardGarbage' guarantees that the set size is less than or equal to
--   the nominal span.
getOccupancy ::
  (Ord a, Offsettable a) =>
  -- | current time
  a ->
  WindowedCounter a ->
  UnitInterval Double
getOccupancy currentTime wc@(WindowedCounter s lastLargest nominalSpan) =
  mkInterval $
    if Set.null s || maybe False (< referenceTick) lastLargest
      then 0
      else fromIntegral (Set.size culledSet) / fromIntegral nominalSpan
 where
  referenceTick = offsetBy (negate nominalSpan) currentTime
  -- Cull the window according to the current time
  WindowedCounter culledSet _ _ = discardGarbage currentTime wc

-- | Invocations of this function shall be guarded externally
-- by the conditions meant to be tracked in the window.
--
-- Proper usage dictates that the value inserted should always
-- be at least as large as the current largest element of the set.
--
-- The 'discardGarbage' function is called from inside this function
-- so that maintenance of the data structure is simplified.
insert ::
  (Ord a, Offsettable a) =>
  -- | current time
  a ->
  WindowedCounter a ->
  WindowedCounter a
insert x (WindowedCounter s lastLargest nominalSpan) =
  discardGarbage x $ WindowedCounter (Set.insert x s) newLargest nominalSpan
 where
  newLargest = Just $! maybe x (max x) lastLargest

-- | Drop the leading elements that are not larger than the cutoff.
--
-- This function is already called by the 'insert' function, so clients
-- do not necessarily ever have to call this directly.
-- However, there may
-- be opportunity to call this even more often, i.e. in code paths where the
-- robot is visited but the condition for insertion is not met.
--
-- == Invariant
-- If the largest member of the set is the current time,
-- then after calling this function, the difference between smallest and largest
-- value in the set is strictly less than the "nominal span", and the size of the
-- set is less than or equal to the nominal span.
--
-- For example, if the nominal span is @3@, the current time is @7@, and the
-- set entails a contiguous sequence @{2, 3, 4, 5, 6, 7}@, then the pivot for 'Set.split' will be
-- @7 - 3 = 4@. The set becomes @{5, 6, 7}@, with cardinality equal to the nominal span.
discardGarbage ::
  (Ord a, Offsettable a) =>
  -- | current time
  a ->
  WindowedCounter a ->
  WindowedCounter a
discardGarbage currentTime (WindowedCounter s lastLargest nominalSpan) =
  WindowedCounter larger lastLargest nominalSpan
 where
  -- NOTE: Neither output set of 'split' includes the "pivot" value.
  (_smaller, larger) = Set.split (offsetBy (negate nominalSpan) currentTime) s
