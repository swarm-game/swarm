-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Data types and functions applicable across different
-- scoring methods.
module Swarm.Game.Scenario.Scoring.GenericMetrics where

import Data.Aeson
import Data.Ord (Down (Down))
import GHC.Generics (Generic)
import Swarm.Util (maxOn)

-- | This is a subset of the "ScenarioStatus" type
-- that excludes the "NotStarted" case.
data Progress
  = Attempted
  | Completed
  deriving (Eq, Ord, Show, Read, Generic)

untaggedJsonOptions :: Options
untaggedJsonOptions =
  defaultOptions
    { sumEncoding = UntaggedValue
    }

instance FromJSON Progress where
  parseJSON = genericParseJSON untaggedJsonOptions

instance ToJSON Progress where
  toJSON = genericToJSON untaggedJsonOptions

data Metric a = Metric Progress a
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

getMetric :: Metric a -> a
getMetric (Metric _ x) = x

-- | This encodes the notion of "more play is better"
-- for incomplete games (rationale: more play = more fun),
--  whereas "smaller inputs are better" for completed games.
--
-- Since 'Maybe' has its own 'Ord' instance where
-- @Nothing < Just x@ regardless of @x@, when we want to
-- choose the minimum value we @fmap Down@ to ensure that
-- the 'Just' is selected while inverting the ordering of
-- the inner member.
chooseBetter ::
  Ord a =>
  -- | criteria; record field extractor
  (b -> Maybe a) ->
  -- | x
  Metric b ->
  -- | y
  Metric b ->
  Metric b
chooseBetter criteria (Metric Attempted x) (Metric Attempted y) =
  Metric Attempted $ maxOn criteria x y
chooseBetter criteria (Metric Completed x) (Metric Completed y) =
  Metric Completed $ maxOn (fmap Down . criteria) x y
-- Having exhausted the possibilities where either both
-- are Completed or both are Attempted, now we can just
-- choose the Completed one.
chooseBetter _ x@(Metric Completed _) _ = x
chooseBetter _ _ y@(Metric Completed _) = y
