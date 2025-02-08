{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Data types and functions applicable across different
-- scoring methods.
module Swarm.Game.Scenario.Scoring.GenericMetrics (
  Progress (..),
  Metric (Metric),
  metricProgress,
  metricData,
  chooseBetter,
) where

import Control.Lens
import Data.Aeson
import Data.Ord (Down (Down))
import GHC.Generics (Generic)
import Swarm.Util (maxOn)
import Swarm.Util.JSON (optionsUntagged)
import Swarm.Util.Lens (makeLensesNoSigs)

-- | This is a subset of the "ScenarioStatus" type
-- that excludes the "NotStarted" case.
data Progress
  = Attempted
  | Completed
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON Progress where
  parseJSON = genericParseJSON optionsUntagged

instance ToJSON Progress where
  toJSON = genericToJSON optionsUntagged

data Metric a = Metric
  { _metricProgress :: Progress
  , _metricData :: a
  }
  deriving (Eq, Ord, Show, Read, Generic, FromJSON, ToJSON)

makeLensesNoSigs ''Metric

-- | The player progress, so that we know if this game was completed.
metricProgress :: Lens' (Metric a) Progress

-- | Metric data, for example start and end time.
metricData :: Lens' (Metric a) a

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
