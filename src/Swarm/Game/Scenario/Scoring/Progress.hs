module Swarm.Game.Scenario.Scoring.Progress where

import Data.Aeson
import GHC.Generics (Generic)

-- | This parallels the "ScenarioStatus" type,
-- but does not need a "NotStarted" member.
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

-- | Adapted from here:
-- https://hackage.haskell.org/package/ghc-prim-0.9.0/docs/src/GHC.Classes.html#max
maxOn, minOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y = if f x <= f y then y else x
minOn f x y = if f x <= f y then x else y

-- | This encodes the notion of "more play is better"
-- for incomplete games (rationale: more play = more fun),
--  whereas "smaller inputs are better" for completed games.
chooseBetter ::
  Ord a =>
  -- | criteria; record field extractor
  (b -> a) ->
  -- | x
  Metric b ->
  -- | y
  Metric b ->
  Metric b
chooseBetter criteria (Metric Attempted x) (Metric Attempted y) =
  Metric Attempted $ maxOn criteria x y
chooseBetter criteria (Metric Completed x) (Metric Completed y) =
  Metric Completed $ minOn criteria x y
-- Having exhausted the possibilities where either both
-- are Completed or both are Attempted, now we can just
-- choose the Completed one.
chooseBetter _ x@(Metric Completed _) _ = x
chooseBetter _ _ x@(Metric Completed _) = x
