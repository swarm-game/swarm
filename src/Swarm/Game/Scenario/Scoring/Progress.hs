module Swarm.Game.Scenario.Scoring.Progress where

-- | This parallels the "ScenarioStatus" type,
-- but does not need a "NotStarted" member.
data Progress
  = Ongoing
  | Completed

data Metric a = Metric Progress a

-- | This encodes the notion of "more play is better"
-- for incomplete games (rationale: more play = more fun),
--  whereas "smaller inputs are better" for completed games.
instance (Ord a) => Semigroup (Metric a) where
  Metric Ongoing x <> Metric Ongoing y = Metric Ongoing $ max x y
  Metric Completed x <> Metric Completed y = Metric Completed $ min x y
  Metric Ongoing _ <> Metric Completed y = Metric Completed y
  Metric Completed x <> Metric Ongoing _ = Metric Completed x
