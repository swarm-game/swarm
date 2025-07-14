-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Data type for talking about ranges between robots.
module Swarm.Game.State.Range (
  RobotRange (..),
) where

-- | Type for describing how far away a robot is from the base, which
--   determines what kind of communication can take place.
data RobotRange
  = -- | Close; communication is perfect.
    Close
  | -- | Mid-range; communication is possible but lossy.
    MidRange Double
  | -- | Far; communication is not possible.
    Far
  deriving (Eq, Ord)
