-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Coach where

import Data.Aeson
import Data.Set (Set)
import GHC.Generics (Generic)

data TutorialConcept
  = Sensing
  | ErrorHandling
  | Recursion
  | CodeReuse
  | Movement
  | EntityManagement
  | BuildingRobots
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- |
-- Intended primarily for Tutorials, though other challenge scenarios
-- may also supply these annotations.
-- This record might eventually contain other metadata like
-- "difficulty" or "par time".

{- HLINT ignore "Use newtype instead of data" -}
data Pedagogy = Pedagogy
  { concepts :: Set TutorialConcept
  }
  deriving (Show, Generic, FromJSON, ToJSON)
