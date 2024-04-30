-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Config data required by initializers of the
-- 'Swarm.Game.State.GameState' record and its subrecords.
module Swarm.Game.State.Config where

import Data.Map (Map)
import Data.Text (Text)
import Swarm.Game.ResourceLoading (NameGenerator)
import Swarm.Game.Scenario (GameStateInputs)

-- | Record to pass information needed to create an initial
--   'GameState' record when starting a scenario.
data GameStateConfig = GameStateConfig
  { initAppDataMap :: Map Text Text
  , nameParts :: NameGenerator
  -- ^ Lists of words/adjectives for use in building random robot names.
  , initState :: GameStateInputs
  }
