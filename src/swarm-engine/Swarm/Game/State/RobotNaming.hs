{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Robot naming sub-state.
module Swarm.Game.State.RobotNaming (
  RobotNaming,
  initRobotNaming,

  -- ** Lenses
  nameGenerator,
  gensym,
) where

import Control.Lens (Getter, Lens')
import Swarm.Game.State.Config (GameStateConfig (nameParts))
import Swarm.ResourceLoading (NameGenerator)
import Swarm.Util.Lens (makeLensesNoSigs)

-- | Sub-state used for assigning names and unique IDs to robots.
data RobotNaming = RobotNaming
  { _nameGenerator :: NameGenerator
  , _gensym :: Int
  }

-- | Initialize robot naming from configuration.
initRobotNaming :: GameStateConfig -> RobotNaming
initRobotNaming gsc =
  RobotNaming
    { _nameGenerator = nameParts gsc
    , _gensym = 0
    }

makeLensesNoSigs ''RobotNaming

--- | Read-only list of words, for use in building random robot names.
nameGenerator :: Getter RobotNaming NameGenerator

-- | A counter used to generate globally unique IDs.
gensym :: Lens' RobotNaming Int
