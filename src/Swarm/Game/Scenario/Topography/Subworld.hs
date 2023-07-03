{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Subworld where

import Data.Aeson
import Swarm.Game.Entity
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.Topography.WorldDescription
import Swarm.Util.Yaml
import Swarm.Game.Universe

data Subworld = Subworld
  { name :: SubworldName
  , world :: WorldDescription
  }
  deriving (Eq, Show)

instance FromJSONE EntityMap Subworld where
  parseJSONE = withObjectE "subworld" $ \v -> do
    n <- liftE (v .: "name")
    let rsMap = buildRobotMap []
    w <- localE (,rsMap) (v ..: "world")
    return $ Subworld n w
