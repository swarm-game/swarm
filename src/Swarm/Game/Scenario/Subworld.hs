{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Subworld where

import Data.Aeson
import Swarm.Game.Entity
import Swarm.Game.Scenario.Portal
import Swarm.Game.Scenario.RobotLookup
import Swarm.Game.Scenario.WorldDescription
import Swarm.Util.Yaml

data Subworld = Subworld
  { name :: SubworldName
  , portals :: [Portal]
  , world :: WorldDescription
  }
  deriving (Eq, Show)

instance FromJSONE EntityMap Subworld where
  parseJSONE = withObjectE "subworld" $ \v -> do
    n <- liftE (v .: "name")
    c <- liftE (v .: "connectivity")
    let rsMap = buildRobotMap []
    w <- localE (,rsMap) (v ..: "world")
    return $ Subworld n c w
