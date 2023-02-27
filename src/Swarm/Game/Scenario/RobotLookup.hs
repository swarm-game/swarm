{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.RobotLookup where

import Control.Lens hiding (from, (<.>))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Swarm.Game.Entity
import Swarm.Game.Robot (TRobot, trobotName)
import Swarm.Util.Yaml

------------------------------------------------------------
-- Robot map
------------------------------------------------------------

-- | A robot template paired with its definition's index within
-- the Scenario file
type IndexedTRobot = (Int, TRobot)

-- | A map from names to robots, used to look up robots in scenario
--   descriptions.
type RobotMap = Map Text IndexedTRobot

-- | Create a 'RobotMap' from a list of robot templates.
buildRobotMap :: [TRobot] -> RobotMap
buildRobotMap rs = M.fromList $ zipWith (\x y -> (view trobotName y, (x, y))) [0 ..] rs

------------------------------------------------------------
-- Lookup utilities
------------------------------------------------------------

-- | Look up a thing by name, throwing a parse error if it is not
--   found.
getThing :: String -> (Text -> m -> Maybe a) -> Text -> ParserE m a
getThing thing lkup name = do
  m <- getE
  case lkup name m of
    Nothing -> fail $ "Unknown " <> thing <> " name: " ++ show name
    Just a -> return a

-- | Look up an entity by name in an 'EntityMap', throwing a parse
--   error if it is not found.
getEntity :: Text -> ParserE EntityMap Entity
getEntity = getThing "entity" lookupEntityName

-- | Look up a robot by name in a 'RobotMap', throwing a parse error
--   if it is not found.
getRobot :: Text -> ParserE RobotMap IndexedTRobot
getRobot = getThing "robot" M.lookup
