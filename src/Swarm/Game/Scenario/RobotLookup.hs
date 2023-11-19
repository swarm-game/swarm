{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.RobotLookup where

import Control.Lens hiding (from, (<.>))
import Data.Aeson (FromJSON)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Swarm.Game.Entity
import Swarm.Game.Robot (TRobot, trobotName)
import Swarm.Util (failT, quote)
import Swarm.Util.Yaml

------------------------------------------------------------
-- Robot map
------------------------------------------------------------

newtype RobotName = RobotName Text
  deriving (Show, Eq, Ord, Generic, FromJSON)

-- | A robot template paired with its definition's index within
-- the Scenario file
type IndexedTRobot = (Int, TRobot)

-- | A map from names to robots, used to look up robots in scenario
--   descriptions.
type RobotMap = Map RobotName IndexedTRobot

-- | Create a 'RobotMap' from a list of robot templates.
buildRobotMap :: [TRobot] -> RobotMap
buildRobotMap rs = M.fromList $ zipWith (\x y -> (RobotName $ view trobotName y, (x, y))) [0 ..] rs

------------------------------------------------------------
-- Lookup utilities
------------------------------------------------------------

-- | Look up a thing by name, throwing a parse error if it is not
--   found.
getThing :: Show k => Text -> (k -> m -> Maybe a) -> k -> ParserE m a
getThing thing lkup name = do
  m <- getE
  case lkup name m of
    Nothing -> failT ["Unknown", thing, "name:", quote $ T.pack $ show name]
    Just a -> return a

-- | Look up an entity by name in an 'EntityMap', throwing a parse
--   error if it is not found.
getEntity :: Text -> ParserE EntityMap Entity
getEntity = getThing "entity" lookupEntityName

-- | Look up a robot by name in a 'RobotMap', throwing a parse error
--   if it is not found.
getRobot :: RobotName -> ParserE RobotMap IndexedTRobot
getRobot = getThing "robot" M.lookup
