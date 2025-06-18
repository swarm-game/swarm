{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.ProtoCell (
  SignpostableCell (..),
  StructureMarker (..),
) where

import Control.Applicative ((<|>))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Map (Map, fromList, toList)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Game.Scenario.Topography.Navigation.Waypoint (WaypointConfig)
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Game.Scenario.Topography.Structure.Named (StructureName)
import Swarm.Util (quote)
import Swarm.Util.Yaml

data StructureMarker = StructureMarker
  { name :: StructureName
  , orientation :: Maybe Orientation
  }
  deriving (Eq, Show, Generic, FromJSON)

-- | Supplements a cell with waypoint and/or structure placement information
data SignpostableCell c = SignpostableCell
  { waypointCfg :: Maybe WaypointConfig
  , structureMarker :: Maybe StructureMarker
  , standardCell :: c
  }
  deriving (Eq, Show)

instance (FromJSONE e a) => FromJSONE e (SignpostableCell a) where
  parseJSONE x =
    withObjectE "SignpostableCell" objParse x
      <|> (SignpostableCell Nothing Nothing <$> parseJSONE x)
   where
    objParse v = do
      waypointCfg <- liftE $ v .:? "waypoint"
      structureMarker <- liftE $ v .:? "structure"
      standardCell <- v ..: "cell"
      pure $ SignpostableCell {..}
