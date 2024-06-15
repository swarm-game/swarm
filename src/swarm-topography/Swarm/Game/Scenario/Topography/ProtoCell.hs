{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.ProtoCell (
  SignpostableCell (..),
  StructurePalette (..),
) where

import Control.Applicative ((<|>))
import Data.Aeson.KeyMap (KeyMap)
import Data.Yaml as Y
import Swarm.Game.Scenario.Topography.Navigation.Waypoint (WaypointConfig)
import Swarm.Util.Yaml

newtype StructurePalette e = StructurePalette
  {unPalette :: KeyMap (SignpostableCell e)}
  deriving (Eq, Show)

instance (FromJSONE e a) => FromJSONE e (StructurePalette a) where
  parseJSONE =
    withObjectE "palette" $
      fmap StructurePalette . mapM parseJSONE

-- | Supplements a cell with waypoint information
data SignpostableCell c = SignpostableCell
  { waypointCfg :: Maybe WaypointConfig
  , standardCell :: c
  }
  deriving (Eq, Show)

instance (FromJSONE e a) => FromJSONE e (SignpostableCell a) where
  parseJSONE x =
    withObjectE "SignpostableCell" objParse x
      <|> (SignpostableCell Nothing <$> parseJSONE x)
   where
    objParse v = do
      waypointCfg <- liftE $ v .:? "waypoint"
      standardCell <- v ..: "cell"
      pure $ SignpostableCell {..}
