{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.ProtoCell (
  SignpostableCell (..),
  StructurePalette (..),
  StructureMarker (..),
) where

import Control.Applicative ((<|>))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Map (Map, fromList, toList)
import Data.Text qualified as T
import Data.Tuple (swap)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Game.Scenario.Topography.Navigation.Waypoint (WaypointConfig)
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Util (quote)
import Swarm.Util.Yaml

newtype StructurePalette e = StructurePalette
  {unPalette :: Map Char (SignpostableCell e)}
  deriving (Eq, Show)

instance (FromJSONE e a) => FromJSONE e (StructurePalette a) where
  parseJSONE =
    withObjectE "palette" $ \v -> do
      m <- mapM parseJSONE v
      -- We swap the tuples twice so we can traverse over the second
      -- element of the tuple in between.
      swappedPairs <- mapM (verifyChar . swap) $ toList $ KM.toMap m
      return . StructurePalette . fromList $ map swap swappedPairs
   where
    verifyChar = traverse $ ensureSingleChar . K.toString
    ensureSingleChar [x] = return x
    ensureSingleChar x =
      fail $
        T.unpack $
          T.unwords
            [ "Palette entry is not a single character:"
            , quote $ T.pack x
            ]

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
