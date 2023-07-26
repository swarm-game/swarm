{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A data type to represent system failures.
--
-- These failures are often not fatal and serve
-- to create common infrastructure for logging.
module Swarm.Game.Failure where

import Data.Text (Text)
import Data.Yaml (ParseException)
import Swarm.Util (showLowT)

data SystemFailure
  = AssetNotLoaded Asset FilePath LoadingFailure
  | ScenarioNotFound FilePath
  | CustomFailure Text

data AssetData = AppAsset | NameGeneration | Entities | Recipes | Scenarios | Script
  deriving (Eq, Show)

prettyAssetData :: AssetData -> Text
prettyAssetData NameGeneration = "name generation data"
prettyAssetData AppAsset = "data assets"
prettyAssetData d = showLowT d

data Asset = Achievement | Data AssetData | History | Save
  deriving (Show)

prettyAsset :: Asset -> Text
prettyAsset (Data ad) = prettyAssetData ad
prettyAsset a = showLowT a

data Entry = Directory | File
  deriving (Eq, Show)

data LoadingFailure
  = DoesNotExist Entry
  | EntryNot Entry
  | CanNotParse ParseException
  | Duplicate AssetData Text
  | CustomMessage Text
