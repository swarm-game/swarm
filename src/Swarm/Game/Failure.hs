{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A data type to represent system failures.
--
-- These failures are often not fatal and serve
-- to create common infrastructure for logging.
module Swarm.Game.Failure (
  SystemFailure (..),
  AssetData (..),
  Asset (..),
  Entry (..),
  LoadingFailure (..),
  prettyFailure,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml (ParseException, prettyPrintParseException)
import Swarm.Util (quote)

data SystemFailure
  = AssetNotLoaded Asset FilePath LoadingFailure
  deriving (Show)

data AssetData = AppAsset | NameGeneration | Entities | Recipes | Scenarios | Script
  deriving (Eq, Show)

data Asset = Achievement | Data AssetData | History | Save
  deriving (Show, Eq)

data Entry = Directory | File
  deriving (Eq, Show)

data LoadingFailure
  = DoesNotExist Entry
  | EntryNot Entry
  | CanNotParse ParseException
  | CustomMessage Text
  deriving (Show)

tShowLow :: Show a => a -> Text
tShowLow = T.toLower . T.pack . show

tShow :: Show a => a -> Text
tShow = T.pack . show

prettyLoadingFailure :: LoadingFailure -> Text
prettyLoadingFailure = \case
  DoesNotExist e -> "The " <> tShowLow e <> " is missing!"
  EntryNot e -> "The entry is not a " <> tShowLow e <> "!"
  CanNotParse p -> "Parse failure:\n" <> T.pack (indent 8 $ prettyPrintParseException p)
  CustomMessage m -> m
 where
  indent n = unlines . map (replicate n ' ' ++) . lines

-- | Pretty print system failure.
prettyFailure :: SystemFailure -> Text
prettyFailure = \case
  AssetNotLoaded a fp l ->
    T.unwords ["Failed to acquire", tShowLow a, tShow fp, "from path", quote $ T.pack fp] <> ": " <> prettyLoadingFailure l
