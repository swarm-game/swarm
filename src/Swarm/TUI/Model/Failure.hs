{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Swarm.Game.Failure
-- Copyright   :  Ondřej Šebek
-- Maintainer  :  ondras98@icloud.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A data type to represent system failures.
--
-- These failures are often not fatal and serve
-- to create common infrastructure for logging.
module Swarm.TUI.Model.Failure where

import Data.Char (toLower)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml (ParseException, prettyPrintParseException)

data SystemFailure
  = AssetNotLoaded Asset PathLoadFailure

data PathLoadFailure
  = PathLoadFailure FilePath LoadingFailure

data AssetData = AppAsset | NameGeneration | Entities | Recipes | Scenarios
  deriving (Eq, Show)

data Asset = Achievement | Data AssetData | History | Save
  deriving (Eq, Show)

data Entry = Directory | File
  deriving (Eq, Show)

data LoadingFailure
  = DoesNotExist Entry
  | EntryNot Entry
  | CanNotParse ParseException
  | CustomMessage Text

tShowLow :: Show a => a -> Text
tShowLow = T.pack . map toLower . show

tShow :: Show a => a -> Text
tShow = T.pack . show

prettyPathLoadingFailure :: PathLoadFailure -> Text
prettyPathLoadingFailure (PathLoadFailure p x) =
  T.unwords
    [ "For path"
    , T.pack p <> ","
    , prettyLoadingFailure x
    ]

prettyLoadingFailure :: LoadingFailure -> Text
prettyLoadingFailure = \case
  DoesNotExist e -> "The " <> tShowLow e <> " is missing!"
  EntryNot e -> "The entry is not a " <> tShowLow e <> "!"
  CanNotParse p -> "Parse failure:\n" <> T.pack (indent 8 $ prettyPrintParseException p)
  CustomMessage m -> m
 where
  indent n = unlines . map (replicate n ' ' ++) . lines

prettyFailure :: SystemFailure -> Text
prettyFailure = \case
  AssetNotLoaded a (PathLoadFailure fp l) ->
    T.unwords ["Failed to acquire", tShowLow a, tShow fp] <> ": " <> prettyLoadingFailure l
