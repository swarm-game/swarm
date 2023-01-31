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
module Swarm.Game.Util.Failure where

import Data.Char (toLower)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml (ParseException, prettyPrintParseException)

data SystemFailure
  = AssetNotLoaded Asset FilePath LoadingFailure

data Asset = Achievement | Data | History | Save
  deriving (Eq, Show)

data Entry = Directory | File
  deriving (Eq, Show)

data LoadingFailure
  = DoesNotExist Entry
  | EntryNot Entry
  | CanNotParse ParseException

tShowLow :: Show a => a -> Text
tShowLow = T.pack . map toLower . show

tShow :: Show a => a -> Text
tShow = T.pack . show

prettyLoadingFailure :: LoadingFailure -> Text
prettyLoadingFailure = \case
  DoesNotExist e -> "The " <> tShowLow e <> " is missing!"
  EntryNot e -> "The entry is not a " <> tShowLow e <> "!"
  CanNotParse p -> "Parse failure:\n" <> T.pack (indent 8 $ prettyPrintParseException p)
 where
  indent n = unlines . map (replicate n ' ' ++) . lines

prettyFailure :: SystemFailure -> Text
prettyFailure = \case
  AssetNotLoaded a fp l -> T.unwords ["Failed to acquire", tShowLow a, tShow fp] <> ": " <> prettyLoadingFailure l
