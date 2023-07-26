{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Pretty-printing failure messages
module Swarm.Game.Failure.Render where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml (prettyPrintParseException)
import Swarm.Game.Failure
import Swarm.Util (quote, showLowT)
import Witch (into)

prettyLoadingFailure :: LoadingFailure -> Text
prettyLoadingFailure = \case
  DoesNotExist e -> "The " <> showLowT e <> " is missing!"
  EntryNot e -> "The entry is not a " <> showLowT e <> "!"
  CanNotParse p -> "Parse failure:\n" <> into @Text (indent 8 $ prettyPrintParseException p)
  Duplicate thing duped -> "Duplicate " <> showLowT thing <> ": " <> quote duped
  CustomMessage m -> m
 where
  indent n = unlines . map (replicate n ' ' ++) . lines

prettyFailure :: SystemFailure -> Text
prettyFailure = \case
  AssetNotLoaded a fp l ->
    T.unwords ["Failed to acquire", prettyAsset a, "from path", quote $ into @Text fp] <> ": " <> prettyLoadingFailure l
  ScenarioNotFound s ->
    "Scenario not found: " <> into @Text s
  CustomFailure m -> m
