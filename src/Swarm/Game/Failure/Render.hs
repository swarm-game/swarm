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

prettyAssetData :: AssetData -> Text
prettyAssetData NameGeneration = "name generation data"
prettyAssetData AppAsset = "data assets"
prettyAssetData d = showLowT d

prettyAsset :: Asset -> Text
prettyAsset (Data ad) = prettyAssetData ad
prettyAsset a = showLowT a

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
  OrderFileWarning orderFile w ->
    "Warning: while processing " <> into @Text orderFile <> ":\n" <> T.unlines (prettyOrderFileWarning w)
  CustomFailure m -> m

prettyOrderFileWarning :: OrderFileWarning -> [Text]
prettyOrderFileWarning = \case
  NoOrderFile -> ["  File not found; using alphabetical order"]
  MissingFiles missing ->
    "  Files not listed will be ignored:"
      : map (("  - " <>) . into @Text) missing
  DanglingFiles dangling ->
    "  Some listed files do not exist:"
      : map (("  - " <>) . into @Text) dangling
