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
  OrderFileWarning (..),
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml (ParseException, prettyPrintParseException)
import Prettyprinter (Pretty (pretty), nest, squotes, vcat, (<+>))
import Swarm.Language.Pretty
import Swarm.Util (showLowT)
import Witch (into)

------------------------------------------------------------
-- Failure descriptions

data AssetData = AppAsset | NameGeneration | Entities | Recipes | Scenarios | Script
  deriving (Eq, Show)

data Asset = Achievement | Data AssetData | History | Save
  deriving (Eq, Show)

data Entry = Directory | File
  deriving (Eq, Show)

data LoadingFailure
  = DoesNotExist Entry
  | EntryNot Entry
  | CanNotParse ParseException
  | Duplicate AssetData Text
  | CustomMessage Text
  deriving (Show)

data OrderFileWarning
  = NoOrderFile
  | MissingFiles (NonEmpty FilePath)
  | DanglingFiles (NonEmpty FilePath)
  deriving (Eq, Show)

data SystemFailure
  = AssetNotLoaded Asset FilePath LoadingFailure
  | ScenarioNotFound FilePath
  | OrderFileWarning FilePath OrderFileWarning
  | CustomFailure Text
  deriving (Show)

------------------------------------------------------------
-- Pretty-printing

instance PrettyPrec AssetData where
  prettyPrec _ = \case
    NameGeneration -> "name generation data"
    AppAsset -> "data assets"
    d -> pretty (showLowT d)

instance PrettyPrec Asset where
  prettyPrec _ = \case
    Data ad -> ppr ad
    a -> pretty (showLowT a)

instance PrettyPrec Entry where
  prettyPrec = const . prettyShowLow

instance PrettyPrec LoadingFailure where
  prettyPrec _ = \case
    DoesNotExist e -> "The" <+> ppr e <+> "is missing!"
    EntryNot e -> "The entry is not a" <+> ppr e <> "!"
    CanNotParse p ->
      nest 2 . vcat $
        "Parse failure:"
          : map pretty (T.lines (into @Text (prettyPrintParseException p)))
    Duplicate thing duped -> "Duplicate" <+> ppr thing <> ":" <+> squotes (pretty duped)
    CustomMessage m -> pretty m

instance PrettyPrec OrderFileWarning where
  prettyPrec _ = \case
    NoOrderFile -> "File not found; using alphabetical order"
    MissingFiles missing ->
      ppr . BulletList "Files not listed will be ignored:" $
        map (into @Text) (NE.toList missing)
    DanglingFiles dangling ->
      ppr . BulletList "Some listed files do not exist:" $
        map (into @Text) (NE.toList dangling)

instance PrettyPrec SystemFailure where
  prettyPrec _ = \case
    AssetNotLoaded a fp l ->
      nest 2 . vcat $
        [ "Failed to acquire" <+> ppr a <+> "from path" <+> squotes (pretty fp) <> ":"
        , ppr l
        ]
    ScenarioNotFound s -> "Scenario not found:" <+> pretty s
    OrderFileWarning orderFile w ->
      nest 2 . vcat $
        [ "Warning: while processing" <+> pretty orderFile <> ":"
        , ppr w
        ]
    CustomFailure m -> pretty m
