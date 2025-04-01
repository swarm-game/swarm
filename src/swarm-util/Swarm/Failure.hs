{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A data type to represent system failures (as distinct from robot
-- program failures).
--
-- These failures are often not fatal and serve
-- to create common infrastructure for logging.
module Swarm.Failure (
  SystemFailure (..),
  simpleErrorHandle,
  AssetData (..),
  Asset (..),
  Entry (..),
  LoadingFailure (..),
  OrderFileWarning (..),
) where

import Control.Carrier.Throw.Either (ThrowC (..), runThrow)
import Control.Monad ((<=<))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Data.Yaml (ParseException, prettyPrintParseException)
import Prettyprinter (Pretty (pretty), nest, squotes, vcat, (<+>))
import Swarm.Pretty (BulletList (..), PrettyPrec (..), ppr, prettyShowLow, prettyString)
import Swarm.Util (showLowT)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Witch (into)

------------------------------------------------------------
-- Failure descriptions

-- | Enumeration of various assets we can attempt to load.
data AssetData = AppAsset | NameGeneration | Entities | Terrain | Recipes | Worlds | Scenarios | Script
  deriving (Eq, Show)

-- | Overarching enumeration of various assets we can attempt to load.
data Asset = Achievement | Data AssetData | History | Keybindings | Save
  deriving (Eq, Show)

-- | Enumeration type to distinguish between directories and files.
data Entry = Directory | File
  deriving (Eq, Show)

-- | An error that occurred while attempting to load some kind of asset.
data LoadingFailure
  = DoesNotExist Entry
  | EntryNot Entry
  | CanNotParseYaml ParseException
  | Duplicate AssetData Text
  | SystemFailure SystemFailure
  deriving (Show)

-- ~~~~ Note [Pretty-printing typechecking errors]
--
-- It would make sense to store a CheckErr in DoesNotTypecheck;
-- however, Swarm.Failure is imported in lots of places, and
-- CheckErr can contain high-level things like TTerms etc., so it
-- would lead to an import cycle.  Instead, we choose to just
-- pretty-print typechecking errors before storing them here.

-- | A warning that arose while processing an @00-ORDER.txt@ file.
data OrderFileWarning
  = NoOrderFile
  | MissingFiles (NonEmpty FilePath)
  | DanglingFiles (NonEmpty FilePath)
  deriving (Eq, Show)

-- | An enumeration of various types of failures (errors or warnings)
--   that can occur.
data SystemFailure
  = AssetNotLoaded Asset FilePath LoadingFailure
  | ScenarioNotFound FilePath
  | OrderFileWarning FilePath OrderFileWarning
  | CanNotParseMegaparsec (ParseErrorBundle Text Void)
  | DoesNotTypecheck Text -- See Note [Typechecking errors]
  | CustomFailure Text
  deriving (Show)

------------------------------------------------------------
-- Basic error handling

simpleErrorHandle :: ThrowC SystemFailure IO a -> IO a
simpleErrorHandle = either (fail . prettyString) pure <=< runThrow

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
  prettyPrec _ = prettyShowLow

instance PrettyPrec LoadingFailure where
  prettyPrec prec = \case
    DoesNotExist e -> "The" <+> ppr e <+> "is missing!"
    EntryNot e -> "The entry is not a" <+> ppr e <> "!"
    CanNotParseYaml p ->
      nest 2 . vcat $
        "Parse failure:"
          : map pretty (T.lines (into @Text (prettyPrintParseException p)))
    Duplicate thing duped -> "Duplicate" <+> ppr thing <> ":" <+> squotes (pretty duped)
    SystemFailure g -> prettyPrec prec g

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
    CanNotParseMegaparsec p ->
      nest 2 . vcat $
        "Parse failure:"
          : map pretty (T.lines (into @Text (errorBundlePretty p)))
    DoesNotTypecheck t ->
      nest 2 . vcat $
        "Parse failure:"
          : map pretty (T.lines t)
    CustomFailure m -> pretty m
