{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Refined JSON schema after converting
-- all JSON Value types to their specific sum types
module Swarm.Doc.Schema.Refined where

import Data.Aeson
import Control.Applicative ((<|>))
import Data.Text (Text)
import Text.Pandoc.Builder
import Data.Text qualified as T

newtype SingleOrList a = SingleOrList {
    getList :: [a]
  } deriving (Eq, Ord, Show)

instance (FromJSON a) => FromJSON (SingleOrList a) where
  parseJSON x = SingleOrList <$> do
    (pure <$> parseJSON x) <|> parseJSON x

type SchemaIdReference = Text

data SchemaType =
    Simple (SingleOrList Text)
  | Reference SchemaIdReference
  | Alternates [Value]
  deriving (Eq, Ord, Show)

fragmentHref :: SchemaIdReference -> Text
fragmentHref = T.cons '#' . T.filter (/= '.'). T.toLower

listToText :: SchemaType -> Inlines
listToText = \case
  Simple xs -> code $ T.intercalate " | " $ getList xs
  Reference x -> link (fragmentHref x) "Link to object properties" $ text $ "Object schema"
  Alternates xs -> code $ T.intercalate " | " $ map (T.pack . show) xs