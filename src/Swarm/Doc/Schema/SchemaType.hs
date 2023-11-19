-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Representation of the "type" of a schema.
module Swarm.Doc.Schema.SchemaType where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath (takeBaseName)

newtype SingleOrList a = SingleOrList
  { getList :: [a]
  }
  deriving (Eq, Ord, Show)

instance (FromJSON a) => FromJSON (SingleOrList a) where
  parseJSON x =
    fmap SingleOrList $
      pure <$> parseJSON x <|> parseJSON x

data SchemaType
  = -- | A basic built-in type
    Simple (SingleOrList Text)
  | -- | Any one of multiple possible schema types
    Alternatives [SchemaType]
  | -- | A reference to a schema defined elsewhere
    Reference SchemaIdReference
  | -- | Members of a list, all of the given schema type
    ListOf SchemaType
  deriving (Eq, Ord, Show)

newtype SchemaIdReference = SchemaIdReference Text
  deriving (Eq, Ord, Show)

fromFilePath :: FilePath -> SchemaIdReference
fromFilePath = SchemaIdReference . T.pack . takeBaseName
