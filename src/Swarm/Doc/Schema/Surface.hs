{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- There are no modern, comprehensive, JSON Schema parsing
-- libraries in Haskell, as explained in this post:
-- https://dev.to/sshine/a-review-of-json-schema-libraries-for-haskell-321
--
-- Therefore, a parser for a small, custom subset of JSON Schema is implemented here,
-- simply for rendering Markdown documentation from Swarm's schema.
module Swarm.Doc.Schema.Surface where

import Data.Aeson
import Data.List.Extra (replace)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import System.FilePath (takeBaseName)
import Swarm.Doc.Schema.Refined

schemaJsonOptions :: Options
schemaJsonOptions =
  defaultOptions
    { fieldLabelModifier = replace "S" "$" . tail -- drops leading underscore
    }

data SchemaRaw = SchemaRaw
  { _description :: Maybe Text
  , _default :: Maybe Value
  , _title :: Maybe Text
  , _type :: Maybe (SingleOrList Text)
  , _properties :: Maybe (Map Text SwarmSchema)
  , _items :: Maybe Value
  , _examples :: Maybe [Value]
  , _Sref :: Maybe Text
  , _oneOf :: Maybe [Value]
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON SchemaRaw where
  parseJSON =
    genericParseJSON schemaJsonOptions

-- | A subset of all JSON schemas, conforming to internal Swarm conventions.
--
-- TODO: Conveniently, this extra layer of processing
-- is able to enforce that all "object" definitions in the schema
-- contain the @additionalProperties: false@ property.
data SwarmSchema = SwarmSchema {
    schemaType :: SchemaType
  , defaultValue :: Maybe Value
  , items :: Maybe Value
  , description :: Maybe Text
  , properties :: Maybe (Map Text SwarmSchema)
  , examples :: Maybe [Value]
  }
  deriving (Eq, Ord, Show)

instance FromJSON SwarmSchema where
  parseJSON x = do

    rawSchema :: rawSchema <- parseJSON x
    let maybeType =
              (Reference . T.pack . takeBaseName . T.unpack <$> _Sref rawSchema)
          <|> (Simple <$> _type rawSchema)
          <|> (Alternates <$> _oneOf rawSchema)

    theType <- maybe (fail "Unspecified sub-schema type") return maybeType

    return $ SwarmSchema {
        schemaType = theType
      , defaultValue = _default rawSchema
      , items = _items rawSchema
      , description = _description rawSchema
      , examples = _examples rawSchema
      , properties = _properties rawSchema
      }