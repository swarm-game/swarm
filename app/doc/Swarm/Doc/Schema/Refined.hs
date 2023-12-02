{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Refined JSON schema after converting
-- all JSON Value types to their specific sum types
module Swarm.Doc.Schema.Refined where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.List.Extra (replace)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Swarm.Doc.Schema.SchemaType
import System.FilePath (takeBaseName)
import Text.Pandoc
import Text.Pandoc.Builder

-- * Basic

schemaJsonOptions :: Options
schemaJsonOptions =
  defaultOptions
    { fieldLabelModifier = replace "S" "$" . drop 1 -- drops leading underscore
    }

-- | A single record that encompasses all possible objects
-- in a JSON schema. All fields are optional.
data SchemaRaw = SchemaRaw
  { _description :: Maybe Text
  , _default :: Maybe Value
  , _title :: Maybe Text
  , _type :: Maybe (SingleOrList Text)
  , _name :: Maybe Text
  , _properties :: Maybe (Map Text SwarmSchema)
  , _items :: Maybe (ItemDescription SwarmSchema)
  , _examples :: Maybe [Value]
  , _Sref :: Maybe Text
  , _oneOf :: Maybe [SchemaRaw]
  , _footers :: Maybe [FilePath]
  , _additionalProperties :: Maybe Bool
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON SchemaRaw where
  parseJSON = genericParseJSON schemaJsonOptions

extractSchemaType :: SchemaRaw -> Maybe SchemaType
extractSchemaType rawSchema =
  mkReference <$> _Sref rawSchema
    <|> getTypeFromItems
    <|> Simple <$> _type rawSchema
    <|> Alternatives . mapMaybe extractSchemaType <$> _oneOf rawSchema
 where
  mkReference = Reference . SchemaIdReference . T.pack . takeBaseName . T.unpack

  getTypeFromItems :: Maybe SchemaType
  getTypeFromItems = do
    itemsThing <- _items rawSchema
    case itemsThing of
      ItemList _ -> Nothing
      ItemType x -> Just $ ListOf $ schemaType x

-- * Refined

data ItemDescription a
  = ItemList [a]
  | ItemType a
  deriving (Eq, Ord, Show)

instance (FromJSON a) => FromJSON (ItemDescription a) where
  parseJSON x =
    ItemList <$> parseJSON x
      <|> ItemType <$> parseJSON x

getSchemaReferences :: SchemaType -> [SchemaIdReference]
getSchemaReferences = \case
  Simple _ -> []
  Alternatives xs -> concatMap getSchemaReferences xs
  Reference x -> pure x
  ListOf x -> getSchemaReferences x

-- | A subset of all JSON schemas, conforming to internal Swarm conventions.
--
-- Conveniently, this extra representation layer
-- is able to enforce (via 'toSwarmSchema') that all "object"
-- definitions in the schema contain the @"additionalProperties": true@ attribute.
data SwarmSchema = SwarmSchema
  { schemaType :: SchemaType
  , defaultValue :: Maybe Value
  , objectDescription :: Maybe Pandoc
  , properties :: Maybe (Map Text SwarmSchema)
  , itemsDescription :: Maybe (ItemDescription SwarmSchema)
  , examples :: [Value]
  }
  deriving (Eq, Ord, Show)

instance FromJSON SwarmSchema where
  parseJSON x = do
    rawSchema :: rawSchema <- parseJSON x
    toSwarmSchema rawSchema

getMarkdown :: MonadFail m => Text -> m Pandoc
getMarkdown desc = case runPure (readMarkdown def desc) of
  Right d -> return d
  Left err -> fail $ T.unpack $ renderError err

toSwarmSchema :: MonadFail m => SchemaRaw -> m SwarmSchema
toSwarmSchema rawSchema = do
  theType <- maybe (fail "Unspecified sub-schema type") return maybeType
  markdownDescription <- mapM getMarkdown $ _description rawSchema

  if null (_properties rawSchema) || not (fromMaybe True (_additionalProperties rawSchema))
    then return ()
    else fail "All objects must specify '\"additionalProperties\": true'"

  return
    SwarmSchema
      { schemaType = theType
      , defaultValue = _default rawSchema
      , objectDescription = markdownDescription <|> doc . plain . text <$> _name rawSchema
      , examples = fromMaybe [] $ _examples rawSchema
      , properties = _properties rawSchema
      , itemsDescription = _items rawSchema
      }
 where
  maybeType = extractSchemaType rawSchema

-- * Utilities

-- | Recursively extract references to other schemas
extractReferences :: SwarmSchema -> Set SchemaIdReference
extractReferences s = thisRefList <> otherRefLists
 where
  thisRefList = Set.fromList . getSchemaReferences $ schemaType s

  otherSchemas = maybe [] M.elems $ properties s
  otherRefLists = Set.unions $ map extractReferences otherSchemas
