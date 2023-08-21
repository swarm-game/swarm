-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Motivation:
-- https://dev.to/sshine/a-review-of-json-schema-libraries-for-haskell-321
module Swarm.Doc.Schema where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Map (Map)

schemaJsonOptions :: Options
schemaJsonOptions =
  defaultOptions
    { fieldLabelModifier = tail -- drops leading underscore
    }

data Prop = Prop {
    _type :: Text
  , _floop :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Prop where
  parseJSON = genericParseJSON schemaJsonOptions

data Schema = Schema {
    description :: Maybe Text
  , properties :: Map Text Prop
  } deriving (Eq, Ord, Show, Generic, FromJSON)