-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- There are no modern, comprehensive JSON Schema parsing
-- libraries in Haskell, as explained in
-- <this post https://dev.to/sshine/a-review-of-json-schema-libraries-for-haskell-321>.
--
-- Therefore, a bespoke parser for a small subset of JSON Schema is implemented here,
-- simply for rendering Markdown documentation from Swarm's schema.
module Swarm.Doc.Schema.Parse where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Swarm.Doc.Schema.Refined
import Text.Pandoc

-- | Includes everything needed to
-- render the schema to markdown
data SchemaData = SchemaData
  { schemaPath :: FilePath
  , schemaContent :: ToplevelSchema
  , markdownFooters :: [Pandoc]
  }

data Members
  = ObjectProperties (Map Text SwarmSchema)
  | ListMembers (ItemDescription SwarmSchema)
  | EnumMembers (NonEmpty Text)
  deriving (Eq, Ord, Show)

data ToplevelSchema = ToplevelSchema
  { title :: Text
  , description :: Maybe Pandoc
  , content :: SwarmSchema
  , members :: Maybe Members
  , footerPaths :: [FilePath]
  }
  deriving (Eq, Ord, Show)

instance FromJSON ToplevelSchema where
  parseJSON x = do
    rawSchema :: rawSchema <- parseJSON x
    swarmSchema <- toSwarmSchema rawSchema

    theTitle <- maybe (fail "Schema requires a title") return $ _title rawSchema
    let theFooters = fromMaybe [] $ _footers rawSchema
        maybeMembers =
          ObjectProperties <$> properties swarmSchema
            <|> ListMembers <$> itemsDescription swarmSchema
            <|> EnumMembers <$> _enum rawSchema
    return $ ToplevelSchema theTitle (objectDescription swarmSchema) swarmSchema maybeMembers theFooters
