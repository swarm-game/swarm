{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Prety-printing Documents back to Markdown source.
module Swarm.Text.Markdown.Pretty where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml
import Swarm.Language.Phase (ImportPhaseFor)
import Swarm.Language.Syntax (Anchor, Syntax, Unresolvable)
import Swarm.Pretty (PrettyPrec (..))
import Swarm.Text.Markdown.Document (Document, TxtAttr (..))
import Swarm.Text.Markdown.Layout (documentToStream)
import Swarm.Text.Markdown.Token (OutputToken, Token' (..))

openAttrToMark :: TxtAttr -> Text
openAttrToMark = \case
  Strong -> "**"
  Emphasis -> "*"
  Raw _ -> "`"
  Code -> "```\n"
  Link _ _ -> "["

closeAttrToMark :: TxtAttr -> Text
closeAttrToMark = \case
  Strong -> "**"
  Emphasis -> "*"
  Raw "" -> "`"
  Raw ann -> "`{=" <> T.pack ann <> "}"
  Code -> "\n```\n"
  Link dest mtitle -> "](" <> mkTarget dest mtitle <> ")"
 where
  wrap c t = c <> t <> c
  mkTarget dest title = dest <> maybe "" ((" " <>) . wrap "\"") title

streamToMark :: [OutputToken] -> Text
streamToMark = mconcat . go []
 where
  go :: [TxtAttr] -> [OutputToken] -> [Text]
  go _ [] = []
  go attrStack (t : ts) = case t of
    TextToken text -> text : go attrStack ts
    PushAttr a -> openAttrToMark a : go (a : attrStack) ts
    PopAttr -> case attrStack of
      [] -> go attrStack ts
      (a : as) -> closeAttrToMark a : go as ts
    Newline -> "\n" : go attrStack ts
    Para -> "\n\n" : go attrStack ts

-- | Convert a 'Document' to Markdown format.
docToMark :: PrettyPrec a => Document a -> Text
docToMark = streamToMark . documentToStream Nothing

-- | Convert a 'Document' to JSON by reserializing it to Markdown format.
instance (PrettyPrec (Anchor (ImportPhaseFor phase)), Unresolvable (ImportPhaseFor phase)) => ToJSON (Document (Syntax phase)) where
  toJSON = String . docToMark
