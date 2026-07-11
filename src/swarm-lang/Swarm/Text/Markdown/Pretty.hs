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
import Swarm.Pretty (
  PrettyPrec (..),
  prettyText,
 )
import Swarm.Text.Markdown.Document

-- | Convert a single 'Node' to Markdown format.
nodeToMark :: PrettyPrec a => Node a -> Text
nodeToMark = \case
  LeafText a t -> foldl attr t a
  LeafRaw a c ->
    mconcat $
      wrap "`" c : ["{=" <> T.pack a <> "}" | not (null a)]
  LeafCode c -> wrap "`" (prettyText c)
  LeafCodeBlock f c -> codeBlock f $ prettyText c
  LeafLink dest title desc -> between "[" "]" (foldMap nodeToMark desc) <> between "(" ")" (mkTarget dest title)
 where
  codeBlock f t = wrap "```" $ T.pack f <> "\n" <> t <> "\n"
  mkTarget dest title = dest <> maybe "" ((" " <>) . wrap "\"") title
  wrap c t = c <> t <> c
  between x y t = x <> t <> y
  attr t a = case a of
    Emphasis -> wrap "_" t
    Strong -> wrap "**" t
    -- Raw, Code, and Link attributes won't actually occur in a LeafText
    -- node, LeafRaw, LeafCode, and LeafLink will be used instead.
    Raw _ -> t
    Code -> t
    Link {} -> t

-- | Convert a 'Paragraph' to Markdown format.
paragraphToMark :: PrettyPrec a => Paragraph a -> Text
paragraphToMark = \case
  SimpleParagraph ns -> foldMap nodeToMark ns
  ListParagraph _ty _sp _ds -> undefined

-- | Convert a 'Document' to Markdown format.
docToMark :: PrettyPrec a => Document a -> Text
docToMark = T.intercalate "\n\n" . map paragraphToMark . paragraphs

-- | Convert a 'Document' to JSON by reserializing it to Markdown format.
instance (PrettyPrec (Anchor (ImportPhaseFor phase)), Unresolvable (ImportPhaseFor phase)) => ToJSON (Document (Syntax phase)) where
  toJSON = String . docToMark
