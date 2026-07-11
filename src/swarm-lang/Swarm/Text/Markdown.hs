-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for parsing, laying out, and rendering Markdown documents
-- with embedded Swarm code.
--
-- This module is just a convenience module that re-exports some of
-- the most generally useful types and functions.
--
-- Overall, the Markdown processing pipeline looks like this:
--
--   1. Parse Markdown content ("Swarm.Text.Markdown.Parse"), using
--      the Commonmark package, into our custom 'Document' data type
--      ("Swarm.Text.Markdown.Document").  At this point any code
--      blocks contained in the Markdown document are just blobs of
--      text.
--
--   2. Run Swarm parsing + typechecking to validate code blocks or
--      inline Swarm code contained in the document
--      ("Swarm.Text.Markdown.Parse"), and replace the text blobs with
--      actual ASTs.
--
--   3. Turn the document into a stream of tokens
--      ("Swarm.Text.Markdown.Token"), performing pretty-printing for
--      code blocks and flowing paragraphs to a specified line
--      width ("Swarm.Text.Markdown.Layout").
--
--   4. The resulting token stream can be easily rendered to text
--      ("Swarm.Text.Markdown.Render") or to brick widgets.
module Swarm.Text.Markdown (
  -- * Markdown document model
  Document (..),
  Paragraph (..),
  Node (..),
  TxtAttr (..),

  -- * Parsing
  fromTextE,
  fromTextM,
  fromText,

  -- * Token stream
  Token' (..),
  OutputToken,

  -- * Layout/rendering
  documentToStream,
  toText,
  toTextWidth,
  docToMark,

  -- ** Utilities
  findCode,
) where

import Swarm.Text.Markdown.Document (
  Document (..),
  Node (..),
  Paragraph (..),
  TxtAttr (..),
  findCode,
 )
import Swarm.Text.Markdown.Layout (documentToStream)
import Swarm.Text.Markdown.Parse (fromText, fromTextE, fromTextM)
import Swarm.Text.Markdown.Pretty (docToMark)
import Swarm.Text.Markdown.Render (toText, toTextWidth)
import Swarm.Text.Markdown.Token (OutputToken, Token' (..))
