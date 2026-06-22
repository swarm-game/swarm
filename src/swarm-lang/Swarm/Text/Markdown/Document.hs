-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- AST to represent Markdown documents with embedded code.
-- Parameterising 'Document' with the type of inline code and code
-- blocks allows us to inspect and validate Swarm code in
-- descriptions.
module Swarm.Text.Markdown.Document where

import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

------------------------------------------------------------
-- Simple Document model
------------------------------------------------------------

-- | A top-level markdown document, represented as a list of
--   paragraphs, with each paragraph consisting of a list of nodes.
--   The representation is as simple as possible while containing the
--   features we need.
--
--   'Document' is parameterized by the type of code blocks it
--   contains.  In particular, we can start by parsing a @Document
--   Text@ from markdown source, and then later run the Swarm parser
--   on the code blocks to produce a @Document (Syntax Raw)@, and so
--   on.
newtype Document c = Document {paragraphs :: [Paragraph c]}
  deriving (Eq, Show, Functor, Foldable, Traversable)
  deriving (Semigroup, Monoid) via [Paragraph c]

-- | Markdown paragraphs, consisting of a list of inline leaf nodes.
--
--   The idea is that paragraphs do not have line breaks, and so the
--   inline elements follow each other, with spaces represented as
--   explicit nodes.  In particular, inline code can be followed by
--   text without space between them (e.g. @\`logger\`s@).
--
--   'Paragraph's form a 'Monoid' under concatenation (where combining
--   two paragraphs means running them together into a single
--   paragraph), with the empty paragraph as the identity.
newtype Paragraph c = Paragraph {nodes :: [Node c]}
  deriving (Eq, Show, Functor, Foldable, Traversable)
  deriving (Semigroup, Monoid) via [Node c]

-- | Map a function over every 'Paragraph' in a 'Document'.
mapD :: (Paragraph c -> Paragraph c') -> Document c -> Document c'
mapD f (Document ps) = Document (map f ps)

-- | Map a function over every 'Node' in a 'Paragraph'.
mapP :: (Node c -> Node c') -> Paragraph c -> Paragraph c'
mapP f (Paragraph ns) = Paragraph (map f ns)

-- | Create a singleton 'Paragraph' with one 'Node'.
pureP :: Node c -> Paragraph c
pureP = Paragraph . (: [])

-- | Text attributes.
data TxtAttr where
  -- | Strong, i.e. bold
  Strong :: TxtAttr
  -- | Emphasis, i.e. italics
  Emphasis :: TxtAttr
  -- | "Raw" text, with an arbitrary annotation.
  Raw :: String -> TxtAttr
  -- | Code.
  Code :: TxtAttr
  deriving (Eq, Show, Ord)

-- | Inline leaf nodes.
data Node c
  = -- | Text, with attributes.
    LeafText (Set TxtAttr) Text
  | -- | The raw node is from the raw_annotation extension (indicated
    --   using syntax like `foo`{=type}) and is used for e.g. types,
    --   entities, or invalid code snippets.  The String preserves the
    --   annotation.
    LeafRaw String Text
  | -- | Inline Swarm code.
    LeafCode c
  | -- | A code block.
    LeafCodeBlock String c
  deriving (Eq, Show, Functor, Foldable, Traversable)

--------------------------------------------------
-- Utilities

-- | Create a plain text node.
txt :: Text -> Node c
txt = LeafText mempty

-- | Add attributes to a text node.  Has no effect on other node types.
addTextAttribute :: TxtAttr -> Node c -> Node c
addTextAttribute a (LeafText as t) = LeafText (Set.insert a as) t
addTextAttribute _ n = n

-- | Extract all the code embedded in a document.
findCode :: Document c -> [c]
findCode = concatMap (mapMaybe codeOnly . nodes) . paragraphs
 where
  codeOnly = \case
    LeafCode s -> Just s
    LeafCodeBlock _i s -> Just s
    _l -> Nothing
