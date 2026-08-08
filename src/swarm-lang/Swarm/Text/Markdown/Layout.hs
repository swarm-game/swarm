{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Converting Documents to token streams + layout.
module Swarm.Text.Markdown.Layout (
  -- * Utility functions on text
  tokenize,

  -- * Token stream normalisation
  normalise,

  -- * Document -> token stream conversion
  -- $document
  nodeToStream,
  paragraphToStream,
  documentToStream,
) where

import Commonmark.Types (ListSpacing (..), ListType (..))
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Pretty (PrettyPrec (..), prettyText, prettyTextLine, prettyTextWidth)
import Swarm.Text.Markdown.Document
import Swarm.Text.Markdown.Token
import Swarm.Util (chopNE, spanMaybe)

------------------------------------------------------------
-- Utility functions on text
------------------------------------------------------------

-- | Split text into individual whitespace and non-whitespace tokens.
--   Each newline is made into an individual token; other whitespace
--   are grouped into consecutive equal characters.
--
-- >>> :set -XOverloadedStrings
-- >>> tokenize "Hello   there, \n\nworld!"
-- ["Hello","   ","there,"," ","\n","\n","world!"]
tokenize :: Text -> [Text]
tokenize t = case T.uncons t of
  Nothing -> []
  Just (c, t')
    | c == '\n' -> "\n" : tokenize t'
    | isSpace c -> let (spc, rest) = T.span (== c) t in spc : tokenize rest
    | otherwise -> let (tok, rest) = T.span (not . isSpace) t in tok : tokenize rest

--------------------------------------------------------------
-- Document -> token stream conversion
--------------------------------------------------------------

-- $document
-- A Document is intended to have e.g. hierarchical structure
-- (especially once we add things like links, lists, etc.); for
-- rendering purposes we want to first convert a structured Document
-- into a simple token stream, split into lines at some specified
-- maximum line width.

------------------------------------------------------------
-- Node -> token stream conversion
------------------------------------------------------------

-- | Convert an inline node into a token stream.  The line width is
--   used only for pretty-printing code blocks; all other nodes are
--   simply turned into a linear stream without newline tokens.
nodeToStream :: PrettyPrec a => Maybe Int -> Node a -> [Token]
nodeToStream mw = \case
  -- Text and Raw nodes just turn into single tokens, with special
  -- cases for Text nodes to recognize spaces and punctuation.
  LeafText a t
    | T.all isSpace t -> [SoftSpace]
    | otherwise -> foldr applyAttr [TextToken t] a
  LeafRaw a t -> applyAttr (Raw a) . map (mkToken True) . tokenize $ t
  -- Inline code nodes get pretty-printed as a single line, then split
  -- into code tokens separated by soft spaces.
  -- TODO (#574): don't use tokenize, the pretty-printer should directly emit tokens!
  LeafCode c -> applyAttr Code . map (mkToken False) . tokenize . prettyTextLine $ c
  -- Code blocks get pretty-printed onto multiple lines using an
  -- appropriate line width, then split into code tokens with hard
  -- spaces.
  LeafCodeBlock _i c -> applyAttr Code . map (mkToken True) . tokenize $ maybe (prettyText c) (prettyTextWidth c) mw
  LeafLink dest title desc -> applyAttr (Link dest title) . concatMap (nodeToStream mw) $ desc
 where
  applyAttr attr ts = PushAttr attr : ts ++ [PopAttr]
  mkToken hard = \case
    "\n" -> Newline
    t
      -- Whitespace tokens turn into spaces.  Anything with only 1
      -- space turns into a SoftSpace.  Longer space tokens can turn
      -- into HardSpace.
      | T.all isSpace t -> if T.length t > 1 && hard then HardSpace (T.length t) else SoftSpace
      | otherwise -> TextToken t

------------------------------------------------------------
-- Token stream normalisation
------------------------------------------------------------

-- | Final normalisation step on a token stream:
--     - Get rid of special token types like HardSpace, SoftSpace, Glue, and EmptyToken
--     - Chunk text tokens together as much as possible, to cut down on e.g. number of brick widgets generated
normalise :: [Token] -> [OutputToken]
normalise = mergeTokens . concatMap normaliseToken
 where
  -- Normalise a single token.
  normaliseToken :: Token -> [OutputToken]
  normaliseToken = \case
    -- Tokens that are already normalised
    TextToken t -> [TextToken t]
    PushAttr a -> [PushAttr a]
    PopAttr -> [PopAttr]
    Newline -> [Newline]
    Para -> [Para]
    -- Turn spaces into text tokens.
    SoftSpace -> [" "]
    HardSpace n -> [TextToken (T.replicate n " ")]
    -- Expand glue tokens.
    Glue ts -> concatMap normaliseToken (NE.toList ts)
    -- Filter out empty tokens.
    EmptyToken -> []

  -- Now, merge as many consecutive text tokens as we can.
  mergeTokens :: [OutputToken] -> [OutputToken]
  mergeTokens = chopNE nextToken

  nextToken :: NonEmpty OutputToken -> (OutputToken, [OutputToken])
  nextToken (t :| ts) = case spanMaybe getTokenText (t : ts) of
    ([], _) -> (t, ts)
    (txts, rest) -> (TextToken (T.concat txts), rest)

------------------------------------------------------------
-- Paragraph -> token stream conversion + layout
------------------------------------------------------------

-- | Convert a paragraph into an output token stream.  The line width
--   is used to flow the paragraph by inserting newline tokens
--   appropriately.
--
--   The indentation is used to add an appropriate amount of hard
--   space to the beginning of each line, possibly including the
--   first.  (The paragraph could be an item in a list, in which case
--   the first line could already have something like a bullet or
--   number at the beginning instead of only indentation.)
--
--   Note that the line width does /not include/ the indentation.  In
--   other words, the line width parameter is the total width
--   available for paragraph content.  When recursively laying out
--   nested paragraphs with larger indentation, the line width must be
--   decreased appropriately.
--
--   A non-positive line width will simply be interpreted as a line
--   width of 1.
paragraphToStream :: PrettyPrec a => Bool -> Int -> Maybe Int -> Paragraph a -> [Token]
paragraphToStream indentFirstLine i mw = \case
  SimpleParagraph ns ->
    maybe id (splitter . max 1) mw
      . glueTokens
      . (if indentFirstLine then (indent <>) else id)
      . concatMap (nodeToStream mw)
      $ ns
  ListParagraph ty sp items -> intercalate (interlist sp) (map2 (listItem ty indentFirstLine) (listItem ty True) items)
 where
  indent = [HardSpace i | i > 0]
  linebreak = [Newline] <> indent

  interlist = \case
    TightList -> [Newline]
    LooseList -> [Newline, Newline]

  nest = 2

  bullet = \case
    BulletList b -> [TextToken (T.singleton b), SoftSpace]
    OrderedList {} -> [TextToken "-", SoftSpace] -- XXX fix me
  map2 _ _ [] = []
  map2 f g (x : xs) = f x : map g xs

  listItem :: PrettyPrec a => ListType -> Bool -> [Paragraph a] -> [Token]
  listItem ty shouldIndent = \case
    [] -> []
    (p : ps) ->
      intercalate
        [Para]
        ( ((if shouldIndent then indent else []) <> bullet ty <> paragraphToStream False (i + nest) (subtract nest <$> mw) p)
            : map (paragraphToStream True (i + nest) (subtract nest <$> mw)) ps
        )

  -- Given a maximum width per line, split a token stream into lines
  -- by inserting newline tokens, and ensure there are no SoftSpace
  -- tokens at the beginning or end of any line.
  splitter :: Int -> [Token] -> [Token]
  -- Defining 'splitter width = go width' looks pointless, but in fact the point is that 'go' can now
  -- keep track of a current width parameter and /also/ refer back to the original 'width'.
  splitter width = go width
   where
    go :: Int -> [Token] -> [Token]
    go remaining = \case
      [] -> []
      -- If we encounter an existing newline token, just emit it and
      -- move on to the next line, resetting the available width
      Newline : ts -> linebreak <> go width ts
      -- Special handling for SoftSpace
      SoftSpace : ts -> case ts of
        -- Discard a SoftSpace at the end of a line
        [] -> []
        Newline : ts' -> linebreak <> go width ts'
        (t : ts')
          -- Discard a SoftSpace at the beginning of a line
          | width == remaining -> go remaining ts
          -- If we can emit the space + next token, do so
          | 1 + tokenWidth t <= remaining -> SoftSpace : t : go (remaining - 1 - tokenWidth t) ts'
          -- Otherwise, discard the SoftSpace + move to the next line.
          | otherwise -> linebreak <> go width ts
      -- In the general case, check if we can emit the next token
      t : ts
        | tokenWidth t <= remaining -> t : go (remaining - tokenWidth t) ts
        -- ...but if the next token doesn't fit and we are at the
        -- beginning of the line, we must chop it into pieces to force
        -- progress
        | width == remaining ->
            let (t1, t2) = splitTokenAt width t in t1 : linebreak <> go width (t2 : ts)
      -- Finally, if nothing fits, emit a Newline and proceed to the next line
      ts -> linebreak <> go width ts

------------------------------------------------------------
-- Top-level Document -> token stream conversion
------------------------------------------------------------

-- | Convert an entire document into a token stream, inserting
--   paragraph breaks in between consecutive paragraphs. The resulting
--   token stream is also normalized by merging together consecutive
--   tokens of the same type as much as possible, e.g. to cut down on
--   the number of Brick widgets to be generated.
documentToStream :: PrettyPrec a => Maybe Int -> Document a -> [OutputToken]
documentToStream mw = normalise . intercalate [Para] . map (paragraphToStream True 0 mw) . paragraphs
