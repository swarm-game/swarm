{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Simple Markdown AST and related utilities.
--
-- Parameterising 'Document' with the type of
-- inline code and code blocks allows us to
-- inspect and validate Swarm code in descriptions.
--
-- See 'Swarm.TUI.View.Util.drawMarkdown' for
-- rendering the descriptions as brick widgets.
module Swarm.Language.Text.Markdown (
  -- ** Markdown document model
  Document (..),
  Paragraph (..),
  Node (..),
  TxtAttr (..),

  -- ** Parsing/conversion
  fromTextE,
  fromTextM,
  fromText,
  docToMark,

  -- ** Token stream
  StreamNode' (..),
  StreamNode,
  ToStream (..),
  streamToText,
  toText,

  -- ** Utilities
  findCode,
  chunksOf,
  splitWordsAt,
) where

import Commonmark qualified as Mark
import Commonmark.Extensions qualified as Mark (rawAttributeSpec)
import Control.Applicative ((<|>))
import Control.Arrow (left)
import Control.Carrier.Error.Either (runError)
import Control.Lens ((%~), (&), _head, _last)
import Data.Char (isSpace)
import Data.Functor.Identity (Identity (..))
import Data.List (intercalate)
import Data.List.Split (chop)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple.Extra (both, first)
import Data.Vector (toList)
import Data.Yaml
import GHC.Exts qualified (IsList (..), IsString (..))
import Swarm.Failure (SystemFailure)
import Swarm.Language.Parser (readTerm)
import Swarm.Language.Phase (ImportPhaseFor)
import Swarm.Language.Pipeline (processTermNoImports)
import Swarm.Language.Syntax (Anchor, Phase (Raw), Syntax, Unresolvable)
import Swarm.Pretty (PrettyPrec (..), prettyText, prettyTextLine)
import Swarm.Util (showT)

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
--   inline elements follow each other.  In particular inline code can
--   be followed by text without space between them
--   (e.g. @\`logger\`s@).
--
--   'Paragraph's form a 'Monoid' under concatenation.
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

-- | Simple text attributes.
data TxtAttr = Strong | Emphasis
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

-- | Normalise a paragraph, by combining consecutive 'LeafText' nodes with the same attributes.
--
-- XXX WHY do we want to do this?  What happens if we don't?
-- It was introduced in https://github.com/swarm-game/swarm/pull/1413 .
normalise :: (Eq c, Semigroup c) => Paragraph c -> Paragraph c
normalise (Paragraph a) = Paragraph $ go a
 where
  go = \case
    [] -> []
    (n : ns) -> let (n', ns') = mergeSame n ns in n' : go ns'
  mergeSame = \case
    l@(LeafText attrs1 t1) -> \case
      (LeafText attrs2 t2 : rss) | attrs1 == attrs2 -> mergeSame (LeafText attrs1 $ t1 <> t2) rss
      rs -> (l, rs)
    l -> (l,)

-- | Extract all the code embedded in a document.
findCode :: Document c -> [c]
findCode = concatMap (mapMaybe codeOnly . nodes) . paragraphs
 where
  codeOnly = \case
    LeafCode s -> Just s
    LeafCodeBlock _i s -> Just s
    _l -> Nothing

------------------------------------------------------------
-- Basic markdown -> Document parsing via Commonmark
------------------------------------------------------------

-- Some Commonmark instances for tracking source spans and attributes.
-- We do not use either, so the implementations are trivial.

instance Mark.Rangeable (Paragraph c) where
  ranged _ = id

instance Mark.HasAttributes (Paragraph c) where
  addAttributes _ = id

instance Mark.Rangeable (Document c) where
  ranged _ = id

instance Mark.HasAttributes (Document c) where
  addAttributes _ = id

-- | This instance allows us to write a 'Document' directly as a list of
--   'Paragraphs'.
instance GHC.Exts.IsList (Document a) where
  type Item (Document a) = Paragraph a
  toList = paragraphs
  fromList = Document

-- | This instance allows us to write a 'Document' as a string literal.
instance GHC.Exts.IsString (Document (Syntax Raw)) where
  fromString = fromText . T.pack

-- | This instance allows us to write a 'Paragraph' as a string literal.
instance GHC.Exts.IsString (Paragraph (Syntax Raw)) where
  fromString s = case paragraphs $ GHC.Exts.fromString s of
    [] -> mempty
    (p : _) -> p

-- | This instance allows us to write a text 'Node' as a string literal.
instance GHC.Exts.IsString (Node c) where
  fromString = LeafText mempty . T.pack

-- | Surround some text in double quotes if it is not empty.
quoteMaybe :: Text -> Text
quoteMaybe t = if T.null t then t else T.concat ["\"", t, "\""]

-- | This instance tells Commonmark how to parse Markdown inline elements into our custom data type.
instance Mark.IsInline (Paragraph Text) where
  lineBreak = pureP $ txt "\n"
  softBreak = pureP $ txt " "
  str = pureP . txt
  entity = Mark.str
  escapedChar c = Mark.str $ T.pack ['\\', c]
  emph = mapP $ addTextAttribute Emphasis
  strong = mapP $ addTextAttribute Strong
  link dest title desc = pureP (txt "[") <> desc <> pureP (txt $ "](" <> dest <> quoteMaybe title <> ")")
  image dest title desc = pureP (txt "!") <> Mark.link dest title desc
  code = pureP . LeafCode
  rawInline (Mark.Format f) = pureP . LeafRaw (T.unpack f)

-- | This instance tells Commonmark how to parse Markdown block elements into our custom data type.
instance Mark.IsBlock (Paragraph Text) (Document Text) where
  paragraph = Document . (: [])
  plain = Mark.paragraph
  thematicBreak = mempty
  blockQuote (Document ns) = Document $ map Mark.emph ns
  codeBlock f = Mark.plain . pureP . LeafCodeBlock (T.unpack f)
  heading _lvl = Mark.plain . Mark.strong
  rawBlock _ _ = mempty
  referenceLinkDefinition = mempty
  list _type _spacing = mconcat

-- | Read a Markdown document, leaving any embedded code as @Text@.
fromTextPure :: Text -> Either Text (Document Text)
fromTextPure t = do
  let spec = Mark.rawAttributeSpec <> Mark.defaultSyntaxSpec
  let runSimple = left showT . runIdentity
  Document tokenizedDoc <- runSimple $ Mark.commonmarkWith spec "markdown" t
  return . Document $ normalise <$> tokenizedDoc

------------------------------------------------------------
-- Markdown -> Document with Swarm code processing
------------------------------------------------------------

-- | Parse some syntax (without resolving any imports) and make sure
--   it typechecks, but keep the raw untyped/unelaborated syntax for
--   display.
parseSyntax :: Text -> Either Text (Syntax Raw)
parseSyntax s = case readTerm s of
  Left e -> Left e
  Right Nothing -> Left "empty code"
  -- Just run the typechecker etc. to make sure the term typechecks
  Right (Just t) -> case runError @SystemFailure (processTermNoImports s t Nothing) of
    -- If typechecking produces an error, just pretty-print the error message.
    Left e -> Left (prettyText @SystemFailure e)
    -- ...but if it does, we throw away the type-annotated +
    -- elaborated AST, and just go back to using the original parsed
    -- (*unelaborated*) AST.  See #1496.
    Right _ -> Right t

-- | Convert a 'Paragraph' to JSON by converting it to simple text.
instance (PrettyPrec (Anchor (ImportPhaseFor phase)), Unresolvable (ImportPhaseFor phase)) => ToJSON (Paragraph (Syntax phase)) where
  toJSON = String . toText -- XXX is this really what we want?

-- | Convert a 'Document' to JSON by reserializing it to Markdown format.
instance (PrettyPrec (Anchor (ImportPhaseFor phase)), Unresolvable (ImportPhaseFor phase)) => ToJSON (Document (Syntax phase)) where
  toJSON = String . docToMark

-- | Parse a 'Document' from JSON, either as a single string or as a list of paragraphs.
instance FromJSON (Document (Syntax Raw)) where
  parseJSON v = parseDoc v <|> parsePars v
   where
    parseDoc = withText "markdown" fromTextM
    parsePars = withArray "markdown paragraphs" $ \a -> do
      (ts :: [Text]) <- mapM parseJSON $ toList a
      fromTextM $ T.intercalate "\n\n" ts

-- | Read a Markdown document with embedded Swarm code.  Return any
--   error (whether Markdown parsing errors, or Swarm code parsing or
--   validation errors) as @Either Text@; the operation succeeds only
--   if the document can be read properly *and* all embedded Swarm
--   code validates.
fromTextE :: Text -> Either Text (Document (Syntax Raw))
fromTextE t = fromTextPure t >>= traverse parseSyntax

-- | Read a Markdown document with embedded Swarm code, but throw
--   errors in a 'MonadFail'.
fromTextM :: MonadFail m => Text -> m (Document (Syntax Raw))
fromTextM = either (fail . T.unpack) pure . fromTextE

-- | Read a Markdown document with embedded Swarm code, but re-inject
--   any parsing or typechecking errors back into the document itself.
--
--   This operation always succeeds.  If the document fails to parse,
--   a document consisting of a simple error message is returned.  If
--   any embedded Swarm code fails to validate, only that embedded
--   code is replaced with an error message.
fromText :: Text -> Document (Syntax Raw)
fromText = either (Document . (: []) . pureP . LeafRaw "") ((mapD . mapP) processNode) . fromTextPure
 where
  processNode = \case
    LeafCode c -> either (LeafRaw "") LeafCode (parseSyntax c)
    LeafCodeBlock b c -> either (LeafRaw "") (LeafCodeBlock b) (parseSyntax c)
    LeafText a b -> LeafText a b
    LeafRaw a b -> LeafRaw a b

--------------------------------------------------------------
-- Document -> token stream conversion
--------------------------------------------------------------

-- A Document is intended to have e.g. hierarchical structure
-- (especially once we add things like links, lists, etc.); for
-- rendering purposes we want to first convert a structured Document
-- into a simple token stream.
--
-- In addition, a Document can contain Nodes consisting of rather
-- large chunks of text. We can split tokens into smaller bits so that
-- they can e.g. flow to fit available space.

-- | Tokens in a stream that can be easily converted to text or brick widgets.
--
-- TODO: #574 Code blocks should probably be handled separately.
data StreamNode' t
  = TextNode (Set TxtAttr) t
  | CodeNode t
  | RawNode String t
  deriving (Eq, Show, Functor)

type StreamNode = StreamNode' Text

instance GHC.Exts.IsString StreamNode where
  fromString = TextNode mempty . T.pack

nodeLength :: StreamNode -> Int
nodeLength = \case
  TextNode _ t -> T.length t
  CodeNode t -> T.length t -- XXX length of a CodeNode is not really relevant if it has newlines...
  RawNode _ t -> T.length t

-- XXX this seems unnecessarily complicated.  Let's figure out a way to get rid of this?
unStream :: StreamNode' t -> (t -> StreamNode' t, t)
unStream = \case
  TextNode a t -> (TextNode a, t)
  CodeNode t -> (CodeNode, t)
  RawNode a t -> (RawNode a, t)

-- XXX does this properly handle raw code blocks with internal newlines??
-- XXX need to add some test cases!  Are there any?

-- | Break a stream of nodes into chunks such that the total length of
--   each chunk does not exceed the given line width, possibly
--   splitting nodes into smaller nodes (at word boundaries) to
--   achieve this.
chunksOf :: Int -> [StreamNode] -> [[StreamNode]]
chunksOf n = chop (splitter True n)
 where
  -- start = are we at the start of a line?
  -- i = remaining total width for this line

  -- Split a stream into an initial part no longer than i, and a
  -- remaining part, possibly splitting a StreamNode into two if it
  -- hangs over the end of the line and can be usefully split.
  --
  -- XXX however, don't do anything with CodeNodes?
  splitter :: Bool -> Int -> [StreamNode] -> ([StreamNode], [StreamNode])
  splitter start i = \case
    [] -> ([], [])
    (tn : ss) ->
      let l = nodeLength tn
       in if l <= i
            then first (tn :) $ splitter False (i - l) ss
            else let (tn1, tn2) = cut start i tn in ([tn1], tn2 : ss)
  cut :: Bool -> Int -> StreamNode -> (StreamNode, StreamNode)
  cut start i tn =
    let (con, t) = unStream tn
        endSpace = T.takeWhileEnd isSpace t
        startSpace = T.takeWhile isSpace t
        twords = T.words t & _head %~ (startSpace <>) & _last %~ (<> endSpace)
     in case splitWordsAt i twords of
          ([], []) -> (con "", con "")
          ([], ws@(ww : wws)) ->
            both (con . T.unwords) $
              -- In case single word (e.g. web link) does not fit on line we must put
              -- it there and guarantee progress (otherwise chop will cycle)
              if start then ([T.take i ww], T.drop i ww : wws) else ([], ws)
          splitted -> both (con . T.unwords) splitted

-- | Given a target length, split a list of words into an maximal
--   initial prefix whose length (*including* one space between each
--   word) does not exceed the target length, and the rest of the
--   words.
--
--   That is, if @splitWordsAt l xs = (ys,zs)@ then
--     1. @ys ++ zs == xs@,
--     2. @map T.length ys + length ys - 1 <= l@ (or, in other
--        words, @T.length (T.unwords ys) <= l@), and
--     3. ys is as long as possible.
splitWordsAt :: Int -> [Text] -> ([Text], [Text])
splitWordsAt i = \case
  [] -> ([], [])
  (w : ws) ->
    let l = T.length w
     in if l <= i
          then first (w :) $ splitWordsAt (i - l - 1) ws
          else ([], w : ws)

-- | Simple stream -> Text conversion, ignoring formatting.  Intended
--   for debugging or otherwise displaying a document in a text-only
--   format.
streamToText :: [StreamNode] -> Text
streamToText = T.concat . map nodeToText
 where
  nodeToText = \case
    TextNode _a t -> t
    RawNode _s t -> t
    CodeNode stx -> stx

-- XXX make toStream take an optional line width, so we can pass it to
-- the pretty-printer!  Then we don't have to call chunksOf afterwards.

-- | Things that can be converted into a stream of nodes.
class ToStream a where
  -- | Convert to a stream of nodes, optionally broken into lines at a
  --   specified line width.  If no line width is given, returns a
  --   single list of nodes.
  toStream :: a -> [StreamNode] -- XXX

instance PrettyPrec a => ToStream (Node a) where
  toStream = \case
    LeafText a t -> [TextNode a t]
    LeafCode t -> [CodeNode (prettyTextLine t)]
    LeafRaw s t -> [RawNode s t]
    LeafCodeBlock _i t -> [CodeNode (prettyText t)]

instance PrettyPrec a => ToStream (Paragraph a) where
  toStream = concatMap toStream . nodes

instance PrettyPrec a => ToStream (Document a) where
  toStream = intercalate ["\n\n"] . map toStream . paragraphs

-- | This is the naive and easy way to get text from anything that can
--   be converted to a token stream (such as 'Document'), ignoring any
--   formatting.
toText :: ToStream a => a -> Text
toText = streamToText . toStream

--------------------------------------------------------------
-- Re-serializing Document -> Markdown
--------------------------------------------------------------

-- | Convert a single 'Node' to Markdown format.
nodeToMark :: PrettyPrec a => Node a -> Text
nodeToMark = \case
  LeafText a t -> foldl attr t a
  LeafRaw _ c -> wrap "`" c
  LeafCode c -> wrap "`" (prettyText c)
  LeafCodeBlock f c -> codeBlock f $ prettyText c
 where
  codeBlock f t = wrap "```" $ T.pack f <> "\n" <> t <> "\n"
  wrap c t = c <> t <> c
  attr t a = case a of
    Emphasis -> wrap "_" t
    Strong -> wrap "**" t

-- | Convert a 'Paragraph' to Markdown format.
paragraphToMark :: PrettyPrec a => Paragraph a -> Text
paragraphToMark = foldMap nodeToMark . nodes

-- | Convert a 'Document' to Markdown format.
docToMark :: PrettyPrec a => Document a -> Text
docToMark = T.intercalate "\n\n" . map paragraphToMark . paragraphs
