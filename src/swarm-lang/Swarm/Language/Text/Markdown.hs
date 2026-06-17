{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- XXX split this out into more modules

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Simple Markdown AST and related utilities.
--
-- Parameterising 'Document' with the type of inline code and code
-- blocks allows us to inspect and validate Swarm code in
-- descriptions.
--
-- See 'Swarm.TUI.View.Util.drawMarkdown' for rendering Markdown as
-- brick widgets.
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
  Token' (..),
  TokenPhase (..),
  OutputToken,
  ToStream (..),
  glueTokens,
  toText,
  toTextWidth,

  -- ** Utilities
  findCode,
  tokenize,
) where

import Commonmark qualified as Mark
import Commonmark.Extensions qualified as Mark (rawAttributeSpec)
import Control.Applicative ((<|>))
import Control.Arrow (left, (***))
import Control.Carrier.Error.Either (runError)
import Control.Lens (both, over)
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.List.Split (chop)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (toList)
import Data.Yaml
import GHC.Exts qualified (IsList (..), IsString (..))
import Swarm.Failure (SystemFailure)
import Swarm.Language.Parser (readTerm)
import Swarm.Language.Phase (ImportPhaseFor)
import Swarm.Language.Pipeline (processTermNoImports)
import Swarm.Language.Syntax (Anchor, Phase (Raw), Syntax, Unresolvable)
import Swarm.Pretty (PrettyPrec (..), prettyText, prettyTextLine, prettyTextWidth)
import Swarm.Util (chopNE, showT, spanMaybe)

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
  runSimple $ Mark.commonmarkWith spec "markdown" t

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

------------------------------------------------------------
-- Utility functions on text
------------------------------------------------------------

-- | Split text into individual whitespace and non-whitespace tokens.
--   Each newline is made into an individual token; other whitespace
--   are grouped into consecutive equal characters.
--
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

-- A Document is intended to have e.g. hierarchical structure
-- (especially once we add things like links, lists, etc.); for
-- rendering purposes we want to first convert a structured Document
-- into a simple token stream, split into lines at some specified
-- maximum line width.

--------------------------------------------------
-- Tokens

-- | At different phases we have different types of tokens available.
--   During the layout phase, we have various types of special tokens
--   which we translate away by the output phase.
data TokenPhase = Layout | Unglued | Output deriving (Eq, Ord, Show)

type Token = Token' Layout
type OutputToken = Token' Output

-- | Tokens in a stream that can be easily converted to text or brick widgets.
data Token' :: TokenPhase -> Type where
  -- | Basic text token.
  TextToken :: Text -> Token' p
  -- | Zero-width token to indicate that an attribute applies from now until a matching pop.
  PushAttr :: TxtAttr -> Token' p
  -- | Zero-width token to indicate that the matching attribute no longer applies.
  PopAttr :: Token' p
  -- | Line break.
  Newline :: Token' p
  -- | Paragraph break.
  Para :: Token' p
  -- | A "soft" space which is used only to separate other tokens.
  --   It should be displayed as a single space in between tokens,
  --   or discarded if it falls at the beginning or end of a line.
  SoftSpace :: Token' Layout
  -- | A "hard" or required space which must be displayed.  The Int
  --   is the number of consecutive space characters.  For example,
  --   a hard space may be generated at the beginning of a line in a
  --   code block, since the indentation is semantically meaningful
  --   and should neither be discarded nor turned into a single
  --   space.
  HardSpace :: Int -> Token' Layout
  -- | Several primitive tokens glued together, considered as an
  --   unbreakable group for the purposes of layout, but still kept
  --   separate so we can style or render them independently.
  Glue :: NonEmpty Token -> Token' Layout
  -- | Empty token.  Takes no space and produces no output.
  EmptyToken :: Token' Layout

deriving instance Eq (Token' p)
deriving instance Show (Token' p)

-- | The width of a token is the amount of horizontal space it takes
--   up on a line.  Hence e.g. the width of a Newline token is
--   considered to be 0.
tokenWidth :: Token' p -> Int
tokenWidth = \case
  TextToken t -> T.length t
  PushAttr _ -> 0
  PopAttr -> 0
  Newline -> 0
  Para -> 0
  SoftSpace -> 1
  HardSpace n -> n
  Glue ts -> sum (fmap tokenWidth ts)
  EmptyToken -> 0

instance GHC.Exts.IsString (Token' p) where
  fromString = TextToken . T.pack

-- | Check if a token is a text token, extracting its text if so.
getTokenText :: Token' p -> Maybe Text
getTokenText = \case
  TextToken t -> Just t
  _ -> Nothing

--------------------------------------------------
-- Token gluing

-- | Glue a list of tokens into a single token appropriately.
glue :: [Token] -> Token
glue [] = EmptyToken
glue [t] = t
glue (t : ts) = Glue (t :| ts)

-- | Some tokens are "sticky" and like to stick to the other tokens to
--   their left or right, or both.  Neutral tokens are not sticky on
--   their own, but can have other tokens stick to them.  Nothing can
--   stick to nonstick tokens.
data Stickiness = StickyL | StickyR | StickyLR | Neutral | Nonstick deriving (Eq, Ord, Show)

-- | The "stickiness" of a token, i.e. whether it prefers to stick to
--   tokens on its left or right.
stickiness :: Token -> Stickiness
stickiness = \case
  TextToken t
    | t `elem` T.words "( [ {" -> StickyR
    | t `elem` T.words ". , ; : ? ! ) ] } - -- --- /" -> StickyL
    | t `elem` T.words "\" '" -> StickyLR
    | otherwise -> Neutral
  -- make push sticky so it won't be emitted by itself at the end of a line
  PushAttr _ -> StickyR
  PopAttr -> StickyL
  Newline -> Neutral
  Para -> Neutral
  -- Soft spaces are nonstick, so that e.g. things sticky in both
  -- directions (such as quote marks) stick to a nonspace thing next
  -- to them but not a space.
  SoftSpace -> Nonstick
  HardSpace {} -> StickyR
  Glue {} -> Neutral
  EmptyToken -> Neutral

-- | Preprocess a token stream by gluing together any sticky tokens.
glueTokens :: [Token] -> [Token]
glueTokens = chop (first glue . go1)
 where
  -- Get a first token.  If nonstick, just emit it.  Otherwise, start
  -- looking for sticky tokens: consecutive right-sticky tokens if the
  -- first token was right-sticky, or left-sticky otherwise.
  go1, goR, goL :: [Token] -> ([Token], [Token])
  go1 [] = ([], [])
  go1 (t : ts)
    | stickiness t == Nonstick = ([t], ts)
    | otherwise = first (t :) $ (if stickiness t `elem` [StickyR, StickyLR] then goR else goL) ts

  -- Look for consecutive right-sticky tokens...
  goR [] = ([], [])
  goR (t : ts)
    -- Accumulate right-sticky tokens and keep looking for more
    | stickiness t `elem` [StickyR, StickyLR] = first (t :) (goR ts)
    -- Stop if we encounter a nonstick token
    | stickiness t == Nonstick = ([], t : ts)
    -- Otherwise, switch to looking for left-sticky tokens
    | otherwise = first (t :) (goL ts)

  -- ...and then consecutive left-sticky tokens.
  goL [] = ([], [])
  goL (t : ts)
    -- Accumulate left-sticky tokens and keep looking for more
    | stickiness t == StickyL = first (t :) (goL ts)
    -- If we see a LR-sticky token, switch back to looking for right-sticky tokens
    | stickiness t == StickyLR = first (t :) (goR ts)
    -- Otherwise, stop
    | otherwise = ([], t : ts)

--------------------------------------------------
-- Token splitting

-- | Split a token into two, such that the first is no longer than the
--   specified length.
splitTokenAt :: Int -> Token -> (Token, Token)
splitTokenAt w = \case
  TextToken t -> over both mkNE (T.splitAt w t)
  PushAttr a -> (PushAttr a, EmptyToken)
  PopAttr -> (PopAttr, EmptyToken)
  Newline -> (Newline, EmptyToken)
  Para -> (Para, EmptyToken)
  SoftSpace -> (SoftSpace, EmptyToken)
  HardSpace n -> (HardSpace (min n w), if min n w == n then EmptyToken else HardSpace (n - min n w))
  Glue ts -> over both glue (splitTokenListAt w (NE.toList ts))
  EmptyToken -> (EmptyToken, EmptyToken)
 where
  mkNE t
    | T.null t = EmptyToken
    | otherwise = TextToken t

  splitTokenListAt :: Int -> [Token] -> ([Token], [Token])
  splitTokenListAt _ [] = ([], [])
  splitTokenListAt n (t : ts) = case compare (tokenWidth t) n of
    LT -> first (t :) (splitTokenListAt (n - tokenWidth t) ts)
    EQ -> ([t], ts)
    GT -> ((: []) *** (: ts)) (splitTokenAt n t)

--------------------------------------------------
-- ToStream class, Node conversion

-- | Things that can be converted into a stream of tokens, possibly
--   taking into account an optional line width.
class ToStream a p where
  -- | Convert to a stream of tokens, taking into account an optional line width.
  --   a specified line width by inserting Newline tokens.  If no line
  --   width is given, no extra Newline tokens will be inserted
  --   (though some may still be generated by e.g. code blocks).
  toStream :: Maybe Int -> a -> [Token' p]

-- | Convert a document node into a token stream.  The line width is
--   used only for pretty-printing code blocks; all other nodes are
--   simply turned into a linear stream without newline tokens.
instance PrettyPrec a => ToStream (Node a) Layout where
  toStream :: PrettyPrec a => Maybe Int -> Node a -> [Token]
  toStream mw = \case
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
   where
    applyAttr attr ts = PushAttr attr : ts ++ [PopAttr]
    mkToken hard = \case
      "\n" -> Newline
      t
        | T.all isSpace t -> if hard then HardSpace (T.length t) else SoftSpace
        | otherwise -> TextToken t

--------------------------------------------------
-- Token stream normalisation

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

--------------------------------------------------
-- Paragraph -> token stream conversion + layout

-- | Convert a paragraph into a token stream.  The line width is used
--   to flow the paragraph by inserting newline tokens appropriately.
--   The resulting token stream is also normalized by merging together
--   consecutive tokens of the same type as much as possible, e.g. to cut
--   down on the number of Brick widgets to be generated.
instance PrettyPrec a => ToStream (Paragraph a) Output where
  toStream mw =
    normalise
      . maybe id (\w -> splitter w w) mw
      . glueTokens
      . concatMap (toStream mw)
      . nodes
   where
    -- Split a token stream into lines by inserting newline tokens,
    -- and ensure there are no SoftSpace tokens at the beginning or
    -- end of any line.
    --
    -- The first Int is the max line width for each line.
    -- The second Int tells us how much width remains on the current line.
    splitter :: Int -> Int -> [Token] -> [Token]
    splitter width remaining = \case
      [] -> []
      -- If we encounter an existing newline token, just emit it and
      -- move on to the next line, resetting the available width
      Newline : ts -> Newline : splitter width width ts
      -- Special handling for SoftSpace
      SoftSpace : ts -> case ts of
        -- Discard a SoftSpace at the end of a line
        [] -> []
        Newline : ts' -> Newline : splitter width width ts'
        (t : ts')
          -- Discard a SoftSpace at the beginning of a line
          | width == remaining -> splitter width remaining ts
          -- If we can emit the space + next token, do so
          | 1 + tokenWidth t <= remaining -> SoftSpace : t : splitter width (remaining - 1 - tokenWidth t) ts'
          -- Otherwise, discard the SoftSpace + move to the next line.
          | otherwise -> Newline : splitter width width ts
      -- In the general case, check if we can emit the next token
      t : ts
        | tokenWidth t <= remaining -> t : splitter width (remaining - tokenWidth t) ts
        -- ...but if the next token doesn't fit and we are at the
        -- beginning of the line, we must chop it into pieces to force
        -- progress
        | width == remaining ->
            let (t1, t2) = splitTokenAt width t in t1 : Newline : splitter width width (t2 : ts)
      -- Finally, if nothing fits, emit a Newline and proceed to the next line
      ts -> Newline : splitter width width ts

--------------------------------------------------
-- Document -> token stream

-- | Convert an entire document into a token stream, inserting
--   paragraph breaks in between consecutive paragraphs.
instance PrettyPrec a => ToStream (Document a) Output where
  toStream mw = intercalate [Para] . map (toStream mw) . paragraphs

------------------------------------------------------------
-- Token stream -> text conversion
------------------------------------------------------------

tokenToText :: OutputToken -> Text
tokenToText = \case
  TextToken t -> t
  Newline -> "\n"
  Para -> "\n\n"
  PushAttr _ -> ""
  PopAttr -> ""

streamToText :: [OutputToken] -> Text
streamToText = T.concat . map tokenToText

-- | Turn anything that can be converted to a token stream (such as
--   'Document') into text, ignoring any formatting.
toText :: ToStream a Output => a -> Text
toText = toTextWidth Nothing

-- | Turn anything that can be converted to a token stream (such as
--   'Document') into text, ignoring any formatting but wrapping to a
--   specified line width.
toTextWidth :: ToStream a Output => Maybe Int -> a -> Text
toTextWidth mw = streamToText . toStream mw

--------------------------------------------------------------
-- Re-serializing Document -> Markdown
--------------------------------------------------------------

-- | Convert a single 'Node' to Markdown format.
nodeToMark :: PrettyPrec a => Node a -> Text
nodeToMark = \case
  LeafText a t -> foldl attr t a
  LeafRaw a c ->
    mconcat $
      wrap "`" c : ["{=" <> T.pack a <> "}" | not (null a)]
  LeafCode c -> wrap "`" (prettyText c)
  LeafCodeBlock f c -> codeBlock f $ prettyText c
 where
  codeBlock f t = wrap "```" $ T.pack f <> "\n" <> t <> "\n"
  wrap c t = c <> t <> c
  attr t a = case a of
    Emphasis -> wrap "_" t
    Strong -> wrap "**" t
    -- Raw and Code attributes won't actually occur in a LeafText
    -- node, LeafRaw and LeafCode will be used instead.
    Raw _ -> t
    Code -> t

-- | Convert a 'Paragraph' to Markdown format.
paragraphToMark :: PrettyPrec a => Paragraph a -> Text
paragraphToMark = foldMap nodeToMark . nodes

-- | Convert a 'Document' to Markdown format.
docToMark :: PrettyPrec a => Document a -> Text
docToMark = T.intercalate "\n\n" . map paragraphToMark . paragraphs
