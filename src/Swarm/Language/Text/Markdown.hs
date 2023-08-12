{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Simple Markdown AST and related utilities.
--
-- Parametrising 'Document' with the type of
-- inline code and code blocks allows us to
-- inspect and validate Swarm code in descriptions.
--
-- See 'Swarm.TUI.View.Util.drawMarkdown' for
-- rendering the descriptions as brick widgets.
module Swarm.Language.Text.Markdown (
  -- ** Markdown document
  Document (..),
  Paragraph (..),
  Node (..),
  TxtAttr (..),
  fromTextM,
  fromText,
  docToText,
  docToMark,

  -- ** Token stream
  StreamNode' (..),
  StreamNode,
  ToStream (..),
  toText,

  -- ** Utilities
  findCode,
  chunksOf,
) where

import Commonmark qualified as Mark
import Commonmark.Extensions qualified as Mark (rawAttributeSpec)
import Control.Applicative ((<|>))
import Control.Arrow (left)
import Control.Lens ((%~), (&), _head, _last)
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Functor.Identity (Identity (..))
import Data.List.Split (chop)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple.Extra (both, first)
import Data.Vector (toList)
import Data.Yaml
import GHC.Exts qualified (IsList (..), IsString (..))
import Swarm.Language.Module (moduleAST)
import Swarm.Language.Parse (readTerm)
import Swarm.Language.Pipeline (ProcessedTerm (..), processParsedTerm)
import Swarm.Language.Pretty (PrettyPrec (..), prettyText, prettyTypeErrText)
import Swarm.Language.Syntax (Syntax)

-- | The top-level markdown document.
newtype Document c = Document {paragraphs :: [Paragraph c]}
  deriving (Eq, Show, Functor, Foldable, Traversable)
  deriving (Semigroup, Monoid) via [Paragraph c]

-- | Markdown paragraphs that contain inline leaf nodes.
--
-- The idea is that paragraphs do not have line breaks,
-- and so the inline elements follow each other.
-- In particular inline code can be followed by text without
-- space between them (e.g. `logger`s).
newtype Paragraph c = Paragraph {nodes :: [Node c]}
  deriving (Eq, Show, Functor, Foldable, Traversable)
  deriving (Semigroup, Monoid) via [Node c]

mapP :: (Node c -> Node c) -> Paragraph c -> Paragraph c
mapP f (Paragraph ns) = Paragraph (map f ns)

pureP :: Node c -> Paragraph c
pureP = Paragraph . (: [])

-- | Inline leaf nodes.
--
-- The raw node is from the raw_annotation extension,
-- and can be used for types/entities/invalid code.
data Node c
  = LeafText (Set TxtAttr) Text
  | LeafRaw String Text
  | LeafCode c
  | LeafCodeBlock String c
  deriving (Eq, Show, Functor, Foldable, Traversable)

txt :: Text -> Node c
txt = LeafText mempty

addTextAttribute :: TxtAttr -> Node c -> Node c
addTextAttribute a (LeafText as t) = LeafText (Set.insert a as) t
addTextAttribute _ n = n

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

-- | Simple text attributes that make it easier to find key info in descriptions.
data TxtAttr = Strong | Emphasis
  deriving (Eq, Show, Ord)

instance Mark.Rangeable (Paragraph c) where
  ranged _ = id

instance Mark.HasAttributes (Paragraph c) where
  addAttributes _ = id

instance Mark.Rangeable (Document c) where
  ranged _ = id

instance Mark.HasAttributes (Document c) where
  addAttributes _ = id

instance GHC.Exts.IsList (Document a) where
  type Item (Document a) = Paragraph a
  toList = paragraphs
  fromList = Document

instance GHC.Exts.IsString (Document Syntax) where
  fromString = fromText . T.pack

instance GHC.Exts.IsString (Paragraph Syntax) where
  fromString s = case paragraphs $ GHC.Exts.fromString s of
    [] -> mempty
    [p] -> p
    ps -> error $ "Error: expected one paragraph, but found " <> show (length ps)

-- | Surround some text in double quotes if it is not empty.
quoteMaybe :: Text -> Text
quoteMaybe t = if T.null t then t else T.concat ["\"", t, "\""]

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

instance Mark.IsBlock (Paragraph Text) (Document Text) where
  paragraph = Document . (: [])
  plain = Mark.paragraph
  thematicBreak = mempty
  blockQuote (Document ns) = Document $ map Mark.emph ns
  codeBlock f = Mark.plain . pureP . LeafCodeBlock (T.unpack f)
  heading _lvl = Mark.plain . Mark.strong
  rawBlock (Mark.Format f) t = error . T.unpack $ "Unsupported raw " <> f <> " block:\n" <> t
  referenceLinkDefinition = mempty
  list _type _spacing = mconcat

parseSyntax :: Text -> Either String Syntax
parseSyntax t = case readTerm t of
  Left e -> Left (T.unpack e)
  Right Nothing -> Left "empty code"
  Right (Just s) -> case processParsedTerm s of
    Left e -> Left (T.unpack $ prettyTypeErrText t e)
    Right (ProcessedTerm modul _req _reqCtx) -> Right $ void $ moduleAST modul

findCode :: Document Syntax -> [Syntax]
findCode = catMaybes . concatMap (map codeOnly . nodes) . paragraphs
 where
  codeOnly = \case
    LeafCode s -> Just s
    LeafCodeBlock _i s -> Just s
    _l -> Nothing

instance ToJSON (Paragraph Syntax) where
  toJSON = String . toText

instance ToJSON (Document Syntax) where
  toJSON = String . docToMark

instance FromJSON (Document Syntax) where
  parseJSON v = parseDoc v <|> parsePars v
   where
    parseDoc = withText "markdown" fromTextM
    parsePars = withArray "markdown paragraphs" $ \a -> do
      (ts :: [Text]) <- mapM parseJSON $ toList a
      fromTextM $ T.intercalate "\n\n" ts

-- | Parse Markdown document, but throw on invalid code.
fromText :: Text -> Document Syntax
fromText = either error id . fromTextE

-- | Read Markdown document and parse&validate the code.
--
-- If you want only the document with code as `Text`,
-- use the 'fromTextPure' function.
fromTextM :: MonadFail m => Text -> m (Document Syntax)
fromTextM = either fail pure . fromTextE

fromTextE :: Text -> Either String (Document Syntax)
fromTextE t = fromTextPure t >>= traverse parseSyntax

-- | Read Markdown document without code validation.
fromTextPure :: Text -> Either String (Document Text)
fromTextPure t = do
  let spec = Mark.rawAttributeSpec <> Mark.defaultSyntaxSpec <> Mark.rawAttributeSpec
  let runSimple = left show . runIdentity
  Document tokenizedDoc <- runSimple $ Mark.commonmarkWith spec "markdown" t
  return . Document $ normalise <$> tokenizedDoc

--------------------------------------------------------------
-- DIY STREAM
--------------------------------------------------------------

-- | Convert 'Document' to 'Text'.
--
-- Note that this will strip some markdown, emphasis and bold marks.
-- If you want to get markdown again, use 'docToMark'.
docToText :: PrettyPrec a => Document a -> Text
docToText = T.intercalate "\n\n" . map toText . paragraphs

-- | This is the naive and easy way to get text from markdown document.
toText :: ToStream a => a -> Text
toText = streamToText . toStream

-- | Token stream that can be easily converted to text or brick widgets.
--
-- TODO: #574 Code blocks should probably be handled separately.
data StreamNode' t
  = TextNode (Set TxtAttr) t
  | CodeNode t
  | RawNode String t
  deriving (Eq, Show, Functor)

type StreamNode = StreamNode' Text

unStream :: StreamNode' t -> (t -> StreamNode' t, t)
unStream = \case
  TextNode a t -> (TextNode a, t)
  CodeNode t -> (CodeNode, t)
  RawNode a t -> (RawNode a, t)

-- | Get chunks of nodes not exceeding length and broken at word boundary.
chunksOf :: Int -> [StreamNode] -> [[StreamNode]]
chunksOf n = chop (splitter True n)
 where
  nodeLength :: StreamNode -> Int
  nodeLength = T.length . snd . unStream
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

splitWordsAt :: Int -> [Text] -> ([Text], [Text])
splitWordsAt i = \case
  [] -> ([], [])
  (w : ws) ->
    let l = T.length w
     in if l < i
          then first (w :) $ splitWordsAt (i - l - 1) ws
          else ([], w : ws)

streamToText :: [StreamNode] -> Text
streamToText = T.concat . map nodeToText
 where
  nodeToText = \case
    TextNode _a t -> t
    RawNode _s t -> t
    CodeNode stx -> stx

-- | Convert elements to one dimensional stream of nodes,
-- that is easy to format and layout.
--
-- If you want to split the stream at line length, use
-- the 'chunksOf' function afterward.
class ToStream a where
  toStream :: a -> [StreamNode]

instance PrettyPrec a => ToStream (Node a) where
  toStream = \case
    LeafText a t -> [TextNode a t]
    LeafCode t -> [CodeNode (prettyText t)]
    LeafRaw s t -> [RawNode s t]
    LeafCodeBlock _i t -> [CodeNode (prettyText t)]

instance PrettyPrec a => ToStream (Paragraph a) where
  toStream = concatMap toStream . nodes

--------------------------------------------------------------
-- Markdown
--------------------------------------------------------------

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

paragraphToMark :: PrettyPrec a => Paragraph a -> Text
paragraphToMark = foldMap nodeToMark . nodes

-- | Convert 'Document' to markdown text.
docToMark :: PrettyPrec a => Document a -> Text
docToMark = T.intercalate "\n\n" . map paragraphToMark . paragraphs
