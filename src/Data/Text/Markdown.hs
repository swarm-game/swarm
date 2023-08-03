{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Markdown (
  -- ** Markdown document
  Document (..),
  Paragraph (..),
  Node (..),
  TxtAttr (..),
  fromTextM,

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
import Control.Monad (void)
import Data.Functor.Identity (Identity (..))
import Data.List qualified as List
import Data.List.Split (chop)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple.Extra (both, first)
import Data.Vector (toList)
import Data.Yaml
import Swarm.Language.Module (moduleAST)
import Swarm.Language.Parse (readTerm)
import Swarm.Language.Pipeline (ProcessedTerm (..), processParsedTerm)
import Swarm.Language.Pretty (prettyText, prettyTypeErrText)
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

-- | Surround some text in double quotes if it is not empty.
quoteMaybe :: Text -> Text
quoteMaybe t = if T.null t then t else T.concat ["\"", t, "\""]

instance Mark.IsInline (Paragraph Text) where
  lineBreak = pureP $ txt "\n"
  softBreak = mempty
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
  toJSON = String . toText

instance FromJSON (Document Syntax) where
  parseJSON v = parsePars v <|> parseDoc v
   where
    parseDoc = withText "markdown" fromTextM
    parsePars = withArray "markdown paragraphs" $ \a -> do
      (ts :: [Text]) <- mapM parseJSON $ toList a
      fromTextM $ T.intercalate "\n\n" ts

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
  runSimple $ Mark.commonmarkWith spec "markdown" t

--------------------------------------------------------------
-- DIY STREAM
--------------------------------------------------------------

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
  | ParagraphBreak
  deriving (Eq, Show, Functor)

type StreamNode = StreamNode' Text

unStream :: StreamNode' t -> (t -> StreamNode' t, t)
unStream = \case
  TextNode a t -> (TextNode a, t)
  CodeNode t -> (CodeNode, t)
  RawNode a t -> (RawNode a, t)
  ParagraphBreak -> error "Logic error: Paragraph break can not be unstreamed!"

-- | Get chunks of nodes not exceeding length and broken at word boundary.
--
-- The split will end when no more nodes (then words) can fit or on 'ParagraphBreak'.
chunksOf :: Int -> [StreamNode] -> [[StreamNode]]
chunksOf n = chop (splitter True n)
 where
  nodeLength :: StreamNode -> Int
  nodeLength = T.length . snd . unStream
  splitter :: Bool -> Int -> [StreamNode] -> ([StreamNode], [StreamNode])
  splitter start i = \case
    [] -> ([], [])
    (ParagraphBreak : ss) -> ([ParagraphBreak], ss)
    (tn : ss) ->
      let l = nodeLength tn
       in if l <= i
            then first (tn :) $ splitter False (i - l) ss
            else let (tn1, tn2) = cut start i tn in ([tn1], tn2 : ss)
  cut :: Bool -> Int -> StreamNode -> (StreamNode, StreamNode)
  cut start i tn =
    let (con, t) = unStream tn
     in case splitWordsAt i (T.words t) of
          ([], []) -> (con "", con "")
          ([], ws@(ww : wws)) ->
            both (con . T.unwords) $
              -- In case single word (e.g. web link) does not fit on line we must put
              -- it there and guarantee progress (otherwise chop will cycle)
              if start then ([ww], wws) else ([], ws)
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
    ParagraphBreak -> "\n"

-- | Convert elements to one dimensional stream of nodes,
-- that is easy to format and layout.
--
-- If you want to split the stream at line length, use
-- the 'chunksOf' function afterward.
class ToStream a where
  toStream :: a -> [StreamNode]

instance ToStream (Node Syntax) where
  toStream = \case
    LeafText a t -> TextNode a <$> T.lines t
    LeafCode t -> CodeNode <$> T.lines (prettyText t)
    LeafRaw s t -> RawNode s <$> T.lines t
    LeafCodeBlock _i t -> ParagraphBreak : (CodeNode <$> T.lines (prettyText t)) <> [ParagraphBreak]

instance ToStream (Paragraph Syntax) where
  toStream = concatMap toStream . nodes

instance ToStream (Document Syntax) where
  toStream = List.intercalate [ParagraphBreak] . map toStream . paragraphs
