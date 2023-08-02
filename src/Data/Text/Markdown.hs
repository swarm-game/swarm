{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Text.Markdown where

import Commonmark qualified as Mark
import Commonmark.Extensions qualified as Mark (rawAttributeSpec)
import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml
import Swarm.Language.Module (moduleAST)
import Swarm.Language.Parse (readTerm)
import Swarm.Language.Pipeline (ProcessedTerm (..), processParsedTerm)
import Swarm.Language.Pretty (prettyText, prettyTypeErrText)
import Swarm.Language.Syntax (Syntax)
import Data.Maybe (catMaybes)
import Data.List qualified as List
import Data.Vector ( toList )
import Data.Set (Set)
import Data.Set qualified as Set
import Control.Arrow (left)
import Control.Applicative ((<|>))
import Data.Functor.Identity (Identity(..))

newtype Document c = Document { paragraphs :: [Paragraph c] }
  deriving (Eq, Show, Functor, Foldable, Traversable)
  deriving (Semigroup, Monoid) via [Paragraph c]

newtype Paragraph c = Paragraph { nodes :: [Node c] }
  deriving (Eq, Show, Functor, Foldable, Traversable)
  deriving (Semigroup, Monoid) via [Node c]

mapP :: (Node c -> Node c) -> Paragraph c -> Paragraph c
mapP f (Paragraph ns) = Paragraph (map f ns)

pureP :: Node c -> Paragraph c
pureP = Paragraph . List.singleton

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
  paragraph = Document . List.singleton
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

fromText :: Text -> Document Syntax
fromText = either error id . fromTextE

fromTextM :: MonadFail m => Text -> m (Document Syntax)
fromTextM = either fail pure . fromTextE

fromTextE :: Text -> Either String (Document Syntax)
fromTextE t = do
  let spec = Mark.rawAttributeSpec <> Mark.defaultSyntaxSpec <> Mark.rawAttributeSpec
  let runSimple = left show . runIdentity
  (docT :: Document Text) <- runSimple $ Mark.commonmarkWith spec "markdown" t
  traverse parseSyntax docT

toText :: ToStream a => a -> Text
toText = streamToText . toStream
data StreamNode
  = TextNode (Set TxtAttr) Text
  | CodeNode Syntax
  | RawNode String Text
  | ParagraphBreak
  deriving (Eq, Show)

isText :: StreamNode -> Bool
isText = \case
  TextNode _ _ -> True
  _ -> False

unText :: StreamNode -> Text
unText (TextNode _a t) = t
unText _ = error "logical error: expected Text node"

streamToText :: [StreamNode] -> Text
streamToText = T.concat . map nodeToText
 where
  nodeToText = \case
    TextNode _a t -> t
    RawNode _s t -> t
    CodeNode stx -> prettyText stx
    ParagraphBreak -> "\n"

class ToStream a where
  toStream :: a -> [StreamNode]

instance ToStream (Node Syntax) where
  toStream = \case
    LeafText a t -> [TextNode a t]
    LeafCode t -> [CodeNode t]
    LeafRaw s t -> [RawNode s t]
    LeafCodeBlock _i t -> [TextNode mempty "\n", CodeNode t, TextNode mempty "\n"]

instance ToStream (Paragraph Syntax) where
  toStream = concatMap toStream . nodes

instance ToStream (Document Syntax) where
  toStream = List.intercalate [ParagraphBreak] . map toStream . paragraphs
