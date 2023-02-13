{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Markdown where

import CMarkGFM qualified as CMark
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml
import Swarm.Language.Module (moduleAST)
import Swarm.Language.Parse (readTerm)
import Swarm.Language.Pipeline (ProcessedTerm (..), prettyTypeErr, processParsedTerm)
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax (Syntax)

data Node
  = Node NodeType [Node]
  | LeafText Text
  | LeafCode Syntax
  | LeafCodeBlock String Syntax
  deriving (Eq, Show)

data NodeType
  = Document
  | Paragraph
  deriving (Eq, Show)

instance ToJSON Node where
  toJSON = String . toText

instance FromJSON Node where
  parseJSON v =
    withArray "markdown paragraphs" (foldMap parseJSON) v
      <|> withText "markdown text" fromTextM v
instance Semigroup Node where
  Node Document cs1 <> Node Document cs2 = Node Document (cs1 <> cs2)
  Node Document cs1 <> n2 = Node Document (cs1 <> [n2])
  n1 <> Node Document cs2 = Node Document (n1 : cs2)
  n1 <> n2 = Node Document [n1, n2]

instance Monoid Node where
  mempty = Node Document []

fromText :: Text -> Node
fromText = either error id . fromTextE

fromTextM :: MonadFail m => Text -> m Node
fromTextM = either fail pure . fromTextE

fromTextE :: Text -> Either String Node
fromTextE = fromGeneralNode . CMark.commonmarkToNode [CMark.optSourcePos] []

-- TODO: use transformer kind of Error? (report all errors)
fromGeneralNode :: CMark.Node -> Either String Node
fromGeneralNode (CMark.Node p t cs) = case t of
  CMark.DOCUMENT -> Node Document <$> mapM fromGeneralNode cs
  CMark.PARAGRAPH -> Node Paragraph <$> mapM fromGeneralNode cs
  CMark.SOFTBREAK -> leaf $ LeafText "\n" -- ?
  CMark.LINEBREAK -> leaf $ LeafText "\n"
  CMark.TEXT txt -> leaf $ LeafText txt
  CMark.CODE txt -> leaf' $ LeafCode <$> parse txt
  CMark.CODE_BLOCK i txt -> leaf' $ LeafCodeBlock (T.unpack i) <$> parse txt
  _ -> Left $ "unsupported markdown node at " <> maybe "???" show p <> ":" <> show t
 where
  parse txt =
    readTerm txt & \case
      Left e -> Left (T.unpack e)
      Right Nothing -> Left "empty code"
      Right (Just s) -> case processParsedTerm s of
        Left e -> Left (T.unpack $ prettyTypeErr txt e)
        Right (ProcessedTerm modul _req _reqCtx) -> Right . void $ moduleAST modul
  leaf' = if null cs then id else const . Left $ "Not a leaf node " <> show t <> ": " <> show cs
  leaf = leaf' . pure

toText :: Node -> Text
toText = streamToText . toStream

data StreamNode
  = TextNode Text
  | CodeNode Syntax
  | ParagraphBreak
  deriving (Eq, Show)

isText :: StreamNode -> Bool
isText = \case
  TextNode _ -> True
  _ -> False

unText :: StreamNode -> Text
unText (TextNode txt) = txt
unText _ = error "logical error: expected Text node"

streamToText :: [StreamNode] -> Text
streamToText = T.concat . map nodeToText
 where
  nodeToText = \case
    TextNode txt -> txt
    CodeNode stx -> prettyText stx
    ParagraphBreak -> "\n"

toStream :: Node -> [StreamNode]
toStream = mergeText . toStreamLines

toStreamLines :: Node -> [StreamNode]
toStreamLines = \case
  LeafText txt -> [TextNode txt]
  LeafCode txt -> [CodeNode txt]
  LeafCodeBlock _i txt -> [TextNode "\n", CodeNode txt, TextNode "\n"]
  Node Document cs -> concatMap toStreamLines cs
  Node Paragraph cs -> {-ParagraphBreak :-} concatMap toStreamLines cs <> [ParagraphBreak]

mergeText :: [StreamNode] -> [StreamNode]
mergeText = \case
  [] -> []
  [ParagraphBreak] -> []
  (CodeNode txt : s) -> CodeNode txt : mergeText s
  -- (ParagraphBreak:ParagraphBreak:s) -> mergeText (ParagraphBreak:s)
  -- (ParagraphBreak:s) -> ParagraphBreak : mergeText s
  s -> let (ts, r) = span isText s in TextNode (T.concat $ map unText ts) : mergeText r
