{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parsing Documents from Markdown source, with embedded Swarm source.
module Swarm.Text.Markdown.Parse (
  -- * Commonmark parsing
  quoteMaybe,
  fromTextPure,

  -- * Markdown -> Document parsing with Swarm code processing
  parseSyntax,
  fromTextE,
  fromTextM,
  fromText,
)
where

import Commonmark qualified as Mark
import Commonmark.Extensions qualified as Mark
import Control.Applicative ((<|>))
import Control.Arrow (left)
import Control.Monad (guard)
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (toList)
import Data.Yaml
import Effectful
import Effectful.Error.Static
import GHC.Exts qualified (IsList (..), IsString (..))
import Swarm.Failure (SystemFailure)
import Swarm.Language.Parser (readTerm)
import Swarm.Language.Pipeline (processTermNoImports)
import Swarm.Language.Syntax (Phase (Raw), Syntax)
import Swarm.Pretty (prettyText)
import Swarm.Text.Markdown.Document
import Swarm.Util (showT)

-- Some Commonmark instances for tracking source spans and attributes.
-- We do not use either, so the implementations are trivial.

instance Mark.Rangeable [Node c] where
  ranged _ = id

instance Mark.HasAttributes [Node c] where
  addAttributes _ = id

instance Mark.Rangeable [Paragraph c] where
  ranged _ = id

instance Mark.HasAttributes [Paragraph c] where
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
    [] -> SimpleParagraph []
    (p : _) -> p

-- | Surround some text in double quotes if it is not empty.
quoteMaybe :: Text -> Text
quoteMaybe t = if T.null t then t else T.concat ["\"", t, "\""]

-- | This instance tells Commonmark how to parse Markdown inline elements into our custom data type.
instance Mark.IsInline [Node Text] where
  lineBreak = pure $ txt "\n"
  softBreak = pure $ txt " "
  str = pure . txt
  entity = Mark.str
  escapedChar c = Mark.str $ T.pack ['\\', c]
  emph = map $ addTextAttribute Emphasis
  strong = map $ addTextAttribute Strong
  link dest title desc = pure $ LeafLink dest (title <$ guard (title /= "")) desc
  image dest title desc = pure (txt "!") <> Mark.link dest title desc
  code = pure . LeafCode
  rawInline (Mark.Format f) = pure . LeafRaw (T.unpack f)

-- | This instance tells Commonmark how to parse Markdown block elements into our custom data type.
instance Mark.IsBlock [Node Text] [Paragraph Text] where
  paragraph = pure . SimpleParagraph
  plain = Mark.paragraph
  thematicBreak = mempty
  blockQuote = (map . mapP) (addTextAttribute Emphasis)
  codeBlock f = Mark.plain . pure . LeafCodeBlock (T.unpack f)
  heading _lvl = Mark.plain . Mark.strong
  rawBlock _ _ = mempty
  referenceLinkDefinition = mempty
  list ty spacing = pure . ListParagraph ty spacing

-- | Read a Markdown document, leaving any embedded code as @Text@.
fromTextPure :: Text -> Either Text (Document Text)
fromTextPure t = do
  let spec =
        mconcat
          [ Mark.fancyListSpec
          , Mark.rawAttributeSpec
          , Mark.defaultSyntaxSpec
          ]
  let runSimple = left showT . runIdentity
  fmap Document . runSimple $ Mark.commonmarkWith spec "markdown" t

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
  Right (Just t) -> case runPureEff . runErrorNoCallStack @SystemFailure $ processTermNoImports s t Nothing of
    -- If typechecking produces an error, just pretty-print the error message.
    Left e -> Left (prettyText @SystemFailure e)
    -- ...but if it does, we throw away the type-annotated +
    -- elaborated AST, and just go back to using the original parsed
    -- (*unelaborated*) AST.  See #1496.
    Right _ -> Right t

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
    LeafLink a b c -> LeafLink a b (map processNode c)
