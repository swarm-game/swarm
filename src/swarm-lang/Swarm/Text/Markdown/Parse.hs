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
import Commonmark.Extensions qualified as Mark (rawAttributeSpec)
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
  link dest title desc = pureP $ LeafLink dest (title <$ guard (title /= "")) desc
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
    LeafLink a b c -> LeafLink a b (mapP processNode c)
