{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parsing records in the Swarm language.
module Swarm.Language.Parser.Import (
  parseImportLocation,
  parseImportLocationRaw,
) where

import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Data.Functor (void)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Swarm.Language.Parser.Core (Parser)
import Swarm.Language.Parser.Lex (lexeme)
import Swarm.Language.Syntax.Import (Anchor, ImportLoc (..), mkImportDir)
import Swarm.Language.Syntax.Import qualified as Import
import Text.Megaparsec hiding (sepBy1)
import Text.Megaparsec.Char (char, letterChar, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Witch (into)

-- | Parse an import location --- something like "~/foo/bar/baz.sw",
--   or "https://github.com/bar/baz", etc.
parseImportLocation :: Parser (ImportLoc Import.Raw)
parseImportLocation = lexeme . between (char '"') (char '"') $ parseImportLocationRaw

-- | Parse an import location, without the double quotes.
parseImportLocationRaw :: Parser (ImportLoc Import.Raw)
parseImportLocationRaw = do
  anchor <- parseAnchor
  cs <- importComponent `sepBy1` separator
  pure $ ImportLoc (mkImportDir anchor (NE.init cs)) (NE.last cs)
 where
  importComponent :: Parser Text
  importComponent = into @Text <$> someTill L.charLiteral (eof <|> void (lookAhead (oneOf ("\"/\\" :: [Char]))))

  separator :: Parser Char
  separator = oneOf ['/', '\\']

  parseAnchor :: Parser (Anchor Import.Raw)
  parseAnchor =
    (Import.Root <$ separator)
      <|> try (Import.Drive <$> (letterChar <* char ':' <* separator))
      <|> try (Import.Swarm <$ (string "~swarm" *> separator))
      <|> (Import.Home <$ (char '~' *> separator))
      <|> (Import.Web <$> parseWeb)
      <|> pure (Import.Local 0)

  parseWeb :: Parser Text
  parseWeb =
    mconcat
      <$> sequenceA [scheme, string "://", into @Text <$> manyTill L.charLiteral separator]

  scheme :: Parser Text
  scheme = string "https" <|> string "http"
