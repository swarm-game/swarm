{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parsing records in the Swarm language.
module Swarm.Language.Parser.Import (
  parseImportLocation,
) where

import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Swarm.Language.Parser.Core (Parser)
import Swarm.Language.Parser.Lex (lexeme)
import Swarm.Language.Syntax.Import (Anchor (..), ImportLoc (..), mkImportDir)
import Swarm.Language.Syntax.Import qualified as Import
import Text.Megaparsec hiding (sepBy1)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Witch (into)

-- | Parse an import location --- something like "~/foo/bar/baz.sw",
--   or "https://github.com/bar/baz", etc.
parseImportLocation :: Parser (ImportLoc Import.Raw)
parseImportLocation =
  lexeme . between (char '"') (char '"') $ do
    anchor <- parseAnchor
    cs <- importComponent `sepBy1` separator
    pure $ ImportLoc (mkImportDir anchor (NE.init cs)) (NE.last cs)
 where
  importComponent :: Parser Text
  importComponent = into @Text <$> someTill L.charLiteral (lookAhead (oneOf ("\"/\\" :: [Char])))

  separator :: Parser Char
  separator = oneOf ['/', '\\']

  parseAnchor :: Parser (Anchor Import.Raw)
  parseAnchor =
    (Absolute <$ separator)
      <|> (Home <$ (char '~' *> separator))
      <|> (Web <$> parseWeb)
      <|> pure (Local 0)

  parseWeb :: Parser Text
  parseWeb =
    mconcat
      <$> sequenceA [scheme, string "://", into @Text <$> manyTill L.charLiteral separator]

  scheme :: Parser Text
  scheme = string "https" <|> string "http"
