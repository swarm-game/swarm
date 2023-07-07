{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parser for the Swarm world description DSL.
module Swarm.Game.World.Parse where

import Control.Monad (MonadPlus, void)
import Control.Monad.Combinators.Expr (makeExprParser)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Swarm.Game.Terrain (TerrainType)
import Swarm.Game.World.Syntax
import Swarm.Util (failT, squote)
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Witch (into)

type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void

------------------------------------------------------------
-- Utility

sepByNE :: (MonadPlus m) => m a -> m sep -> m (NonEmpty a)
sepByNE p sep = NE.fromList <$> p `sepBy1` sep

------------------------------------------------------------
-- Lexing

reservedWords :: [Text]
reservedWords = ["true", "false", "seed", "x", "y", "hash", "let", "in", "overlay", "hcat", "vcat"]

-- | Skip spaces and comments.
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

-- | In general, we follow the convention that every token parser
--   assumes no leading whitespace and consumes all trailing
--   whitespace.  Concretely, we achieve this by wrapping every token
--   parser using 'lexeme'.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | A lexeme consisting of a literal string.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | A positive integer literal token.
integerOrFloat :: Parser (Either Integer Double)
integerOrFloat =
  label "numeric literal" $
    lexeme (Right <$> try L.float <|> Left <$> L.decimal)

-- | Parse a case-insensitive reserved word, making sure it is not a
--   prefix of a longer variable name, and allowing the parser to
--   backtrack if it fails.
reserved :: Text -> Parser ()
reserved w = (lexeme . try) $ string' w *> notFollowedBy (alphaNumChar <|> char '_')

-- | Parse an identifier, i.e. any non-reserved string containing
--   alphanumeric characters and underscores and not starting with a
--   number.
identifier :: Parser Var
identifier = (lexeme . try) (p >>= check) <?> "variable name"
 where
  p = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_' <|> char '\'')
  check (into @Text -> t)
    | T.toLower t `elem` reservedWords =
        failT ["reserved word", squote t, "cannot be used as variable name"]
    | otherwise = return t

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

comma :: Parser ()
comma = void $ symbol ","

------------------------------------------------------------
-- Parser

parseWExpAtom :: Parser WExp
parseWExpAtom =
  either WInt WFloat <$> integerOrFloat
    <|> WBool <$> (True <$ reserved "true" <|> False <$ reserved "false")
    <|> parseCell
    <|> WVar <$> identifier
    <|> WSeed <$ reserved "seed"
    <|> WCoord <$> (X <$ reserved "x" <|> Y <$ reserved "y")
    <|> WHash <$ reserved "hash"
    <|> parseLet
    <|> parseOverlay
    <|> parseCat
    <|> parseStruct
    <|> parens parseWExp

parseWExp :: Parser WExp
parseWExp = makeExprParser parseWExpAtom table
 where
  table = []

parseCell :: Parser WExp
parseCell =
  braces $
    WCell
      <$> ( CellVal
              <$> (Last . Just <$> parseTerrain)
              <*> (Last <$> optional (comma *> parseName)) -- XXX 'empty'?
              <*> (fromMaybe [] <$> optional (comma *> brackets (parseName `sepBy` comma)))
          )

parseName :: Parser Text
parseName = into @Text <$> manyTill anySingle (char ',' <|> char '}')

parseTerrain :: Parser TerrainType
parseTerrain = undefined

parseLet :: Parser WExp
parseLet =
  WLet
    <$> ( reserved "let"
            *> (((,) <$> identifier <*> (symbol "=" *> parseWExp)) `sepBy` comma)
        )
    <*> (reserved "in" *> parseWExp)

parseOverlay :: Parser WExp
parseOverlay = do
  reserved "overlay"
  brackets $ WOverlay <$> parseWExp `sepByNE` comma

parseCat :: Parser WExp
parseCat =
  WCat
    <$> (X <$ reserved "hcat" <|> Y <$ reserved "vcat")
    <*> brackets (parseWExp `sepBy` comma)

parseStruct :: Parser WExp
parseStruct = reserved "struct" *> fail "struct not implemented"

------------------------------------------------------------
-- Utility

runParser :: Parser a -> Text -> Either ParserError a
runParser p = parse p ""
