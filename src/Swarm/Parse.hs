{-# LANGUAGE OverloadedStrings #-}

module Swarm.Parse where

import           Data.Bifunctor
import           Data.Text
import           Data.Void
import           Witch

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec                hiding (State, runParser)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Swarm.AST

type Parser = Parsec Void Text

--------------------------------------------------
-- Lexer

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserved :: Text -> Parser ()
reserved w = (lexeme . try) $ string' w *> notFollowedBy alphaNumChar

integer :: Parser Integer
integer = lexeme L.decimal

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

--------------------------------------------------
-- Parser

parseDirection :: Parser Direction
parseDirection =
      Lt     <$ reserved "left"
  <|> Rt     <$ reserved "right"
  <|> Back   <$ reserved "back"
  <|> Fwd    <$ reserved "forward"
  <|> North  <$ reserved "north"
  <|> South  <$ reserved "south"
  <|> East   <$ reserved "east"
  <|> West   <$ reserved "west"

parseConst :: Parser Const
parseConst =
      Wait <$ reserved "wait"
  <|> Move <$ reserved "move"
  <|> Turn <$ reserved "turn"
  <|> Harvest <$ reserved "harvest"
  <|> Repeat <$ reserved "repeat"
  <|> Build <$ reserved "build"

parseTermAtom :: Parser Term
parseTermAtom =
      TConst <$> parseConst
  <|> TDir   <$> parseDirection
  <|> TInt   <$> integer
  <|> parens parseTerm
  <|> TNop <$ try (symbol "{" *> symbol "}")
  <|> braces parseTerm

parseTerm :: Parser Term
parseTerm = makeExprParser parseTermAtom table
  where
    table =
      [ [ InfixL (TApp <$ string "") ]
      , [ InfixR (TBind <$ symbol ";") ]
      ]

--------------------------------------------------
-- Utilities

runParser :: Parser a -> Text -> Either Text a
runParser p t = first (from . errorBundlePretty) (parse p "" t)

readTerm :: Text -> Either Text Term
readTerm = runParser parseTerm
