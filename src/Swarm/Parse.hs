{-# LANGUAGE OverloadedStrings #-}

module Swarm.Parse where

import           Data.Bifunctor
import           Data.Text
import           Data.Void
import           Witch

import           Text.Megaparsec            hiding (State, runParser)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

parseCommand :: Parser Command
parseCommand =
      Wait    <$  reserved "wait"
  <|> Move    <$  reserved "move"
  <|> Turn    <$> (reserved "turn" *> parseDirection)
  <|> Harvest <$  reserved "harvest"
  <|> Block   <$> braces parseProgram
  <|> Repeat  <$> (reserved "repeat" *> integer) <*> parseCommand
  <|> Build   <$> (reserved "build" *> parseCommand)

parseDirection :: Parser Direction
parseDirection =
      Lt    <$ reserved "left"
  <|> Rt    <$ reserved "right"
  <|> North <$ reserved "north"
  <|> South <$ reserved "south"
  <|> East  <$ reserved "east"
  <|> West  <$ reserved "west"

parseProgram :: Parser Program
parseProgram = sepEndBy parseCommand (symbol ";")

--------------------------------------------------
-- Utilities

runParser :: Parser a -> Text -> Either Text a
runParser p t = first (from . errorBundlePretty) (parse p "" t)

readCommand :: Text -> Either Text Command
readCommand = runParser parseCommand
