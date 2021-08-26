{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Swarm.Parse where

import           Data.Bifunctor
import           Data.Char
import           Data.Text                      (Text)
import           Data.Void
import           Witch

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec                hiding (State, runParser)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Swarm.AST
import           Swarm.Types

type Parser = Parsec Void Text

--------------------------------------------------
-- Lexer

reservedWords :: [String]
reservedWords =
  [ "left", "right", "back", "forward", "north", "south", "east", "west"
  , "wait", "move", "turn", "harvest", "repeat", "build", "run"
  , "int", "string", "dir", "cmd"
  , "let", "in"
  ]

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

-- | Parse an identifier, i.e. any non-reserved string containing
--   alphanumeric characters and not starting with a number.
identifier :: Parser Text
identifier = (lexeme . try) (p >>= check) <?> "variable name"
  where
    p = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
    check x
      | map toLower x `elem` reservedWords
      = fail $ "reserved word " ++ x ++ " cannot be used as variable name"
      | otherwise = return (into @Text x)

stringLiteral :: Parser Text
stringLiteral = into <$> (char '"' >> manyTill L.charLiteral (char '"'))

integer :: Parser Integer
integer = lexeme L.decimal

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

--------------------------------------------------
-- Parser

parseType :: Parser Type
parseType = makeExprParser parseTypeAtom table
  where
    table =
      [ [ InfixR ((:->:) <$ symbol "->") ]
      ]

parseTypeAtom :: Parser Type
parseTypeAtom =
      TyUnit   <$ symbol "()"
  <|> TyInt    <$ reserved "int"
  <|> TyString <$ reserved "string"
  <|> TyDir    <$ reserved "dir"
  <|> TyCmd    <$ reserved "cmd"

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
      Wait    <$ reserved "wait"
  <|> Move    <$ reserved "move"
  <|> Turn    <$ reserved "turn"
  <|> Harvest <$ reserved "harvest"
  <|> Repeat  <$ reserved "repeat"
  <|> Build   <$ reserved "build"
  <|> Run     <$ reserved "run"

parseTermAtom :: Parser Term
parseTermAtom =
      TUnit   <$  symbol "()"
  <|> TConst  <$> parseConst
  <|> TVar    <$> identifier
  <|> TDir    <$> parseDirection
  <|> TInt    <$> integer
  <|> TString <$> stringLiteral
  <|> TLam    <$> (symbol "\\" *> identifier)
              <*> optional (symbol ":" *> parseType)
              <*> (symbol "." *> parseTerm)
  <|> TLet    <$> (reserved "let" *> identifier)
              <*> optional (symbol ":" *> parseType)
              <*> (symbol "=" *> parseTerm)
              <*> (reserved "in" *> parseTerm)
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
