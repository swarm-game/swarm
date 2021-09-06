{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Parse
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parser for the Swarm language.  Note, you probably don't want to
-- use this directly, unless there is a good reason to parse a term
-- without also type checking it; use 'Swarm.Language.Pipeline.processTerm'
-- or 'Swarm.Language.Pipeline.processCmd' instead, which parse,
-- typecheck, and elaborate a term all at once.
--
-----------------------------------------------------------------------------

module Swarm.Language.Parse
  ( -- * Parsers

    parseType, parseTerm

    -- * Utility functions

  , runParser, readTerm

  ) where

import           Data.Bifunctor
import           Data.Char
import qualified Data.Map                       as M
import           Data.Text                      (Text)
import           Data.Void
import           Witch

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec                hiding (State, runParser)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Swarm.Language.Syntax
import           Swarm.Language.Types

type Parser = Parsec Void Text

--------------------------------------------------
-- Lexer

-- | List of reserved words that cannot be used as variable names.
reservedWords :: [String]
reservedWords =
  [ "left", "right", "back", "forward", "north", "south", "east", "west"
  , "wait", "halt", "move", "turn", "grab", "place", "give"
  , "build", "run", "getx", "gety"
  , "random", "say", "view", "appear", "ishere"
  , "int", "string", "dir", "bool", "cmd"
  , "let", "def", "in", "if", "true", "false", "fst", "snd"
  ]

-- | Skip spaces and comments.
sc :: Parser ()
sc = L.space
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

-- | Parse a case-insensitive reserved word, making sure it is not a
--   prefix of a longer variable name, and allowing the parser to
--   backtrack if it fails.
reserved :: Text -> Parser ()
reserved w = (lexeme . try) $ string' w *> notFollowedBy alphaNumChar

-- | Parse an identifier, i.e. any non-reserved string containing
--   alphanumeric characters and underscores and not starting with a
--   number.
identifier :: Parser Text
identifier = (lexeme . try) (p >>= check) <?> "variable name"
  where
    p = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
    check x
      | map toLower x `elem` reservedWords
      = fail $ "reserved word " ++ x ++ " cannot be used as variable name"
      | otherwise = return (into @Text x)

-- | Parse a string literal (including escape sequences) in double quotes.
stringLiteral :: Parser Text
stringLiteral = into <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

-- | Parse a positive integer literal token.  Note that negation is
--   handled as a separate operator.
integer :: Parser Integer
integer = lexeme L.decimal

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

--------------------------------------------------
-- Parser

-- | Parse a Swarm language type.
parseType :: Parser Type
parseType = makeExprParser parseTypeAtom table
  where
    table =
      [ [ InfixR ((:->:) <$ symbol "->") ]
      , [ InfixR ((:*:) <$ symbol "*") ]
      ]

parseTypeAtom :: Parser Type
parseTypeAtom =
      TyUnit   <$ symbol "()"
  <|> TyInt    <$ reserved "int"
  <|> TyString <$ reserved "string"
  <|> TyDir    <$ reserved "dir"
  <|> TyBool   <$ reserved "bool"
  <|> TyCmd'   <$> (reserved "cmd" *> parseTypeAtom)
               <*> (maybe M.empty M.fromList <$>
                      optional (brackets (parseTyAnn `sepBy` symbol ","))
                   )
  <|> parens parseType

  where
    parseTyAnn = (,) <$> identifier <*> (symbol ":" *> parseType)

parseDirection :: Parser Direction
parseDirection =
      Lft    <$ reserved "left"
  <|> Rgt    <$ reserved "right"
  <|> Back   <$ reserved "back"
  <|> Fwd    <$ reserved "forward"
  <|> North  <$ reserved "north"
  <|> South  <$ reserved "south"
  <|> East   <$ reserved "east"
  <|> West   <$ reserved "west"

parseConst :: Parser Const
parseConst =
      Wait    <$ reserved "wait"
  <|> Halt    <$ reserved "halt"
  <|> Return  <$ reserved "return"
  <|> Move    <$ reserved "move"
  <|> Turn    <$ reserved "turn"
  <|> Grab    <$ reserved "grab"
  <|> Place   <$ reserved "place"
  <|> Give    <$ reserved "give"
  <|> Craft   <$ reserved "craft"
  <|> Build   <$ reserved "build"
  <|> Run     <$ reserved "run"
  <|> GetX    <$ reserved "getx"
  <|> GetY    <$ reserved "gety"
  <|> Random  <$ reserved "random"
  <|> Say     <$ reserved "say"
  <|> View    <$ reserved "view"
  <|> Appear  <$ reserved "appear"
  <|> IsHere  <$ reserved "ishere"
  <|> If      <$ reserved "if"
  <|> Fst     <$ reserved "fst"
  <|> Snd     <$ reserved "snd"

parseTermAtom :: Parser Term
parseTermAtom =
      TUnit   <$  symbol "()"
  <|> TConst  <$> parseConst
  <|> TVar    <$> identifier
  <|> TDir    <$> parseDirection
  <|> TInt    <$> integer
  <|> TString <$> stringLiteral
  <|> TBool   <$> ((True <$ reserved "true") <|> (False <$ reserved "false"))
  <|> TLam    <$> (symbol "\\" *> identifier)
              <*> optional (symbol ":" *> parseType)
              <*> (symbol "." *> parseTerm)
  <|> TLet    <$> (reserved "let" *> identifier)
              <*> optional (symbol ":" *> parseType)
              <*> (symbol "=" *> parseTerm)
              <*> (reserved "in" *> parseTerm)
  <|> TDef    <$> (reserved "def" *> identifier)
              <*> optional (symbol ":" *> parseType)
              <*> (symbol "=" *> parseTerm)
  <|> parens parseTerm
  <|> TConst Noop <$ try (symbol "{" *> symbol "}")
  <|> braces parseTerm

-- | Parse a Swarm language term.
parseTerm :: Parser Term
parseTerm = sepEndBy1 parseStmt (symbol ";") >>= mkBindChain

mkBindChain :: [Stmt] -> Parser Term
mkBindChain stmts = case last stmts of
  Binder _ _ -> fail "Last command in a chain must not have a binder"
  BareTerm t -> return $ foldr mkBind t (init stmts)

  where
    mkBind (BareTerm t1) t2 = TBind Nothing Nothing t1 t2
    mkBind (Binder x t1) t2 = TBind (Just x) Nothing t1 t2

data Stmt
  = BareTerm      Term
  | Binder   Text Term
  deriving (Eq, Ord, Show)

parseStmt :: Parser Stmt
parseStmt =
  mkStmt <$> optional (try (identifier <* symbol "<-")) <*> parseExpr

mkStmt :: Maybe Text -> Term -> Stmt
mkStmt Nothing  = BareTerm
mkStmt (Just x) = Binder x

parseExpr :: Parser Term
parseExpr = makeExprParser parseTermAtom table
  where
    table =
      [ [ InfixL (TApp Nothing <$ string "") ]
      , [ InfixR (mkOp (Arith Exp) <$ symbol "^") ]
      , [ Prefix (TApp Nothing (TConst (Arith Neg)) <$ symbol "-") ]
      , [ InfixL (mkOp (Arith Mul) <$ symbol "*")
        , InfixL (mkOp (Arith Div) <$ symbol "/")
        ]
      , [ InfixL (mkOp (Arith Add) <$ symbol "+")
        , InfixL (mkOp (Arith Sub) <$ symbol "-")
        ]
      , map (\(s, op) -> InfixN (mkOp (Cmp op) <$ symbol s))
        [ ("==", CmpEq)
        , ("/=", CmpNeq)
        , ("<=", CmpLeq)
        , (">=", CmpGeq)
        , ("<", CmpLt)
        , (">", CmpGt)
        ]
      , [ InfixR (TPair <$ symbol ",") ]
      ]

mkOp :: Const -> Term -> Term -> Term
mkOp c = TApp Nothing . TApp Nothing (TConst c)

--------------------------------------------------
-- Utilities

-- | Run a parser on some input text, returning either the result or a
--   pretty-printed parse error message.
runParser :: Parser a -> Text -> Either Text a
runParser p t = first (from . errorBundlePretty) (parse p "" t)

-- | Run a parser "fully", consuming leading whitespace and ensuring
--   that the parser extends all the way to eof.
fully :: Parser a -> Parser a
fully p = sc *> p <* eof

-- | Parse some input 'Text' completely as a 'Term', consuming leading
--   whitespace and ensuring the parsing extends all the way to the
--   end of the input 'Text'.  Returns either the resulting 'Term' or
--   a pretty-printed parse error message.
readTerm :: Text -> Either Text Term
readTerm = runParser (fully parseTerm)
