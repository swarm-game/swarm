{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
  , "wait", "move", "turn", "harvest", "build", "run", "getx", "gety"
  , "int", "string", "dir", "cmd"
  , "let", "in", "if", "true", "false"
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
  <|> TyCmd    <$> (reserved "cmd" *> parseTypeAtom)
  <|> parens parseType

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
  <|> Move    <$ reserved "move"
  <|> Turn    <$ reserved "turn"
  <|> Harvest <$ reserved "harvest"
  <|> Build   <$ reserved "build"
  <|> Run     <$ reserved "run"
  <|> GetX    <$ reserved "getx"
  <|> GetY    <$ reserved "gety"
  <|> If      <$ reserved "if"

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
  <|> parens parseTerm
  <|> TConst Noop <$ try (symbol "{" *> symbol "}")
  <|> braces parseTerm

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
      ]

mkOp :: Const -> Term -> Term -> Term
mkOp c = TApp Nothing . TApp Nothing (TConst c)

--------------------------------------------------
-- Utilities

runParser :: Parser a -> Text -> Either Text a
runParser p t = first (from . errorBundlePretty) (parse p "" t)

readTerm :: Text -> Either Text Term
readTerm = runParser (parseTerm <* eof)
