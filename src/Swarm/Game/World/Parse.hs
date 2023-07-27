{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- FromJSON WExp
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parser for the Swarm world description DSL.
module Swarm.Game.World.Parse where

import Control.Monad (MonadPlus, void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Data.Yaml (FromJSON (parseJSON), withText)
import Swarm.Game.World.Syntax
import Swarm.Util (failT, showT, squote)
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
reservedWords =
  [ "not"
  , "true"
  , "false"
  , "seed"
  , "x"
  , "y"
  , "hash"
  , "let"
  , "in"
  , "overlay"
  , "hcat"
  , "vcat"
  , "if"
  , "then"
  , "else"
  , "perlin"
  , "mask"
  , "empty"
  , "abs"
  ]

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

operatorChar :: Parser Char
operatorChar = oneOf ("!@#$%^&*=+-/<>" :: String)

operator :: Text -> Parser Text
operator op = (lexeme . try) $ string op <* notFollowedBy operatorChar

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
    <|> parseIf
    <|> parsePerlin
    <|> parseAbs
    <|> parseLet
    <|> parseOverlay
    <|> parseMask
    <|> parseImport
    -- <|> parseCat
    -- <|> parseStruct
    <|> parens parseWExp

parseWExp :: Parser WExp
parseWExp =
  makeExprParser
    parseWExpAtom
    [
      [ Prefix (unary Not <$ reserved "not")
      , Prefix (unary Neg <$ operator "-")
      ]
    ,
      [ InfixL (binary Mul <$ operator "*")
      , InfixL (binary Div <$ operator "/")
      , InfixL (binary Mod <$ operator "%")
      ]
    ,
      [ InfixL (binary Add <$ operator "+")
      , InfixL (binary Sub <$ operator "-")
      , InfixR (binary Overlay <$ operator "<>")
      ]
    ,
      [ InfixN (binary Eq <$ operator "==")
      , InfixN (binary Neq <$ operator "/=")
      , InfixN (binary Lt <$ operator "<")
      , InfixN (binary Leq <$ operator "<=")
      , InfixN (binary Gt <$ operator ">")
      , InfixN (binary Geq <$ operator ">=")
      ]
    , [InfixR (binary And <$ operator "&&")]
    , [InfixR (binary Or <$ operator "||")]
    ]
 where
  unary op x = WOp op [x]
  binary op x1 x2 = WOp op [x1, x2]

parseCell :: Parser WExp
parseCell =
  braces $ WCell <$> parseCellItem `sepBy1` comma

parseCellItem :: Parser (Maybe CellTag, Text)
parseCellItem =
  (,)
    <$> optional (try (parseCellTag <* symbol ":"))
    <*> parseName

parseCellTag :: Parser CellTag
parseCellTag = choice (map mkCellTagParser [minBound .. maxBound :: CellTag])
 where
  mkCellTagParser ct = ct <$ string' (T.drop 4 $ showT ct)

parseName :: Parser Text
parseName =
  into @Text
    <$> manyTill anySingle (lookAhead (satisfy (\c -> c == ',' || c == '}' || c == ']')))

parseIf :: Parser WExp
parseIf =
  (\i t e -> WOp If [i, t, e])
    <$> (reserved "if" *> parseWExp)
    <*> (reserved "then" *> parseWExp)
    <*> (reserved "else" *> parseWExp)

parsePerlin :: Parser WExp
parsePerlin =
  (\s o k p -> WOp Perlin [s, o, k, p])
    <$> (reserved "perlin" *> parseWExpAtom)
    <*> parseWExpAtom
    <*> parseWExpAtom
    <*> parseWExpAtom

parseAbs :: Parser WExp
parseAbs =
  WOp Abs . (: []) <$> (reserved "abs" *> parseWExpAtom)

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

parseMask :: Parser WExp
parseMask = do
  reserved "mask"
  w1 <- parseWExpAtom
  w2 <- parseWExpAtom
  return $ WOp Mask [w1, w2]

parseImport :: Parser WExp
parseImport = WImport . into @Text <$> between (symbol "\"") (symbol "\"") (some (satisfy (/= '"')))

-- parseCat :: Parser WExp
-- parseCat =
--   WCat
--     <$> (X <$ reserved "hcat" <|> Y <$ reserved "vcat")
--     <*> brackets (parseWExp `sepBy` comma)

-- parseStruct :: Parser WExp
-- parseStruct = reserved "struct" *> fail "struct not implemented"

------------------------------------------------------------
-- Utility

runParser :: Parser a -> Text -> Either ParserError a
runParser p = parse p ""

------------------------------------------------------------
-- JSON instance

instance FromJSON WExp where
  parseJSON = withText "World DSL program" $ \t ->
    case runParser parseWExp t of
      Left err -> error (errorBundlePretty err)
      Right wexp -> return wexp
