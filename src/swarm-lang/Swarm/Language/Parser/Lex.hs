{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Token lexing and comment preservation for the Swarm language.
module Swarm.Language.Parser.Lex (
  -- * Parsing with source locations
  parseLoc,
  parseLocG,

  -- * Whitespace + comments
  getCommentSituation,
  lineComment,
  blockComment,
  sc,

  -- * Tokens

  -- ** Lexemes
  lexeme,

  -- ** Specific token types
  symbol,
  reservedWords,
  reserved,
  identifier,
  locIdentifier,
  textLiteral,
  integer,

  -- ** Combinators
  braces,
  parens,
  brackets,
) where

import Control.Lens (use, (%=), (.=))
import Control.Monad (void)
import Data.Sequence qualified as Seq
import Data.Text (Text, toLower)
import Swarm.Language.Parser.Core
import Swarm.Language.Syntax
import Swarm.Util (failT, squote)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Witch (into)

------------------------------------------------------------
-- Parsing with source locations

-- | Add 'SrcLoc' to a parser
parseLocG :: Parser a -> Parser (SrcLoc, a)
parseLocG pa = do
  start <- getOffset
  a <- pa
  end <- getOffset
  pure (SrcLoc start end, a)

-- | Add 'SrcLoc' to a 'Term' parser
parseLoc :: Parser Term -> Parser Syntax
parseLoc pterm = uncurry Syntax <$> parseLocG pterm

------------------------------------------------------------
-- Whitespace

-- Approach for preserving comments taken from https://www.reddit.com/r/haskell/comments/ni4gpm/comment/gz0ipmp/

-- | If we see a comment starting now, is it the first non-whitespace
--   thing on the current line so far, or were there other
--   non-whitespace tokens previously?
getCommentSituation :: Parser CommentSituation
getCommentSituation = do
  fl <- use freshLine
  return $ if fl then StandaloneComment else SuffixComment

-- | Parse a line comment, while appending it out-of-band to the list of
--   comments saved in the custom state.
lineComment :: Text -> Parser ()
lineComment start = do
  cs <- getCommentSituation
  (loc, t) <- parseLocG $ do
    string start *> takeWhileP (Just "character") (/= '\n')
  comments %= (Seq.|> Comment loc LineComment cs t)

-- | Parse a block comment, while appending it out-of-band to the list of
--   comments saved in the custom state.
blockComment :: Text -> Text -> Parser ()
blockComment start end = do
  cs <- getCommentSituation
  (loc, t) <- parseLocG $ do
    void $ string start
    manyTill anySingle (string end)
  comments %= (Seq.|> Comment loc BlockComment cs (into @Text t))

-- | Skip spaces and comments.
sc :: Parser ()
sc =
  -- Typically we would use L.space here, but we have to inline its
  -- definition and use our own slight variant, since we need to treat
  -- end-of-line specially.
  skipMany . choice . map hidden $
    [ hspace1
    , eol *> (freshLine .= True) -- If we see a newline, reset freshLine to True.
    , lineComment "//"
    , blockComment "/*" "*/"
    ]

------------------------------------------------------------
-- Tokens

-- | In general, we follow the convention that every token parser
--   assumes no leading whitespace and consumes all trailing
--   whitespace.  Concretely, we achieve this by wrapping every token
--   parser using 'lexeme'.
--
--   Also sets freshLine to False every time we see a non-whitespace
--   token.
lexeme :: Parser a -> Parser a
lexeme p = (freshLine .= False) *> L.lexeme sc p

-- | A lexeme consisting of a literal string.
symbol :: Text -> Parser Text
symbol s = (freshLine .= False) *> L.symbol sc s

-- | List of reserved words that cannot be used as variable names.
reservedWords :: [Text]
reservedWords =
  map (syntax . constInfo) (filter isUserFunc allConst)
    ++ map directionSyntax allDirs
    ++ [ "void"
       , "unit"
       , "int"
       , "text"
       , "dir"
       , "bool"
       , "actor"
       , "key"
       , "cmd"
       , "delay"
       , "let"
       , "def"
       , "end"
       , "in"
       , "true"
       , "false"
       , "forall"
       , "require"
       , "requirements"
       ]

-- | Parse a case-insensitive reserved word, making sure it is not a
--   prefix of a longer variable name, and allowing the parser to
--   backtrack if it fails.
reserved :: Text -> Parser ()
reserved w = (lexeme . try) $ string' w *> notFollowedBy (alphaNumChar <|> char '_')

-- | Parse an identifier, i.e. any non-reserved string containing
--   alphanumeric characters and underscores and not starting with a
--   number.
identifier :: Parser Var
identifier = lvVar <$> locIdentifier

-- | Parse an identifier together with its source location info.
locIdentifier :: Parser LocVar
locIdentifier = uncurry LV <$> parseLocG ((lexeme . try) (p >>= check) <?> "variable name")
 where
  p = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_' <|> char '\'')
  check (into @Text -> t)
    | toLower t `elem` reservedWords =
        failT ["reserved word", squote t, "cannot be used as variable name"]
    | otherwise = return t

-- | Parse a text literal (including escape sequences) in double quotes.
textLiteral :: Parser Text
textLiteral = into <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

-- | Parse a positive integer literal token, in decimal, binary,
--   octal, or hexadecimal notation.  Note that negation is handled as
--   a separate operator.
integer :: Parser Integer
integer =
  label "integer literal" $
    lexeme $ do
      n <-
        string "0b"
          *> L.binary
          <|> string "0o"
            *> L.octal
          <|> string "0x"
            *> L.hexadecimal
          <|> L.decimal
      notFollowedBy alphaNumChar
      return n

------------------------------------------------------------
-- Combinators

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")
