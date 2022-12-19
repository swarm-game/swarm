{-# LANGUAGE FlexibleContexts #-}

module Data.BoolExpr.Parser (
  -- * Parsing function
  parseBoolExpr,

  -- * Language definition and components
  languageDef,
  lexer,
  identifier,
  whiteSpace,
  symbol,
)
where

import Control.Monad
import Data.BoolExpr
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P

-- | Parse a search query as a boolean tree using the following grammar.
--     Note that this parser is parameterized over the parser of query simple
--     terms (const).
--
-- @
--  bt ::= bt AND bt
--        | bt bt -- same as AND
--        | bt OR bt
--        | - bt
--        | NOT bt
--        | ( bt )
--        | const
--   const ::= \<given as argument\>
-- @
parseBoolExpr :: CharParser st a -> CharParser st (BoolExpr a)
parseBoolExpr parseConst = disj
 where
  disj = conj `chainl1` orOp
  conj = factor `chainl1` andOp
  factor =
    ( (symbol "-" >> return BNot)
        <|> (symbol "NOT" >> return BNot)
        <|> (return id)
    )
      `ap` ( parens disj
              <|> BConst
              `fmap` (Positive `fmap` parseConst)
           )

  andOp = BAnd <$ option "" (symbol "AND")
  orOp = BOr <$ symbol "OR"

-- | Underlying lexer of 'languageDef'
lexer :: P.TokenParser st
lexer = P.makeTokenParser languageDef

-- | Shorthand for 'P.parens lexer'.
parens :: CharParser st a -> CharParser st a
parens = P.parens lexer

-- | Shorthand for 'P.symbol' 'lexer'.
symbol :: String -> CharParser st String
symbol = P.symbol lexer

-- | Shorthand for 'P.whiteSpace' 'lexer'.
whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

-- | Shorthand for 'P.identifier' 'lexer'.
identifier :: CharParser st String
identifier = do
  str <- P.identifier lexer
  pure str

wordLetter :: CharParser st Char
wordLetter = alphaNum <|> oneOf "_:;`,~@.!#$%^&*=+?|\\{}[]<>"

-- | Basic language definition for search queries.
-- Reserved names are @\"AND\"@ @\"OR\"@ and @\"-\"@.
-- Identifiers accepts almost every ASCII sequences without blanks nor @\'-\'@.
languageDef :: P.LanguageDef st
languageDef =
  P.LanguageDef
    { P.commentStart = ""
    , P.commentEnd = ""
    , P.commentLine = ""
    , P.nestedComments = True
    , P.identStart = wordLetter
    , P.identLetter = wordLetter <|> char '-'
    , P.opStart = mzero
    , P.opLetter = mzero
    , P.reservedOpNames = []
    , P.reservedNames = ["AND", "OR", "-"]
    , P.caseSensitive = True
    }
