{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  Swarm.Language.Parse
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parser for the Swarm language.  Note, you probably don't want to
-- use this directly, unless there is a good reason to parse a term
-- without also type checking it; use
-- 'Swarm.Language.Pipeline.processTerm' instead, which parses,
-- typechecks, elaborates, and capability checks a term all at once.
module Swarm.Language.Parse (
  -- * Parsers
  Parser,
  parsePolytype,
  parseType,
  parseTerm,

  -- * Utility functions
  runParser,
  runParserTH,
  readTerm,
  readTerm',
  showShortError,
  showErrorPos,
  getLocRange,
) where

import Control.Monad.Reader
import Data.Bifunctor
import Data.List (nub)
import qualified Data.List.NonEmpty (head)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, index, toLower)
import qualified Data.Text as T
import Data.Void
import Witch

import Control.Monad.Combinators.Expr
import qualified Data.Map.Strict as Map
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Pos as Pos

import Data.Foldable (asum)
import qualified Data.Set as S
import Data.Set.Lens (setOf)
import Swarm.Language.Syntax
import Swarm.Language.Types

-- | When parsing a term using a quasiquoter (i.e. something in the
--   Swarm source code that will be parsed at compile time), we want
--   to allow antiquoting, i.e. writing something like $x to refer to
--   an existing Haskell variable.  But when parsing a term entered by
--   the user at the REPL, we do not want to allow this syntax.
data Antiquoting = AllowAntiquoting | DisallowAntiquoting
  deriving (Eq, Ord, Show)

type Parser = ReaderT Antiquoting (Parsec Void Text)

type ParserError = ParseErrorBundle Text Void

--------------------------------------------------
-- Lexer

-- | List of reserved words that cannot be used as variable names.
reservedWords :: [Text]
reservedWords =
  map (syntax . constInfo) (filter isUserFunc allConst)
    ++ map (dirSyntax . dirInfo) allDirs
    ++ [ "int"
       , "string"
       , "dir"
       , "bool"
       , "cmd"
       , "delay"
       , "let"
       , "def"
       , "end"
       , "in"
       , "true"
       , "false"
       , "forall"
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

-- | Parse a case-insensitive reserved word, making sure it is not a
--   prefix of a longer variable name, and allowing the parser to
--   backtrack if it fails.
reserved :: Text -> Parser ()
reserved w = (lexeme . try) $ string' w *> notFollowedBy (alphaNumChar <|> char '_')

-- | Parse an identifier, i.e. any non-reserved string containing
--   alphanumeric characters and underscores and not starting with a
--   number.
identifier :: Parser Text
identifier = (lexeme . try) (p >>= check) <?> "variable name"
 where
  p = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
  check s
    | toLower t `elem` reservedWords =
      fail $ "reserved word '" ++ s ++ "' cannot be used as variable name"
    | otherwise = return t
   where
    t = into @Text s

-- | Parse a string literal (including escape sequences) in double quotes.
stringLiteral :: Parser Text
stringLiteral = into <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

-- | Parse a positive integer literal token.  Note that negation is
--   handled as a separate operator.
integer :: Parser Integer
integer = lexeme L.decimal

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- dbraces :: Parser a -> Parser a
-- dbraces = between (symbol "{{") (symbol "}}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

--------------------------------------------------
-- Parser

-- | Parse a Swarm language polytype, which starts with an optional
--   quanitifation (@forall@ followed by one or more variables and a
--   period) followed by a type.  Note that anything accepted by
--   'parseType' is also accepted by 'parsePolytype'.
parsePolytype :: Parser Polytype
parsePolytype =
  join $
    quantify
      <$> (fromMaybe [] <$> optional (reserved "forall" *> some identifier <* symbol "."))
      <*> parseType
 where
  quantify :: [Var] -> Type -> Parser Polytype
  quantify xs ty
    -- Iplicitly quantify over free type variables if the user didn't write a forall
    | null xs = return $ Forall (S.toList free) ty
    -- Otherwise, require all variables to be explicitly quantified
    | S.null free = return $ Forall xs ty
    | otherwise =
      fail $
        unlines
          [ "  Type contains free variable(s): " ++ unwords (map from (S.toList free))
          , "  Try adding them to the 'forall'."
          ]
   where
    free = tyVars ty `S.difference` S.fromList xs

-- | Parse a Swarm language (mono)type.
parseType :: Parser Type
parseType = makeExprParser parseTypeAtom table
 where
  table =
    [ [InfixR ((:*:) <$ symbol "*")]
    , [InfixR ((:+:) <$ symbol "+")]
    , [InfixR ((:->:) <$ symbol "->")]
    ]

parseTypeAtom :: Parser Type
parseTypeAtom =
  TyUnit <$ symbol "()"
    <|> TyVar <$> identifier
    <|> TyInt <$ reserved "int"
    <|> TyString <$ reserved "string"
    <|> TyDir <$ reserved "dir"
    <|> TyBool <$ reserved "bool"
    <|> TyCmd <$> (reserved "cmd" *> parseTypeAtom)
    <|> TyDelay <$> braces parseType
    <|> parens parseType

parseDirection :: Parser Direction
parseDirection = asum $ map alternative allDirs
 where
  alternative d = d <$ (reserved . dirSyntax . dirInfo) d

-- | Parse Const as reserved words (e.g. @Raise <$ reserved "raise"@)
parseConst :: Parser Const
parseConst = asum $ map alternative consts
 where
  consts = filter isUserFunc allConst
  alternative c = c <$ reserved (syntax $ constInfo c)

-- | Add 'Location' to a 'Term' parser
parseLoc :: Parser Term -> Parser Syntax
parseLoc pterm = do
  start <- getOffset
  term <- pterm
  end <- getOffset
  pure $ Syntax (Location start end) term

parseTermAtom :: Parser Syntax
parseTermAtom =
  parseLoc
    ( TUnit <$ symbol "()"
        <|> TConst <$> parseConst
        <|> TVar <$> identifier
        <|> TDir <$> parseDirection
        <|> TInt <$> integer
        <|> TString <$> stringLiteral
        <|> TBool <$> ((True <$ reserved "true") <|> (False <$ reserved "false"))
        <|> SLam <$> (symbol "\\" *> identifier)
          <*> optional (symbol ":" *> parseType)
          <*> (symbol "." *> parseTerm)
        <|> sLet <$> (reserved "let" *> identifier)
          <*> optional (symbol ":" *> parsePolytype)
          <*> (symbol "=" *> parseTerm)
          <*> (reserved "in" *> parseTerm)
        <|> sDef <$> (reserved "def" *> identifier)
          <*> optional (symbol ":" *> parsePolytype)
          <*> (symbol "=" *> parseTerm <* reserved "end")
    )
    <|> parens parseTerm
    -- Potential syntax for explicitly requesting memoized delay.
    -- Perhaps we will not need this in the end; see the discussion at
    -- https://github.com/byorgey/swarm/issues/150 .
    -- <|> parseLoc (TDelay SimpleDelay (TConst Noop) <$ try (symbol "{{" *> symbol "}}"))
    -- <|> parseLoc (SDelay MemoizedDelay <$> dbraces parseTerm)

    <|> parseLoc (TDelay SimpleDelay (TConst Noop) <$ try (symbol "{" *> symbol "}"))
    <|> parseLoc (SDelay SimpleDelay <$> braces parseTerm)
    <|> parseLoc (ask >>= (guard . (== AllowAntiquoting)) >> parseAntiquotation)

-- | Construct an 'SLet', automatically filling in the Boolean field
--   indicating whether it is recursive.
sLet :: Var -> Maybe Polytype -> Syntax -> Syntax -> Term
sLet x ty t1 = SLet (x `S.member` setOf fv (sTerm t1)) x ty t1

-- | Construct an 'SDef', automatically filling in the Boolean field
--   indicating whether it is recursive.
sDef :: Var -> Maybe Polytype -> Syntax -> Term
sDef x ty t = SDef (x `S.member` setOf fv (sTerm t)) x ty t

parseAntiquotation :: Parser Term
parseAntiquotation =
  TAntiString <$> (lexeme . try) (symbol "$str:" *> identifier)
    <|> TAntiInt <$> (lexeme . try) (symbol "$int:" *> identifier)

-- | Parse a Swarm language term.
parseTerm :: Parser Syntax
parseTerm = sepEndBy1 parseStmt (symbol ";") >>= mkBindChain

mkBindChain :: [Stmt] -> Parser Syntax
mkBindChain stmts = case last stmts of
  Binder _ _ -> fail "Last command in a chain must not have a binder"
  BareTerm t -> return $ foldr mkBind t (init stmts)
 where
  mkBind (BareTerm t1) t2 = loc t1 t2 $ SBind Nothing t1 t2
  mkBind (Binder x t1) t2 = loc t1 t2 $ SBind (Just x) t1 t2
  loc a b = Syntax $ sLoc a <> sLoc b 

data Stmt
  = BareTerm Syntax
  | Binder Text Syntax
  deriving (Show)

parseStmt :: Parser Stmt
parseStmt =
  mkStmt <$> optional (try (identifier <* symbol "<-")) <*> parseExpr

mkStmt :: Maybe Text -> Syntax -> Stmt
mkStmt Nothing = BareTerm
mkStmt (Just x) = Binder x

-- | When semicolons are missing between definitions, for example:
--     def a = 1 end def b = 2 end def c = 3 end
--   The makeExprParser produces:
--     App (App (TDef a) (TDef b)) (TDef x)
--   This function fix that by converting the Apps into Binds, so that it results in:
--     Bind a (Bind b (Bind c))
fixDefMissingSemis :: Syntax -> Syntax
fixDefMissingSemis term =
  case nestedDefs term [] of
    [] -> term
    defs -> foldr1 mkBind defs
 where
  mkBind t1 t2 = Syntax (sLoc t1 <> sLoc t2) $ SBind Nothing t1 t2
  nestedDefs term' acc = case term' of
    def@(Syntax _ SDef {}) -> def : acc
    (Syntax _ (SApp nestedTerm def@(Syntax _ SDef {}))) -> nestedDefs nestedTerm (def : acc)
    -- Otherwise returns an empty list to keep the term unchanged
    _ -> []

parseExpr :: Parser Syntax
parseExpr = fixDefMissingSemis <$> makeExprParser parseTermAtom table
 where
  table = snd <$> Map.toDescList tableMap
  tableMap =
    Map.unionsWith
      (++)
      [ Map.singleton 9 [InfixL (exprLoc2 $ SApp <$ string "")]
      , binOps
      , unOps
      , Map.singleton 2 [InfixR (exprLoc2 $ SPair <$ symbol ",")]
      ]

-- | Utility to add empty location for ExprParser
exprLoc2 :: Parser (Syntax -> Syntax -> Term) -> Parser (Syntax -> Syntax -> Syntax)
exprLoc2 p = do
  f <- p
  pure $ \s1 s2 -> Syntax (sLoc s1 <> sLoc s2) $ f s1 s2

-- | Utility to add empty location for ExprParser
exprLoc1 :: Parser (Syntax -> Term) -> Parser (Syntax -> Syntax)
exprLoc1 p = do
  f <- p
  pure $ \s -> s {sTerm = f s}

-- | Precedences and parsers of binary operators.
--
-- >>> Map.map length binOps
-- fromList [(4,6),(6,2),(7,2),(8,1)]
binOps :: Map.Map Int [Operator Parser Syntax]
binOps = Map.unionsWith (++) $ mapMaybe binOpToTuple allConst
 where
  binOpToTuple c = do
    let ci = constInfo c
    ConstMBinOp assoc <- pure (constMeta ci)
    let assI = case assoc of
          L -> InfixL
          N -> InfixN
          R -> InfixR
    pure $
      Map.singleton
        (fixity ci)
        [assI (mkOp c <$ operatorString (syntax ci))]

-- | Precedences and parsers of unary operators (currently only 'Neg').
--
-- >>> Map.map length unOps
-- fromList [(7,1)]
unOps :: Map.Map Int [Operator Parser Syntax]
unOps = Map.unionsWith (++) $ mapMaybe unOpToTuple allConst
 where
  unOpToTuple c = do
    let ci = constInfo c
    ConstMUnOp assoc <- pure (constMeta ci)
    let assI = case assoc of
          P -> Prefix
          S -> Postfix
    pure $
      Map.singleton
        (fixity ci)
        [assI (exprLoc1 $ SApp (noLoc $ TConst c) <$ operatorString (syntax ci))]

operatorString :: Text -> Parser Text
operatorString n = (lexeme . try) (string n <* notFollowedBy operatorSymbol)

operatorSymbol :: Parser Text
operatorSymbol = T.singleton <$> oneOf opChars
 where
  isOp = \case { ConstMFunc {} -> False; _ -> True } . constMeta
  opChars = nub . concatMap (from . syntax) . filter isOp $ map constInfo allConst

--------------------------------------------------
-- Utilities

-- | Run a parser on some input text, returning either the result or a
--   pretty-printed parse error message.
runParser :: Parser a -> Text -> Either Text a
runParser p t = first (from . errorBundlePretty) (parse (runReaderT p DisallowAntiquoting) "" t)

-- | A utility for running a parser in an arbitrary 'MonadFail' (which
--   is going to be the TemplateHaskell 'Q' monad --- see
--   "Swarm.Language.Parse.QQ"), with a specified source position.
runParserTH :: (Monad m, MonadFail m) => (String, Int, Int) -> Parser a -> String -> m a
runParserTH (file, line, col) p s =
  case snd (runParser' (runReaderT (fully p) AllowAntiquoting) initState) of
    Left err -> fail $ errorBundlePretty err
    Right e -> return e
 where
  -- This is annoying --- megaparsec does not export its function to
  -- construct an initial parser state, so we can't just use that
  -- and then change the one field we need to be different (the
  -- pstateSourcePos). We have to copy-paste the whole thing.
  initState :: State Text Void
  initState =
    State
      { stateInput = from s
      , stateOffset = 0
      , statePosState =
          PosState
            { pstateInput = from s
            , pstateOffset = 0
            , pstateSourcePos = SourcePos file (mkPos line) (mkPos col)
            , pstateTabWidth = defaultTabWidth
            , pstateLinePrefix = ""
            }
      , stateParseErrors = []
      }

-- | Run a parser "fully", consuming leading whitespace and ensuring
--   that the parser extends all the way to eof.
fully :: Parser a -> Parser a
fully p = sc *> p <* eof

-- | Parse some input 'Text' completely as a 'Term', consuming leading
--   whitespace and ensuring the parsing extends all the way to the
--   end of the input 'Text'.  Returns either the resulting 'Term' or
--   a pretty-printed parse error message.
readTerm :: Text -> Either Text Syntax
readTerm = runParser (fully parseTerm)

-- | A lower-level readTerm which returns the megaparsec bundle error
--   for precise error reporting.
readTerm' :: Text -> Either ParserError Syntax
readTerm' = parse (runReaderT (fully parseTerm) DisallowAntiquoting) ""

-- | A utility for converting a ParserError into a one line message:
--   <line-nr>: <error-msg>
showShortError :: ParserError -> String
showShortError pe = show (line + 1) <> ": " <> from msg
 where
  ((line, _), _, msg) = showErrorPos pe

-- | A utility for converting a ParseError into a range and error message.
showErrorPos :: ParserError -> ((Int, Int), (Int, Int), Text)
showErrorPos (ParseErrorBundle errs sourcePS) = (minusOne start, minusOne end, from msg)
 where
  -- convert megaparsec source pos to starts at 0
  minusOne (x, y) = (x - 1, y - 1)

  -- get the first error position (ps) and line content (str)
  err = Data.List.NonEmpty.head errs
  offset = case err of
    TrivialError x _ _ -> x
    FancyError x _ -> x
  (str, ps) = reachOffset offset sourcePS
  msg = parseErrorTextPretty err

  -- extract the error starting position
  start@(line, col) = getLineCol ps

  -- compute the ending position based on the word at starting position
  wordlength = case break (== ' ') . drop col <$> str of
    Just (word, _) -> length word + 1
    _ -> 0
  end = (line, col + wordlength)

getLineCol :: PosState a -> (Int, Int)
getLineCol ps = (line, col)
 where
  line = unPos $ sourceLine $ pstateSourcePos ps
  col = unPos $ sourceColumn $ pstateSourcePos ps

-- | A utility for converting a Location into a range
getLocRange :: Text -> (Int, Int) -> ((Int, Int), (Int, Int))
getLocRange code (locStart, locEnd) = (start, end)
 where
  start = getLocPos locStart
  end = getLocPos (dropWhiteSpace locEnd)

  -- remove trailing whitespace that got included by the lexer
  dropWhiteSpace offset
    | isWhiteSpace offset = dropWhiteSpace (offset - 1)
    | otherwise = offset
  isWhiteSpace offset =
    -- Megaparsec offset needs to be (-1) to start at 0
    Data.Text.index code (offset - 1) `elem` [' ', '\n', '\r', '\t']

  -- using megaparsec offset facility, compute the line/col
  getLocPos offset =
    let sourcePS =
          PosState
            { pstateInput = code
            , pstateOffset = 0
            , pstateSourcePos = Pos.initialPos ""
            , pstateTabWidth = Pos.defaultTabWidth
            , pstateLinePrefix = ""
            }
        (_, ps) = reachOffset offset sourcePS
     in getLineCol ps
