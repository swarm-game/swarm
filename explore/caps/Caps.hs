{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text, toLower)
import Data.Text.IO qualified as T
import Data.Void
import Text.Printf
import Witch

------------------------------------------------------------
-- Type declarations
------------------------------------------------------------

type Var = Text

data Expr where
  EVar :: Var -> Expr
  EInt :: Integer -> Expr
  EPlus :: Expr -> Expr -> Expr
  ELam :: Var -> Maybe Type -> Expr -> Expr
  EApp :: Expr -> Expr -> Expr
  EDelay :: Expr -> Expr
  EForce :: Expr
  EMove :: Expr
  EBuild :: Expr
  EBind :: Maybe Var -> Expr -> Expr -> Expr
  deriving (Show)

data Value where
  VInt :: Integer -> Value
  VClosure :: Env -> Text -> Expr -> Value
  VDelay :: Env -> Expr -> Value
  deriving (Show)

type Env = M.Map Text Value

data Capability = CMove | CBuild deriving (Eq, Ord, Read, Show, Enum, Bounded)

type CapSet = Set Capability

data Type where
  TyInt :: Type
  TyFun :: Type -> CapSet -> Type -> Type
  TyDelay :: Type -> CapSet -> Type
  deriving (Show, Eq)

type Ctx = M.Map Var Type

------------------------------------------------------------
-- Parser
------------------------------------------------------------

type Parser = Parsec Void Text

reservedWords :: [Text]
reservedWords = ["cmd", "let", "in", "force", "move", "build"]

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

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
identifier = (lexeme . try) (p >>= checkReserved) <?> "variable name"
 where
  p = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_' <|> char '\'')
  checkReserved s
    | toLower t `elem` reservedWords =
      fail $ "reserved word '" ++ s ++ "' cannot be used as variable name"
    | otherwise = return t
   where
    t = into @Text s

integer :: Parser Integer
integer = lexeme L.decimal

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseAtom :: Parser Expr
parseAtom =
  EVar <$> identifier
    <|> EInt <$> integer
    <|> ELam <$> (symbol "\\" *> identifier)
      <*> optional (symbol ":" *> parseType)
      <*> (symbol "." *> parseExpr)
    <|> EForce <$ reserved "force"
    <|> EMove <$ reserved "move"
    <|> EBuild <$ reserved "build"
    <|> EDelay <$> braces parseExpr
    <|> parens parseExpr

parseExpr :: Parser Expr
parseExpr = makeExprParser parseAtom table
 where
  table =
    [ [InfixL (EApp <$ symbol "")]
    , [InfixL (EPlus <$ symbol "+")]
    ]

parseTypeAtom :: Parser Type
parseTypeAtom =
  TyInt <$ reserved "Int"
    <|> TyDelay <$> braces parseType
    <|> parens parseType

parseType :: Parser Type
parseType = makeExprParser parseTypeAtom table
 where
  table = [[InfixR (TyFun <$ symbol "->")]]

expr :: Parser Expr
expr = sc *> parseExpr <* eof

tm :: Text -> Expr
tm s = case parse expr "" s of
  Left err -> error (show err)
  Right e -> e

------------------------------------------------------------
-- Pretty printing
------------------------------------------------------------

type Prec = Int

prettyType :: Type -> Text
prettyType = prettyTyPrec 0
 where
  prettyTyPrec :: Prec -> Type -> Text
  prettyTyPrec _ TyInt = "Int"
  prettyTyPrec p (TyFun ty1 ty2) =
    mparens (p > 0) $ prettyTyPrec 1 ty1 <> " -> " <> prettyTyPrec 0 ty2

mparens :: Bool -> Text -> Text
mparens True = ("(" <>) . (<> ")")
mparens False = id

data Associativity = L | R
  deriving (Show, Eq)

pretty :: Expr -> Text
pretty = prettyPrec 0 L
 where
  prettyPrec :: Prec -> Associativity -> Expr -> Text
  prettyPrec _ _ (EVar x) = x
  prettyPrec _ _ (EInt i) = from @String (show i)
  prettyPrec p a (EPlus e1 e2) =
    mparens
      (p > 1 || (p == 1 && a == R))
      (prettyPrec 1 L e1 <> " + " <> prettyPrec 1 R e2)
  prettyPrec p _ (ELam x mty body) =
    mparens
      (p > 0)
      ( "^" <> x <> maybe "" (\ty -> " : " <> prettyType ty) mty
          <> ". "
          <> prettyPrec 0 L body
      )
  prettyPrec p a (EApp e1 e2) =
    mparens
      (p > 2 || (p == 2 && a == R))
      (prettyPrec 2 L e1 <> " " <> prettyPrec 2 R e2)

------------------------------------------------------------
-- Type checker
------------------------------------------------------------

data TypeError where
  UnboundVar :: Var -> TypeError
  Mismatch :: Expr -> Type -> Type -> TypeError
  MismatchFun :: Expr -> Type -> TypeError
  CantInfer :: Expr -> TypeError
  NotAFunction :: Expr -> Type -> TypeError

prettyTypeError :: TypeError -> Text
prettyTypeError (UnboundVar x) = "Unbound variable " <> x
prettyTypeError (Mismatch e expected actual) =
  from @String $
    printf
      "Type error: %s should have type %s, but has type %s."
      (pretty e)
      (prettyType expected)
      (prettyType actual)
prettyTypeError (MismatchFun e expected) =
  from @String $
    printf
      "Type error: %s should have type %s, but has a function type."
      (pretty e)
      (prettyType expected)
prettyTypeError (NotAFunction e ty) =
  from @String $
    printf
      "Type error: %s should be a function, but has type %s."
      (pretty e)
      (prettyType ty)
prettyTypeError (CantInfer e) =
  from @String $
    printf
      "Can't infer the type of %s."
      (pretty e)

-- infer :: Ctx -> Expr -> Either TypeError Type
-- infer ctx (EVar x) =
--   case M.lookup x ctx of
--     Just ty -> return ty
--     Nothing -> Left $ UnboundVar x
-- infer _ (EInt _) = return TyInt
-- infer ctx (EPlus e1 e2) = do
--   check ctx e1 TyInt
--   check ctx e2 TyInt
--   return TyInt
-- infer ctx (EApp e1 e2) = do
--   (argTy, resTy) <- checkFun ctx e1
--   check ctx e2 argTy
--   return resTy
-- infer ctx (ELam x (Just argTy) body) = do
--   resTy <- infer (M.insert x argTy ctx) body
--   return $ TyFun argTy resTy

-- -- Can't infer type of a bare lambda
-- infer _ e = Left $ CantInfer e

-- check :: Ctx -> Expr -> Type -> Either TypeError ()
-- check ctx e@(ELam x Nothing body) ty =
--   case ty of
--     TyFun argTy resTy -> check (M.insert x argTy ctx) body resTy
--     _ -> Left $ MismatchFun e ty
-- check ctx e ty = do
--   ty' <- infer ctx e
--   case ty == ty' of
--     True -> return ()
--     False -> Left $ Mismatch e ty ty'

-- checkFun :: Ctx -> Expr -> Either TypeError (Type, Type)
-- checkFun ctx e = do
--   ty <- infer ctx e
--   case ty of
--     TyFun argTy resTy -> return (argTy, resTy)
--     _ -> Left $ NotAFunction e ty

------------------------------------------------------------
-- Interpreter
------------------------------------------------------------

prettyValue :: Value -> String
prettyValue = show

interp :: Expr -> Value
interp = interp' M.empty

interp' :: Env -> Expr -> Value
interp' env (EVar x) = fromJust $ M.lookup x env
interp' _ (EInt n) = VInt n
interp' env (EPlus ea eb) =
  case (interp' env ea, interp' env eb) of
    (VInt va, VInt vb) -> VInt (va + vb)
    _ -> error "Impossible! interp' EPlus on non-Ints"
interp' env (ELam x _ body) = VClosure env x body
interp' env (EApp fun arg) =
  case interp' env fun of
    VClosure env' x body ->
      interp' (M.insert x (interp' env arg) env') body
    _ -> error "Impossible! interp' EApp on non-closure"

eval :: String -> IO ()
eval s = case parse expr "" (into @Text s) of
  Left err -> print err
  Right e -> case infer M.empty e of
    Left tyerr -> T.putStrLn $ prettyTypeError tyerr
    Right _ -> putStrLn $ prettyValue (interp e)

-- Example to show that closures are working properly:
--
-- ((\x -> (\f -> f x + f (x + 3))) 1) ((\x -> \y -> x + y) 3)
--
-- should yield 11 when evaluated.  Possible wrong results would be
-- (a) crash, or (b) 7.

main :: IO ()
main = getLine >>= eval
