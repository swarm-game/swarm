{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           SubsetSolver

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Map                       as M
import           Data.Maybe
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           Data.Void
import           Text.Megaparsec.Error          (errorBundlePretty)
import           Text.Printf
import           Witch

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
  EForce :: Expr -> Expr
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

data Capability = CMove | CBuild | CArith | CLambda deriving (Eq, Ord, Read, Show, Enum, Bounded)

type CapSet = Set Capability

noCaps :: CapSet
noCaps = S.empty

data Type where
  TyInt :: Type
  TyFun :: CapSet -> Type -> Type -> Type
  TyDelay :: CapSet -> Type -> Type
  TyCmd :: CapSet -> Type -> Type
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
    | T.toLower t `elem` reservedWords =
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
      <*> (symbol "." *> parseTerm)
    <|> EForce <$> (reserved "force" *> parseAtom)
    <|> EMove <$ reserved "move"
    <|> EBuild <$ reserved "build"
    <|> EDelay <$> braces parseTerm
    <|> parens parseTerm

parseTerm :: Parser Expr
parseTerm = sepEndBy1 parseStmt (symbol ";") >>= mkBindChain

mkBindChain :: [Stmt] -> Parser Expr
mkBindChain stmts = case last stmts of
  Binder _ _ -> fail "Last command in a chain must not have a binder"
  BareTerm t -> return $ foldr mkBind t (init stmts)
 where
  mkBind (BareTerm t1) t2 = EBind Nothing t1 t2
  mkBind (Binder x t1) t2 = EBind (Just x) t1 t2

data Stmt
  = BareTerm Expr
  | Binder Text Expr
  deriving (Show)

parseStmt :: Parser Stmt
parseStmt =
  mkStmt <$> optional (try (identifier <* symbol "<-")) <*> parseExpr

mkStmt :: Maybe Text -> Expr -> Stmt
mkStmt Nothing  = BareTerm
mkStmt (Just x) = Binder x

parseExpr :: Parser Expr
parseExpr = makeExprParser parseAtom table
 where
  table =
    [ [InfixL (EApp <$ symbol "")]
    , [InfixL (EPlus <$ symbol "+")]
    ]

parseTypeAtom :: Parser Type
parseTypeAtom =
  TyInt <$ reserved "int"
    <|> TyDelay S.empty <$> braces parseType
    <|> TyCmd S.empty <$> (reserved "cmd" *> parseTypeAtom)
    <|> parens parseType

parseType :: Parser Type
parseType = makeExprParser parseTypeAtom table
 where
  table = [[InfixR (TyFun S.empty <$ symbol "->")]]

term :: Parser Expr
term = sc *> parseTerm <* eof

tm :: Text -> Expr
tm s = case parse term "" s of
  Left err -> error (errorBundlePretty err)
  Right e  -> e

------------------------------------------------------------
-- Pretty printing
------------------------------------------------------------

type Prec = Int

prettyType :: Type -> Text
prettyType = prettyTyPrec 0
 where
  prettyTyPrec :: Prec -> Type -> Text
  prettyTyPrec _ TyInt = "Int"
  prettyTyPrec p (TyFun c ty1 ty2) =
    mparens (p > 0) $ prettyTyPrec 1 ty1 <> prettyArrow c <> prettyTyPrec 0 ty2
  prettyTyPrec _ (TyDelay c ty) =
    "{" <> prettyTyPrec 0 ty <> "}" <> (if S.null c then "" else prettyCapSet c)
  prettyTyPrec _ (TyCmd c ty)
    | S.null c  = "cmd " <> prettyTyPrec 2 ty
    | otherwise = "cmd " <> prettyCapSet c <> " " <> prettyTyPrec 2 ty

  prettyArrow :: CapSet -> Text
  prettyArrow s
    | S.null s = " -> "
    | otherwise = " -" <> prettyCapSet s <> "-> "

prettyCapSet :: CapSet -> Text
prettyCapSet s = "[" <> T.intercalate "," (map prettyCap (S.toList s)) <> "]"

prettyCap :: Capability -> Text
prettyCap CMove   = "m"
prettyCap CBuild  = "b"
prettyCap CArith  = "a"
prettyCap CLambda = "λ"

mparens :: Bool -> Text -> Text
mparens True  = ("(" <>) . (<> ")")
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
  NotDelay :: Expr -> Type -> TypeError
  NotCmd :: Expr -> Type -> TypeError

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
prettyTypeError (NotDelay e ty) =
  from @String $
    printf
      "Type error: %s should be a delayed expression, but has type %s."
      (pretty e)
      (prettyType ty)
prettyTypeError (NotCmd e ty) =
  from @String $
    printf
      "Type error: %s should be a command, but has type %s."
      (pretty e)
      (prettyType ty)
prettyTypeError (CantInfer e) =
  from @String $
    printf
      "Can't infer the type of %s."
      (pretty e)

type TCM a = WriterT [Ineq Var Capability] (ReaderT Ctx (Except TypeError)) a

runTCM :: TCM a -> Either TypeError (a, [Ineq Var Capability])
runTCM = runExcept . flip runReaderT M.empty . runWriterT

infer :: Expr -> TCM (Type, CapSet)
infer (EVar x) = do
  ctx <- ask
  case M.lookup x ctx of
    Just ty -> return (ty, noCaps)
    Nothing -> throwError $ UnboundVar x
infer (EInt _) = return (TyInt, noCaps)
infer (EPlus e1 e2) = do
  c1 <- check e1 TyInt
  c2 <- check e2 TyInt
  return (TyInt, S.insert CArith (c1 `S.union` c2))
infer (ELam x (Just argTy) body) = do
  (resTy, d) <- local (M.insert x argTy) $ infer body
  return (TyFun d argTy resTy, S.singleton CLambda)
infer e@(ELam _ Nothing _) = throwError $ CantInfer e
infer (EApp e1 e2) = do
  (d1, argTy, resTy, d2) <- checkFun e1
  d3 <- check e2 argTy
  return (resTy, S.unions [d1,d2,d3])
infer (EDelay e) = do
  (ty, d) <- infer e
  return (TyDelay d ty, noCaps)
infer (EForce e) = do
  (dty, d2) <- infer e
  (ty, d1) <- checkDelay e dty
  return (ty, d1 `S.union` d2)
infer (EBind mx c1 c2) = do
  (ty1, d11, d12) <- checkCmd c1
  (ty2, d21, d22) <- local (maybe id (`M.insert` ty1) mx) $ checkCmd c2
  return (TyCmd noCaps ty2, S.unions [d11, d12, d21, d22])

check :: Expr -> Type -> TCM CapSet
check = undefined
-- check ctx e@(ELam x Nothing body) ty =
--   case ty of
--     TyFun d argTy resTy -> check (M.insert x argTy ctx) body resTy
--     _ -> Left $ MismatchFun e ty
-- check ctx e ty = do
--   ty' <- infer ctx e
--   case ty == ty' of
--     True -> return ()
--     False -> Left $ Mismatch e ty ty'

checkFun :: Expr -> TCM (CapSet, Type, Type, CapSet)
checkFun e = do
  (ty, d2) <- infer e
  case ty of
    TyFun d1 argTy resTy -> return (d1, argTy, resTy, d2)
    _                    -> throwError $ NotAFunction e ty

checkDelay :: Expr -> Type -> TCM (Type, CapSet)
checkDelay _ (TyDelay d ty) = return (ty, d)
checkDelay e ty             = throwError $ NotDelay e ty

checkCmd :: Expr -> TCM (Type, CapSet, CapSet)
checkCmd e = do
  (ty, d2) <- infer e
  case ty of
    TyCmd d1 ty1 -> return (ty1, d1, d2)
    _            -> throwError $ NotCmd e ty

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
    _                  -> error "Impossible! interp' EPlus on non-Ints"
interp' env (ELam x _ body) = VClosure env x body
interp' env (EApp fun arg) =
  case interp' env fun of
    VClosure env' x body ->
      interp' (M.insert x (interp' env arg) env') body
    _ -> error "Impossible! interp' EApp on non-closure"

-- eval :: String -> IO ()
-- eval s = case parse expr "" (into @Text s) of
--   Left err -> print err
--   Right e -> case infer M.empty e of
--     Left tyerr -> T.putStrLn $ prettyTypeError tyerr
--     Right _    -> putStrLn $ prettyValue (interp e)

-- Example to show that closures are working properly:
--
-- ((\x -> (\f -> f x + f (x + 3))) 1) ((\x -> \y -> x + y) 3)
--
-- should yield 11 when evaluated.  Possible wrong results would be
-- (a) crash, or (b) 7.

main :: IO ()
main = putStrLn "hi!"
  -- getLine >>= eval
