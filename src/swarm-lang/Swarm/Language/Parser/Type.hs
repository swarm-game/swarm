{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parsing types in the Swarm language.
module Swarm.Language.Parser.Type (
  parsePolytype,
  parseType,
  parseTypeMolecule,
  parseTypeAtom,
  parseTyCon,
) where

import Control.Monad.Combinators (many)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Bifunctor (first)
import Data.Fix (Fix (..), foldFix)
import Data.List.Extra (enumerate)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Swarm.Language.Parser.Core (Parser)
import Swarm.Language.Parser.Lex (
  braces,
  brackets,
  parens,
  reserved,
  reservedCS,
  symbol,
  tyName,
  tyVar,
 )
import Swarm.Language.Parser.Record (parseRecord)
import Swarm.Language.Syntax.Loc (locVal)
import Swarm.Language.Types
import Text.Megaparsec (choice, optional, some, (<|>))

-- | Parse a Swarm language polytype, which starts with an optional
--   quanitifation (@forall@ followed by one or more variables and a
--   period) followed by a type.  Note that anything accepted by
--   'parseType' is also accepted by 'parsePolytype'.
parsePolytype :: Parser RawPolytype
parsePolytype =
  mkPoly . fromMaybe []
    <$> optional ((reserved "forall" <|> reserved "∀") *> some tyVar <* symbol ".")
    <*> parseType

-- | Parse a Swarm language (mono)type.
parseType :: Parser Type
parseType = makeExprParser parseTypeMolecule table
 where
  table =
    [ [InfixR ((:*:) <$ symbol "*")]
    , [InfixR ((:+:) <$ symbol "+")]
    , [InfixR ((:->:) <$ symbol "->")]
    ]

-- | A "type molecule" consists of either a type constructor applied
--   to a chain of type atoms, or just a type atom by itself.  We have
--   to separate this out from parseTypeAtom to deal with the left
--   recursion.
parseTypeMolecule :: Parser Type
parseTypeMolecule =
  TyConApp
    <$> parseTyCon
    <*> many parseTypeAtom
    <|> parseTypeAtom

-- | A "type atom" consists of some atomic type snytax --- type
--   variables, things in brackets of some kind, or a lone type
--   constructor.
parseTypeAtom :: Parser Type
parseTypeAtom =
  TyVar
    <$> tyVar
    <|> TyConApp
      <$> parseTyCon
      <*> pure []
    <|> TyDelay
      <$> braces parseType
    <|> TyRcd
      <$> brackets (M.fromList . (map . first) locVal <$> parseRecord (symbol ":" *> parseType))
    <|> tyRec
      <$> (reserved "rec" *> tyVar)
      <*> (symbol "." *> parseType)
    <|> parens parseType

-- | A type constructor.
parseTyCon :: Parser TyCon
parseTyCon = do
  choice (map (\b -> TCBase b <$ reservedCS (baseTyName b)) enumerate)
    <|> TCCmd
      <$ reservedCS "Cmd"
    <|> TCUser
      <$> tyName

-- | Close over a recursive type, replacing any bound occurrences
--   of its variable in the body with de Bruijn indices.  Note that
--   (1) we don't have to worry about conflicts with type variables
--   bound by a top-level @forall@; since @forall@ must always be at
--   the top level, any @rec@ will necessarily be lexically within the
--   scope of any @forall@ and hence variables bound by @rec@ will
--   shadow any variables bound by a @forall@.  For example, @forall
--   a. a -> (rec a. unit + a)@ is a function from an arbitrary type
--   to a recursive natural number. (2) Any @rec@ contained inside
--   this one will have already been closed over when it was parsed,
--   and its bound variables thus replaced by de Bruijn indices, so
--   neither do we have to worry about being shadowed --- any
--   remaining free occurrences of the variable name in question are
--   indeed references to this @rec@ binder.
tyRec :: Var -> Type -> Type
tyRec x = TyRec x . ($ NZ) . foldFix s
 where
  s :: TypeF (Nat -> Type) -> Nat -> Type
  s = \case
    TyRecF y ty -> Fix . TyRecF y . ty . NS
    TyVarF orig y
      | x == y -> Fix . TyRecVarF
      | otherwise -> const (Fix (TyVarF orig y))
    fty -> \i -> Fix (fmap ($ i) fty)
