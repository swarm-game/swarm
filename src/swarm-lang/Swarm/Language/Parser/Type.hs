{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parsing types in the Swarm language.
module Swarm.Language.Parser.Type (
  parsePolytype,
  parseType,
) where

import Control.Monad (join)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Swarm.Language.Parser.Core (Parser)
import Swarm.Language.Parser.Lex (
  braces,
  brackets,
  parens,
  reserved,
  reservedCS,
  symbol,
  tyVar,
 )
import Swarm.Language.Parser.Record (parseRecord)
import Swarm.Language.Types
import Text.Megaparsec (optional, some, (<|>))
import Witch (from)

-- | Parse a Swarm language polytype, which starts with an optional
--   quanitifation (@forall@ followed by one or more variables and a
--   period) followed by a type.  Note that anything accepted by
--   'parseType' is also accepted by 'parsePolytype'.
parsePolytype :: Parser Polytype
parsePolytype =
  join $
    ( quantify . fromMaybe []
        <$> optional ((reserved "forall" <|> reserved "∀") *> some tyVar <* symbol ".")
    )
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
  TyVoid
    <$ reservedCS "Void"
    <|> TyUnit
      <$ reservedCS "Unit"
    <|> TyInt
      <$ reservedCS "Int"
    <|> TyText
      <$ reservedCS "Text"
    <|> TyDir
      <$ reservedCS "Dir"
    <|> TyBool
      <$ reservedCS "Bool"
    <|> TyActor
      <$ reservedCS "Actor"
    <|> TyKey
      <$ reservedCS "Key"
    <|> TyCmd
      <$> (reservedCS "Cmd" *> parseTypeAtom)
    <|> TyVar
      <$> tyVar
    <|> TyDelay
      <$> braces parseType
    <|> TyRcd
      <$> brackets (parseRecord (symbol ":" *> parseType))
    <|> parens parseType
