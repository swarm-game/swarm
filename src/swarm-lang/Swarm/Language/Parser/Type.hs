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

import Control.Lens (view)
import Control.Monad (join)
import Control.Monad.Combinators (many)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Swarm.Language.Parser.Core (LanguageVersion (..), Parser, languageVersion)
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
import Swarm.Util (listEnums)
import Text.Megaparsec (choice, optional, some, (<|>))
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
  TyConApp <$> parseTyCon <*> many parseTypeAtom
    <|> parseTypeAtom

-- | A "type atom" consists of some atomic type snytax --- type
--   variables, things in brackets of some kind, or a lone type
--   constructor.
parseTypeAtom :: Parser Type
parseTypeAtom =
  TyVar <$> tyVar
    <|> TyConApp <$> parseTyCon <*> pure []
    <|> TyDelay <$> braces parseType
    <|> TyRcd <$> brackets (parseRecord (symbol ":" *> parseType))
    <|> parens parseType

-- | A type constructor.
parseTyCon :: Parser TyCon
parseTyCon = do
  ver <- view languageVersion
  let reservedCase = case ver of
        -- Version 0.5 of the language accepted type names in any case
        SwarmLang0_5 -> reserved
        -- The latest version requires them to be uppercase
        SwarmLangLatest -> reservedCS
  choice (map (\b -> TCBase b <$ reservedCase (baseTyName b)) listEnums)
    <|> TCCmd <$ reservedCase "Cmd"
