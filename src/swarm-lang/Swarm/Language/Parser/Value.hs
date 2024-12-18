{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parser for values of the Swarm language, indexed by type.
--
-- Note that this is completely separate from the main Swarm language
-- parser in the other Swarm.Language.Parser.* modules.  Parsing
-- *values* only ever happens at runtime (e.g. to implement the 'read'
-- command), and does not need to deal with comments, source position
-- tracking, etc.
module Swarm.Language.Parser.Value (readValue, parseValue) where

import Data.Either.Extra (eitherToMaybe)
import Data.Foldable (asum)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Void (Void)
import Swarm.Language.Syntax.Direction
import Swarm.Language.Value
import Swarm.Language.Types
import Swarm.Util (findDup)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Witch (into)

------------------------------------------------------------

readValue :: Type -> Text -> Maybe Value
readValue ty = eitherToMaybe . runParser (sc *> parseValue ty <* eof) ""

------------------------------------------------------------

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

decimal :: Parser Integer
decimal = lexeme L.decimal

integer :: Parser Integer
integer = L.signed sc decimal

parseAtomicValue :: Type -> Parser Value
parseAtomicValue = \case
  TyVoid -> empty
  TyUnit -> VUnit <$ symbol "()"
  TyInt -> VInt <$> integer
  TyText -> VText . into @Text <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))
  TyDir -> VDir <$> parseDirection
  TyBool -> VBool <$> (False <$ symbol "false" <|> True <$ symbol "true")
  ty1 :*: ty2 -> parens (parseTuple ty1 ty2)
  TyRcd r -> parseRecord r

  -- Can't parse Delay values for now since they contain closures;
  -- would require calling out to swarm-lang parser (maybe later)
  TyDelay _ -> empty

  -- All other values must be enclosed in parentheses in order to
  -- count as syntactically atomic
  ty -> parens (parseValue ty)

parseValue :: Type -> Parser Value
parseValue = \case
  ty1 :+: ty2 ->
    VInj False <$> (symbol "inl" *> parseAtomicValue ty1) <|>
    VInj True <$> (symbol "inr" *> parseAtomicValue ty2)

  -- TODO
  TyKey -> empty

  -- Can't parse Actor values since they are just of the form "<a3>",
  -- not enough info to reconstruct
  TyActor -> empty

  -- Can't parse function or command values for now since they contain
  -- closures; would require calling out to swarm-lang parser (maybe
  -- later)
  _ :->: _ -> empty
  TyCmd _ -> empty

  -- TODO: not sure what to do with these yet
  TyRec _ _ -> empty
  TyUser _ _ -> empty

  ty -> parseAtomicValue ty

parseDirection :: Parser Direction
parseDirection = asum (map alternative allDirs)
 where
  alternative d = d <$ (symbol . directionSyntax) d

parseTuple :: Type -> Type -> Parser Value
parseTuple ty1 ty2 = VPair <$> parseValue ty1 <*> (symbol "," *> parseRHS ty2)
  where
    parseRHS (ty21 :*: ty22) = parseTuple ty21 ty22
    parseRHS ty = parseValue ty

parseRecord :: Map Var Type -> Parser Value
parseRecord r = brackets (parseField `sepBy` symbol ",") >>= mkRcd
  where
    -- This is slightly more lenient than the real swarm-lang parser
    -- since it will accept e.g. reserved words as field names, but it
    -- doesn't matter since they could never correspond to a valid
    -- record type.
    ident = into @Text <$> lexeme
      ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_' <|> char '\''))

    parseField :: Parser (Var, Value)
    parseField = do
      x <- ident
      case M.lookup x r of
        Just ty -> (x,) <$> (symbol "=" *> parseValue ty)
        _ -> empty

    mkRcd :: [(Var,Value)] -> Parser Value
    mkRcd vs
      -- Don't allow duplicate fields
      | isJust (findDup (map fst vs)) = empty
      -- Require set of keys to be the same as the set in the type
      | S.fromList (map fst vs) == M.keysSet r = pure $ VRcd (M.fromList vs)
      | otherwise = empty

