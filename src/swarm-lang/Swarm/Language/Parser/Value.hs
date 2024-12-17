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
-- command or)
module Swarm.Language.Parser.Value (readValue) where

import Control.Monad (mzero)
import Data.Either.Extra (eitherToMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Swarm.Language.Value
import Swarm.Language.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Witch (into)

------------------------------------------------------------

readValue :: Type -> Text -> Maybe Value
readValue ty = eitherToMaybe . runParser (parseValue ty <* eof) ""

------------------------------------------------------------

type Parser = Parsec Void Text

sc :: Parser ()
sc = space1

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

decimal :: Parser Integer
decimal = lexeme L.decimal

integer :: Parser Integer
integer = L.signed sc decimal

parseValue :: Type -> Parser Value
parseValue = \case
  TyVoid -> mzero
  TyUnit -> VUnit <$ symbol "()"
  TyInt -> VInt <$> integer
  TyText -> VText . into @Text <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))
  _ -> mzero -- XXX

