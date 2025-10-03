{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parsing records in the Swarm language.
module Swarm.Language.Parser.Record (
  parseRecord,
) where

import Swarm.Language.Parser.Core (Parser)
import Swarm.Language.Parser.Lex (symbol, locTmVar)
import Swarm.Language.Syntax.Loc (LocVar, lvVar)
import Swarm.Util (failT, findDup, squote)
import Text.Megaparsec (sepBy)

-- | Parse something using record syntax of the form @x1 v1, x2 v2,
--   ...@.  The same parser is used both in parsing record types and
--   record values, so it is factored out into its own module.
--
--   The @Parser a@ argument is the parser to use for the RHS of each
--   binding in the record.
parseRecord :: Parser a -> Parser [(LocVar, a)]
parseRecord p = (parseBinding `sepBy` symbol ",") >>= fromListUnique
 where
  parseBinding = (,) <$> locTmVar <*> p
  fromListUnique kvs = case findDup (map (lvVar . fst) kvs) of
    Nothing -> pure kvs
    Just x -> failT ["duplicate field name", squote x, "in record literal"]
