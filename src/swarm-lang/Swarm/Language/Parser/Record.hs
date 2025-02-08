{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parsing records in the Swarm language.
module Swarm.Language.Parser.Record (
  parseRecord,
) where

import Data.Map (Map)
import Data.Map qualified as M
import Swarm.Language.Context (Var)
import Swarm.Language.Parser.Core (Parser)
import Swarm.Language.Parser.Lex (symbol, tmVar)
import Swarm.Util (failT, findDup, squote)
import Text.Megaparsec (sepBy)

-- | Parse something using record syntax of the form @x1 v1, x2 v2,
--   ...@.  The same parser is used both in parsing record types and
--   record values, so it is factored out into its own module.
--
--   The @Parser a@ argument is the parser to use for the RHS of each
--   binding in the record.
parseRecord :: Parser a -> Parser (Map Var a)
parseRecord p = (parseBinding `sepBy` symbol ",") >>= fromListUnique
 where
  parseBinding = (,) <$> tmVar <*> p
  fromListUnique kvs = case findDup (map fst kvs) of
    Nothing -> return $ M.fromList kvs
    Just x -> failT ["duplicate field name", squote x, "in record literal"]
