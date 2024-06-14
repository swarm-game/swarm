{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Some convenient functions for putting together the whole Swarm
-- language processing pipeline: parsing, type checking, capability
-- checking, and elaboration.  If you want to simply turn some raw
-- text representing a Swarm program into something useful, this is
-- probably the module you want.
module Swarm.Language.Pipeline (
  -- * Pipeline functions
  processTerm,
  processParsedTerm,
  processTerm',
  processParsedTerm',
  processTermEither,
) where

import Control.Lens ((^.))
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Language.Elaborate
import Swarm.Language.Parser (readTerm)
import Swarm.Language.Pretty
import Swarm.Language.Syntax
import Swarm.Language.Typecheck
import Swarm.Language.Value (Env, envReqs, envTydefs, envTypes)

processTermEither :: Text -> Either Text TSyntax
processTermEither t = case processTerm t of
  Left err -> Left $ T.unwords ["Could not parse term:", err]
  Right Nothing -> Left "Term was only whitespace"
  Right (Just pt) -> Right pt

-- | Given a 'Text' value representing a Swarm program,
--
--   1. Parse it (see "Swarm.Language.Parse")
--   2. Typecheck it (see "Swarm.Language.Typecheck")
--   3. Elaborate it (see "Swarm.Language.Elaborate")
--
--   Return either the end result (or @Nothing@ if the input was only
--   whitespace) or a pretty-printed error message.
processTerm :: Text -> Either Text (Maybe TSyntax)
processTerm = processTerm' mempty

-- | Like 'processTerm', but use a term that has already been parsed.
processParsedTerm :: Syntax -> Either ContextualTypeErr TSyntax
processParsedTerm = processParsedTerm' mempty

-- | Like 'processTerm', but use explicit starting contexts.
processTerm' :: Env -> Text -> Either Text (Maybe TSyntax)
processTerm' e txt = do
  mt <- readTerm txt
  first (prettyTypeErrText txt) $ traverse (processParsedTerm' e) mt

-- | Like 'processTerm'', but use a term that has already been parsed.
processParsedTerm' :: Env -> Syntax -> Either ContextualTypeErr TSyntax
processParsedTerm' e t = do
  tt <- inferTop (e ^. envTypes) (e ^. envReqs) (e ^. envTydefs) t
  return $ elaborate tt
