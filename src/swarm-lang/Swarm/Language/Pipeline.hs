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

  -- * Utilities
  extractTCtx,
  extractReqCtx,
) where

import Control.Lens ((^.))
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Elaborate
import Swarm.Language.Parser (readTerm)
import Swarm.Language.Requirements.Type (ReqCtx)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck
import Swarm.Language.Types (TCtx)
import Swarm.Language.Value (Env, emptyEnv, envReqs, envTydefs, envTypes)

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
processTerm = processTerm' emptyEnv

-- | Like 'processTerm', but use a term that has already been parsed.
processParsedTerm :: Syntax -> Either ContextualTypeErr TSyntax
processParsedTerm = processParsedTerm' emptyEnv

-- | Like 'processTerm', but use explicit starting contexts.
processTerm' :: Env -> Text -> Either Text (Maybe TSyntax)
processTerm' e txt = do
  mt <- readTerm txt
  first (prettyTypeErrText txt) $ traverse (processParsedTerm' e) mt

-- | Like 'processTerm'', but use a term that has already been parsed.
processParsedTerm' :: Env -> Syntax -> Either ContextualTypeErr TSyntax
processParsedTerm' e t = do
  -- XXX load SourceMap
  tt <- inferTop (e ^. envTypes) (e ^. envReqs) (e ^. envTydefs) _ t
  return $ elaborate tt

------------------------------------------------------------
-- Some utility functions
------------------------------------------------------------

-- | Extract a type context from type annotations on definitions
--   contained in a term.  Should probably only be used for testing.
extractTCtx :: Syntax' ty -> TCtx
extractTCtx (Syntax' _ t _ _) = extractTCtxTerm t
 where
  extractTCtxTerm = \case
    SLet _ _ (LV _ x) _ mty _ _ t2 -> maybe id (Ctx.addBinding x) mty (extractTCtx t2)
    SBind mx _ mty _ c1 c2 ->
      maybe
        id
        (uncurry Ctx.addBinding)
        ((,) . lvVar <$> mx <*> mty)
        (extractTCtx c1 <> extractTCtx c2)
    SAnnotate t1 _ -> extractTCtx t1
    _ -> mempty

-- | Extract a requirements context from requirements annotations on
--   definitions contained in a term.  Should probably only be used
--   for testing.
extractReqCtx :: Syntax' ty -> ReqCtx
extractReqCtx (Syntax' _ t _ _) = extractReqCtxTerm t
 where
  extractReqCtxTerm = \case
    SLet _ _ (LV _ x) _ _ mreq _ t2 -> maybe id (Ctx.addBinding x) mreq (extractReqCtx t2)
    SBind mx _ _ mreq c1 c2 ->
      maybe
        id
        (uncurry Ctx.addBinding)
        ((,) . lvVar <$> mx <*> mreq)
        (extractReqCtx c1 <> extractReqCtx c2)
    SAnnotate t1 _ -> extractReqCtx t1
    _ -> mempty
