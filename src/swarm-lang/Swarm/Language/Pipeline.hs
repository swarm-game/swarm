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
  processParsedTermWithSrcMap,
  processTermEither,

  -- * Utilities
  extractTCtx,
  extractReqCtx,
) where

import Control.Algebra (Has)
import Control.Effect.Lift (Lift)
import Control.Effect.Error (Error)
import Control.Effect.Throw (liftEither)
import Control.Carrier.Error.Either (runError)
import Control.Lens ((^.))
import Data.Text (Text)
import Swarm.Failure (SystemFailure (..))
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Elaborate
import Swarm.Language.Load (buildSourceMap, SourceMap)
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig)
import Swarm.Language.Requirements.Type (ReqCtx)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck
import Swarm.Language.Types (TCtx)
import Swarm.Language.Value (Env, emptyEnv, envReqs, envTydefs, envTypes)
import Swarm.Util.Effect (withError, withThrow)

processTermEither :: Text -> IO (Either SystemFailure TSyntax)
processTermEither t = do
  res <- processTerm t
  pure $ case res of
    Left err -> Left err
    Right Nothing -> Left (CustomFailure "Term was only whitespace")
    Right (Just pt) -> Right pt

-- | Given a 'Text' value representing a Swarm program,
--
--   1. Parse it (see "Swarm.Language.Parse")
--   2. Typecheck it (see "Swarm.Language.Typecheck")
--   3. Elaborate it (see "Swarm.Language.Elaborate")
--
--   Return either the end result (or @Nothing@ if the input was only
--   whitespace) or a pretty-printed error message.
processTerm :: Text -> IO (Either SystemFailure (Maybe TSyntax))
processTerm = runError . processTerm' emptyEnv

-- | Like 'processTerm', but use a term that has already been parsed.
processParsedTerm :: Syntax -> IO (Either SystemFailure TSyntax)
processParsedTerm = runError . processParsedTerm' emptyEnv

-- | Like 'processTerm', but use explicit starting contexts.
processTerm' ::
  (Has (Lift IO) sig m, Has (Error SystemFailure) sig m) =>
  Env -> Text -> m (Maybe TSyntax)
processTerm' e txt = do
  mt <- withThrow CanNotParseMegaparsec . liftEither $ readTerm' defaultParserConfig txt
  withError (DoesNotTypecheck . prettyTypeErrText txt) $ traverse (processParsedTerm' e) mt

-- | Like 'processTerm'', but use a term that has already been parsed.
processParsedTerm' ::
  (Has (Lift IO) sig m, Has (Error SystemFailure) sig m) =>
  Env -> Syntax -> m TSyntax
processParsedTerm' e t = do
  srcMap <- buildSourceMap t
  processParsedTermWithSrcMap srcMap e t

-- | Process an already-parsed term with an explicit SourceMap.
--
--   Note that this no longer requires IO, since it is assumed that
--   any imports have already been loaded.
processParsedTermWithSrcMap ::
  (Has (Error SystemFailure) sig m) =>
  SourceMap -> Env -> Syntax -> m TSyntax
processParsedTermWithSrcMap srcMap e t = do
  tt <- withError (DoesNotTypecheck . prettyTypeErrText "") $  -- XXX fix me: need src
    inferTop (e ^. envTypes) (e ^. envReqs) (e ^. envTydefs) srcMap t
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
