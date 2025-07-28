{-# LANGUAGE DataKinds #-}
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
  processParsedTermNoImports,
  processTerm',
  processParsedTerm',
  processParsedTermWithSrcMap,
  processTermEither,

  -- * Utilities
  extractTCtx,
  extractReqCtx,
  typeErrToSystemFailure,

  -- * Generic processing
  Processable (..),
) where

import Control.Algebra (Has, run)
import Control.Effect.Lift (Lift)
import Control.Effect.Error (Error)
import Control.Effect.Throw (liftEither)
import Control.Carrier.Error.Either (runError)
import Control.Lens ((^.))
import Data.Bifunctor (second)
import Data.Functor.Identity
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
import Swarm.Language.Types (TCtx, emptyTDCtx)
import Swarm.Language.Value (Env, emptyEnv, envReqs, envTydefs, envTypes)
import Swarm.Util.Effect (withError, withThrow)

processTermEither :: Text -> IO (Either SystemFailure (SourceMap Elaborated, Syntax Elaborated))
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
processTerm :: Text -> IO (Either SystemFailure (Maybe (SourceMap Elaborated, Syntax Elaborated)))
processTerm = runError . processTerm' emptyEnv

-- | Like 'processTerm', but use a term that has already been parsed.
--   (along with the original unparsed concrete syntax, for use in
--   generating error messages).
processParsedTerm :: (Text, Syntax Raw) -> IO (Either SystemFailure (SourceMap Elaborated, Syntax Elaborated))
processParsedTerm = runError . processParsedTerm' emptyEnv

-- | Like 'processParsedTerm', but don't allow any imports (and hence
--   don't require IO).  XXX this currently just crashes if any
--   imports are encountered; needs to be fixed.  Do we want imports
--   to be an error, or just ignored/not properly resolved?
processParsedTermNoImports :: (Text, Syntax Raw) -> Either SystemFailure (SourceMap Elaborated, Syntax Elaborated)
processParsedTermNoImports =
  run . runError . processParsedTermWithSrcMap mempty emptyEnv .
  second (runIdentity . traverseSyntax pure undefined)

-- | Like 'processTerm', but use explicit starting contexts.
processTerm' ::
  (Has (Lift IO) sig m, Has (Error SystemFailure) sig m) =>
  Env -> Text -> m (Maybe (SourceMap Elaborated, Syntax Elaborated))
processTerm' e txt = do
  mt <- withThrow CanNotParseMegaparsec . liftEither $ readTerm' defaultParserConfig txt
  withError (typeErrToSystemFailure txt) $ traverse (processParsedTerm' e . (txt,)) mt

-- | Like 'processTerm'', but use a term that has already been parsed
--   (along with the original unparsed concrete syntax, for use in
--   generating error messages).
processParsedTerm' ::
  (Has (Lift IO) sig m, Has (Error SystemFailure) sig m) =>
  Env -> (Text, Syntax Raw) -> m (SourceMap Elaborated, Syntax Elaborated)
processParsedTerm' e (s,t) = do
  (t', srcMap) <- buildSourceMap t
  processParsedTermWithSrcMap srcMap e (s,t')

-- | Process an already-parsed term with an explicit SourceMap.
--
--   Note that this no longer requires IO, since it is assumed that
--   any imports have already been loaded.
processParsedTermWithSrcMap ::
  (Has (Error SystemFailure) sig m) =>
  SourceMap Resolved -> Env -> (Text, Syntax Resolved) -> m (SourceMap Elaborated, Syntax Elaborated)
processParsedTermWithSrcMap srcMap e (s,t) = do
  (srcMap', tt) <- withError (typeErrToSystemFailure s) $
    inferTop (e ^. envTypes) (e ^. envReqs) (e ^. envTydefs) srcMap t
  pure $ (fmap elaborateModule srcMap', elaborate tt)

-- | Convert a 'ContextualTypeErr' into a 'SystemFailure', by
--   pretty-printing it (given the original source code) and
--   preserving the 'SrcLoc'.
typeErrToSystemFailure :: Text -> ContextualTypeErr -> SystemFailure
typeErrToSystemFailure s cte@(CTE loc _ _) = DoesNotTypecheck loc (prettyTypeErrText s cte)

------------------------------------------------------------
-- Some utility functions
------------------------------------------------------------

-- | Extract a type context from type annotations on definitions
--   contained in a term.  Should probably only be used for testing.
extractTCtx :: Syntax phase -> TCtx
extractTCtx (Syntax _ t _ _) = extractTCtxTerm t
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
extractReqCtx :: Syntax phase -> ReqCtx
extractReqCtx (Syntax _ t _ _) = extractReqCtxTerm t
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

------------------------------------------------------------
-- Generic processing of things that contain terms
------------------------------------------------------------

class Processable t where
  process :: (Has (Lift IO) sig m, Has (Error SystemFailure) sig m) => t Raw -> m (t Elaborated)
  -- XXX should m include effects to save resulting SrcMap ??

instance Processable Syntax where
  process s = do
    (s', srcMap) <- buildSourceMap s
    r <- withError (typeErrToSystemFailure "") . inferTop mempty mempty emptyTDCtx srcMap $ s'
    (pure . elaborate . snd) r
