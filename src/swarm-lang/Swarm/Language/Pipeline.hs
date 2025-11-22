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
  processSource,
  processTerm,
  processTermNoImports,

  -- * Utilities
  typeErrToSystemFailure,
  requireNonEmptyTerm,

  -- * Generic processing
  Processable (..),
  processSyntax,
) where

import Control.Algebra (Has)
import Control.Effect.Error (Error, throwError)
import Control.Effect.Lift (Lift)
import Control.Effect.Throw (liftEither)
import Control.Lens ((^.))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import Swarm.Failure (SystemFailure (..))
import Swarm.Language.Elaborate
import Swarm.Language.Load (SyntaxWithImports (..), eraseSourceMap, resolve, resolve')
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck
import Swarm.Language.Types (emptyTDCtx)
import Swarm.Language.Value (Env, emptyEnv, envReqs, envSourceMap, envTydefs, envTypes)
import Swarm.Util.Effect (withError, withThrow)

-- | Given raw 'Text' representing swarm-lang source code:
--
--   1. Parse it (see "Swarm.Language.Parse")
--   2. Recursively load imports (see "Swarm.Language.Load")
--   3. Typecheck the term and all imports (see "Swarm.Language.Typecheck")
--   4. Elaborate the term and all imports (see "Swarm.Language.Elaborate")
--
--   Return the end result (an elaborated term + source map for
--   imports), or @Nothing@ if the input was only whitespace.
processSource ::
  (Has (Lift IO) sig m, Has (Error SystemFailure) sig m) =>
  -- | Provenance of the source code was obtained, relative to
  --   which imports should be interpreted.  If Nothing, use
  --   the current working directory.
  Maybe FilePath ->
  -- | Text of the source code
  Text ->
  -- | Possible Env to use while typechecking.  If Nothing, use a
  --   default empty Env.
  Maybe Env ->
  m (Maybe (SyntaxWithImports Elaborated))
processSource prov txt menv = do
  mt <- withThrow CanNotParseMegaparsec . liftEither $ readTerm' defaultParserConfig txt
  for mt $ \t -> processTerm prov txt t menv

-- | Like 'processSource', but start with an already-parsed raw AST.
processTerm ::
  forall sig m.
  (Has (Lift IO) sig m, Has (Error SystemFailure) sig m) =>
  -- | Provenance of the source code was obtained, relative to
  --   which imports should be interpreted.  If Nothing, use
  --   the current working directory.
  Maybe FilePath ->
  -- | Text of the source code, used to generate error messages
  Text ->
  -- | Raw AST.
  Syntax Raw ->
  -- | Possible Env to use while typechecking.  If Nothing, use a
  --   default empty Env.
  Maybe Env ->
  m (SyntaxWithImports Elaborated)
processTerm prov txt t menv = do
  let e = fromMaybe emptyEnv menv
  SyntaxWithImports _ srcMapRes tRes <- resolve prov t
  SyntaxWithImports _ srcMapTy tTy <-
    withError (typeErrToSystemFailure txt) $
      inferTop
        prov
        (e ^. envTypes)
        (e ^. envReqs)
        (e ^. envTydefs)
        (srcMapRes <> eraseSourceMap (e ^. envSourceMap))
        tRes
  -- XXX what srcMap to use here?  Make sure, and write a note about it
  pure $ SyntaxWithImports prov (fmap elaborateModule srcMapTy) (elaborate tTy)

-- | Like 'processTerm', but don't allow any imports that need to be
--   loaded (and hence would require IO).  If any imports are
--   encountered, throw an error.
processTermNoImports ::
  forall sig m.
  (Has (Error SystemFailure) sig m) =>
  -- | Text of the source code, used to generate error messages
  Text ->
  -- | Raw AST.
  Syntax Raw ->
  -- | Possible Env to use while typechecking.  If Nothing, use a
  --   default empty Env.
  Maybe Env ->
  m (Syntax Elaborated)
processTermNoImports txt t menv = do
  let e = fromMaybe emptyEnv menv
  tRes <- resolve' t
  SyntaxWithImports _ _ tTy <-
    withError (typeErrToSystemFailure txt) $
      inferTop
        Nothing
        (e ^. envTypes)
        (e ^. envReqs)
        (e ^. envTydefs)
        M.empty
        tRes
  pure $ elaborate tTy

------------------------------------------------------------
-- Utility adapters for processTerm
------------------------------------------------------------

-- | Convert a 'ContextualTypeErr' into a 'SystemFailure', by
--   pretty-printing it (given the original source code) and
--   preserving the 'SrcLoc'.
typeErrToSystemFailure :: Text -> ContextualTypeErr -> SystemFailure
typeErrToSystemFailure s cte@(CTE loc _ _) = DoesNotTypecheck loc (prettyTypeErrText s cte)

-- | Require a term to be non-empty, throwing an error about the term
--   consisting only of whitespace otherwise.  Appropriate for use
--   with the output of 'processTerm'.
requireNonEmptyTerm :: Has (Error SystemFailure) sig m => Maybe t -> m t
requireNonEmptyTerm = maybe (throwError EmptyTerm) pure

------------------------------------------------------------
-- Generic processing of things that contain terms
------------------------------------------------------------

class Processable t where
  process :: (Has (Lift IO) sig m, Has (Error SystemFailure) sig m) => t Raw -> m (t Elaborated)

instance Processable SyntaxWithImports where
  process (SyntaxWithImports prov _ t) = do
    SyntaxWithImports _ srcMapRes tRes <- resolve prov t
    SyntaxWithImports _ srcMapTy tTy <- withError (typeErrToSystemFailure "") . inferTop prov mempty mempty emptyTDCtx srcMapRes $ tRes
    pure $ SyntaxWithImports prov (M.map elaborateModule srcMapTy) (elaborate tTy)

-- | Process syntax, but deliberately throw away information about
--   imports.  Used e.g. for processing code embedded in markdown.
processSyntax :: (Has (Lift IO) sig m, Has (Error SystemFailure) sig m) => Syntax Raw -> m (Syntax Elaborated)
processSyntax = fmap getSyntax . process . SyntaxWithImports Nothing mempty
