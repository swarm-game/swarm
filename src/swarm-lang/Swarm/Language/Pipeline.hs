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
import Control.Carrier.Lift (sendIO)
import Control.Effect.Error (Error, throwError)
import Control.Effect.Lift (Lift)
import Control.Effect.Throw (Throw, liftEither)
import Control.Lens ((^.))
import Control.Monad ((<=<))
import Data.Functor (void)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Swarm.Failure (SystemFailure (..))
import Swarm.Language.Cache
import Swarm.Language.Elaborate
import Swarm.Language.Load (resolve, resolve')
import Swarm.Language.Module (Module (..), ModuleProvenance (..), emptyModule, moduleTerm)
import Swarm.Language.Parser (readTerm')
import Swarm.Language.Parser.Core (defaultParserConfig)
import Swarm.Language.Syntax
import Swarm.Language.Typecheck
import Swarm.Language.Value (Env, emptyEnv, envReqs, envTydefs, envTypes)
import Swarm.Util.Effect (withError, withThrow)
import Swarm.Util.GlobalCache (deleteCached, freezeCache, insertCached)

-- | Given raw 'Text' representing swarm-lang source code:
--
--   1. Parse it (see "Swarm.Language.Parse")
--   2. Recursively load imports (see "Swarm.Language.Load")
--   3. Typecheck the term and all imports (see "Swarm.Language.Typecheck")
--   4. Elaborate the term and all imports (see "Swarm.Language.Elaborate")
--
--   Return the end result (an elaborated Module), or @Nothing@ if the
--   input was only whitespace.
--
--   Also inserts all elaborated, imported modules into the module
--   cache, so they do not have to be reloaded + rechecked the next
--   time they are used
processSource ::
  (Has (Lift IO) sig m, Has (Error SystemFailure) sig m) =>
  -- | Provenance of the source code was obtained, relative to
  --   which imports should be interpreted.  If Nothing, use
  --   the current working directory.
  Maybe FilePath ->
  -- | Possible Env to use while typechecking.  If Nothing, use a
  --   default empty Env.
  Maybe Env ->
  -- | Text of the source code
  Text ->
  m (Module Elaborated)
processSource prov menv txt = do
  mt <- withThrow CanNotParseMegaparsec . liftEither $ readTerm' defaultParserConfig txt
  case mt of
    Nothing -> pure emptyModule
    Just t -> processTerm prov txt menv t

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
  -- | Possible Env to use while typechecking.  If Nothing, use a
  --   default empty Env.
  Maybe Env ->
  -- | Raw AST.
  Syntax Raw ->
  m (Module Elaborated)
processTerm prov txt menv tm = do
  let e = fromMaybe emptyEnv menv

  -- Resolve + recursively collect up any imports that aren't already
  -- cached
  (srcMapRes, (imps, tmRes)) <- resolve prov tm

  modCache <- sendIO $ freezeCache moduleCache

  -- Typecheck term + collected imports
  (srcMapTy, tmTy) <-
    withError (typeErrToSystemFailure txt) $
      inferTop
        (e ^. envTypes)
        (e ^. envReqs)
        (e ^. envTydefs)
        srcMapRes
        modCache
        tmRes

  -- Elaborate term + collected imports
  let tmElab = elaborate tmTy
      srcMapElab = fmap elaborateModule srcMapTy

  -- Insert all newly checked + elaborated modules into the module
  -- cache, and delete them from the environment cache in case they
  -- are replacing a previously cached + evaluated version
  let newModule loc m = insertCached moduleCache loc m >> deleteCached envCache loc
  void . sendIO $ M.traverseWithKey newModule srcMapElab

  -- Get current time, to mark elaborated module with timestamp.
  -- Probably not really important, since this module (not being
  -- loaded via an import location) won't go in the module cache.  But
  -- we might as well.
  time <- sendIO getCurrentTime

  -- Package up elaborated term as a Module.  Note that we put an
  -- empty context in the resulting Module, which is not really
  -- correct, but since we are processing a top-level term, we won't
  -- ever use this module as an import to some other module, so the
  -- context is not really needed.
  let modElab = Module (Just tmElab) mempty imps (Just time) (maybe NoProvenance FromFile prov)

  -- Return the elaborated module.
  pure modElab

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
  m (Module Elaborated)
processTermNoImports txt tm menv = do
  let e = fromMaybe emptyEnv menv
  tmRes <- resolve' tm
  (_, tmTy) <-
    withError (typeErrToSystemFailure txt) $
      inferTop
        (e ^. envTypes)
        (e ^. envReqs)
        (e ^. envTydefs)
        M.empty
        (const Nothing)
        tmRes
  pure $ Module (Just $ elaborate tmTy) (mempty, mempty) S.empty Nothing NoProvenance

------------------------------------------------------------
-- Utility adapters for processTerm
------------------------------------------------------------

-- | Convert a 'ContextualTypeErr' into a 'SystemFailure', by
--   pretty-printing it (given the original source code) and
--   preserving the 'SrcLoc'.
typeErrToSystemFailure :: Text -> ContextualTypeErr -> SystemFailure
typeErrToSystemFailure s cte@(CTE loc _ _) = DoesNotTypecheck loc (prettyTypeErrText s cte)

-- | Require a term to be non-empty, throwing an error about the term
--   consisting only of whitespace otherwise.
requireNonNothing :: Has (Throw SystemFailure) sig m => Maybe t -> m t
requireNonNothing = maybe (throwError EmptyTerm) pure

-- | Extract the term contained in a module, requiring it to be
--   non-empty, and throwing an error about the term consisting only
--   of whitespace otherwise.  Appropriate for use with the output of
--   'processTerm'.
requireNonEmptyTerm :: Has (Throw SystemFailure) sig m => Module phase -> m (Syntax phase)
requireNonEmptyTerm = requireNonNothing . moduleTerm

------------------------------------------------------------
-- Generic processing of things that contain terms
------------------------------------------------------------

class Processable t where
  process :: (Has (Lift IO) sig m, Has (Error SystemFailure) sig m) => t Raw -> m (t Elaborated)

instance Processable Module where
  process = \case
    Module Nothing _ _ ts prov -> pure $ Module Nothing mempty S.empty ts prov
    Module (Just t) _ _ _ prov -> processTerm (fileProv prov) "" Nothing t
   where
    fileProv = \case
      FromFile f -> Just f
      _ -> Nothing

-- | Process syntax, but deliberately throw away information about
--   imports.  Used e.g. for processing code embedded in markdown.
processSyntax :: (Has (Lift IO) sig m, Has (Error SystemFailure) sig m) => Syntax Raw -> m (Syntax Elaborated)
processSyntax = (requireNonNothing . moduleTerm) <=< processTerm Nothing "" Nothing
