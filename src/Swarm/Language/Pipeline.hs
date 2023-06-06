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
  ProcessedTerm (..),
  processTerm,
  processParsedTerm,
  processTerm',
  processParsedTerm',
  processTermEither,
) where

import Control.Lens ((^.))
import Data.Bifunctor (first)
import Data.Data (Data)
import Data.Text (Text)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Language.Context
import Swarm.Language.Elaborate
import Swarm.Language.Module
import Swarm.Language.Parse
import Swarm.Language.Pretty
import Swarm.Language.Requirement
import Swarm.Language.Syntax
import Swarm.Language.Typecheck
import Swarm.Language.Types
import Witch

-- | A record containing the results of the language processing
--   pipeline.  Put a 'Term' in, and get one of these out.
data ProcessedTerm
  = ProcessedTerm
      TModule
      -- ^ The elaborated + type-annotated term, plus types of any embedded definitions
      Requirements
      -- ^ Requirements of the term
      ReqCtx
      -- ^ Capability context for any definitions embedded in the term
  deriving (Data, Show, Eq, Generic)

processTermEither :: Text -> Either String ProcessedTerm
processTermEither t = case processTerm t of
  Left err -> Left $ "Could not parse term: " ++ from err
  Right Nothing -> Left "Term was only whitespace"
  Right (Just pt) -> Right pt

instance FromJSON ProcessedTerm where
  parseJSON = withText "Term" $ either fail return . processTermEither

instance ToJSON ProcessedTerm where
  toJSON (ProcessedTerm t _ _) = String $ prettyText (moduleAST t)

-- | Given a 'Text' value representing a Swarm program,
--
--   1. Parse it (see "Swarm.Language.Parse")
--   2. Typecheck it (see "Swarm.Language.Typecheck")
--   3. Elaborate it (see "Swarm.Language.Elaborate")
--   4. Check what capabilities it requires (see "Swarm.Language.Capability")
--
--   Return either the end result (or @Nothing@ if the input was only
--   whitespace) or a pretty-printed error message.
processTerm :: Text -> Either Text (Maybe ProcessedTerm)
processTerm = processTerm' empty empty

-- | Like 'processTerm', but use a term that has already been parsed.
processParsedTerm :: Syntax -> Either ContextualTypeErr ProcessedTerm
processParsedTerm = processParsedTerm' empty empty

-- | Like 'processTerm', but use explicit starting contexts.
processTerm' :: TCtx -> ReqCtx -> Text -> Either Text (Maybe ProcessedTerm)
processTerm' ctx capCtx txt = do
  mt <- readTerm txt
  first (prettyTypeErr txt) $ traverse (processParsedTerm' ctx capCtx) mt

-- | Like 'processTerm'', but use a term that has already been parsed.
processParsedTerm' :: TCtx -> ReqCtx -> Syntax -> Either ContextualTypeErr ProcessedTerm
processParsedTerm' ctx capCtx t = do
  m <- inferTop ctx t
  let (caps, capCtx') = requirements capCtx (t ^. sTerm)
  return $ ProcessedTerm (elaborateModule m) caps capCtx'

elaborateModule :: TModule -> TModule
elaborateModule (Module ast ctx) = Module (elaborate ast) ctx
