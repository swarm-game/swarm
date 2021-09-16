-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Language.Pipeline
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Some convenient functions for putting together the whole Swarm
-- language processing pipeline: parsing, type checking, and
-- elaboration.  If you want to simply turn some raw text representing
-- a Swarm program into something useful, this is probably the module
-- you want.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Swarm.Language.Pipeline
  ( ProcessedTerm(..)
  , processTerm
  , processTerm'
  ) where

import           Data.Bifunctor            (first)
import           Data.Set                  (Set)
import           Data.Text                 (Text)

import           Swarm.Language.Capability
import           Swarm.Language.Context
import           Swarm.Language.Elaborate
import           Swarm.Language.Parse
import           Swarm.Language.Pretty
import           Swarm.Language.Syntax
import           Swarm.Language.Typecheck
import           Swarm.Language.Types

data ProcessedTerm = ProcessedTerm
  Term              -- ^ The elaborated term
  TModule           -- ^ The type of the term (and of embedded definitions)
  (Set Capability)  -- ^ Capabilities required by the term
  CapCtx            -- ^ Capability context for definitions embedded in the term

-- | Given a 'Text' value representing a Swarm program,
--
--   1. Parse it (see "Swarm.Language.Parse")
--   2. Typecheck it (see "Swarm.Language.Typecheck")
--   3. Elaborate it (see "Swarm.Language.Elaborate")
--   4. Check what capabilities it requires (see "Swarm.Language.Capability")
--
--   Return either the end result or a pretty-printed error message.
processTerm :: Text -> Either Text ProcessedTerm
processTerm = processTerm' empty empty

-- | Like 'processTerm', but use explicit starting contexts.
processTerm' :: TCtx -> CapCtx -> Text -> Either Text ProcessedTerm
processTerm' ctx capCtx txt = do
  t <- readTerm txt
  ty <- first prettyText (inferTop ctx t)
  let (caps, capCtx') = requiredCaps capCtx t
  return $ ProcessedTerm (elaborate t) ty caps capCtx'

