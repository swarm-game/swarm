-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Swarm.Language.Pipeline
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
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
  showTypeErrorPos,
) where

import Data.Bifunctor (first)
import Data.Data (Data)
import Data.Set (Set)
import Data.Text (Text)
import Data.Yaml as Y
import Witch

import Swarm.Language.Capability
import Swarm.Language.Context
import Swarm.Language.Elaborate
import Swarm.Language.Parse
import Swarm.Language.Pretty
import Swarm.Language.Syntax
import Swarm.Language.Typecheck
import Swarm.Language.Types

-- | A record containing the results of the language processing
--   pipeline.  Put a 'Term' in, and get one of these out.
data ProcessedTerm
  = ProcessedTerm
      Term
      -- ^ The elaborated term
      TModule
      -- ^ The type of the term (and of any embedded definitions)
      (Set Capability)
      -- ^ Capabilities required by the term
      CapCtx
      -- ^ Capability context for any definitions embedded in the term
  deriving (Data, Show)

instance FromJSON ProcessedTerm where
  parseJSON = withText "Term" tryProcess
   where
    tryProcess :: Text -> Y.Parser ProcessedTerm
    tryProcess t = case processTerm t of
      Left err -> fail $ "Could not parse term: " ++ from err
      Right pt -> return pt

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

-- | Like 'processTerm', but use a term that has already been parsed.
processParsedTerm :: Syntax -> Either TypeErr ProcessedTerm
processParsedTerm = processParsedTerm' empty empty

-- | Like 'processTerm', but use explicit starting contexts.
processTerm' :: TCtx -> CapCtx -> Text -> Either Text ProcessedTerm
processTerm' ctx capCtx txt = do
  t <- readTerm txt
  first (prettyTypeErr txt) $ processParsedTerm' ctx capCtx t

prettyTypeErr :: Text -> TypeErr -> Text
prettyTypeErr code te = teLoc <> prettyText te
 where
  teLoc = case getTypeErrLocation te of
    Just (Location s e) -> (from . show . fst . fst $ getLocRange code (s, e)) <> ": "
    _anyOtherLoc -> ""

showTypeErrorPos :: Text -> TypeErr -> ((Int, Int), (Int, Int), Text)
showTypeErrorPos code te = (minusOne start, minusOne end, msg)
 where
  minusOne (x, y) = (x - 1, y - 1)

  (start, end) = case getTypeErrLocation te of
    Just (Location s e) -> getLocRange code (s, e)
    _anyOtherLoc -> ((1, 1), (65535, 65535)) -- unknown loc spans the whole document
  msg = prettyText te

-- | Like 'processTerm'', but use a term that has already been parsed.
processParsedTerm' :: TCtx -> CapCtx -> Syntax -> Either TypeErr ProcessedTerm
processParsedTerm' ctx capCtx t = do
  ty <- inferTop ctx t
  let (caps, capCtx') = requiredCaps capCtx (sTerm t)
  return $ ProcessedTerm (elaborate (sTerm t)) ty caps capCtx'
