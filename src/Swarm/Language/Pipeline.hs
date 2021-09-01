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

{-# LANGUAGE TypeOperators #-}

module Swarm.Language.Pipeline
  ( processTerm
  , processCmd
  ) where

import           Data.Bifunctor           (first)
import qualified Data.Map                 as M
import           Data.Text                (Text)

import           Swarm.Language.Elaborate
import           Swarm.Language.Parse
import           Swarm.Language.Pretty
import           Swarm.Language.Syntax
import           Swarm.Language.Typecheck
import           Swarm.Language.Types
import           Swarm.Util

-- | Like 'processTerm', but assume the program is supposed to have type
--   @cmd ()@ (/i.e./ we start off the type checker in checking mode
--   instead of inference mode).
processCmd :: Text -> Either Text ATerm
processCmd txt = do
  t <- readTerm txt
  let ty = TyCmd TyUnit
  at <- first prettyText (check M.empty t ty)
  return $ elaborate ty at

-- | Given a 'Text' value representing a Swarm program,
--
--   1. Parse it (see "Swarm.Language.Parse")
--   2. Typecheck it (see "Swarm.Language.Typecheck")
--   3. Elaborate it (see "Swarm.Language.Elaborate")
--
--   Return either the end result (an 'ATerm' paired with its 'Type'),
--   or a pretty-printed error message.
processTerm :: Text -> Either Text (ATerm ::: Type)
processTerm txt = do
  t <- readTerm txt
  at ::: ty <- first prettyText (infer M.empty t)
  return $ elaborate ty at ::: ty
