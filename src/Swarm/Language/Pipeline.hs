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
  ( processTerm
  , processTerm'
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

-- | Given a 'Text' value representing a Swarm program,
--
--   1. Parse it (see "Swarm.Language.Parse")
--   2. Typecheck it (see "Swarm.Language.Typecheck")
--   3. Elaborate it (see "Swarm.Language.Elaborate")
--
--   Return either the end result (a 'Term' paired with its type), or a pretty-printed
--   error message.
processTerm :: Text -> Either Text (Term ::: Type)
processTerm = processTerm' M.empty

-- | Like 'processTerm', but use an explicit starting context.
processTerm' :: Ctx -> Text -> Either Text (Term ::: Type)
processTerm' ctx txt = do
  t <- readTerm txt
  ty <- first prettyText (infer ctx t)
  return $ elaborate t ::: ty

