{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Some orphan @To/FromJSON@ instances for terms and values.  We have
-- to put them all here to avoid circular module dependencies.
module Swarm.Language.JSON where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Aeson qualified as Ae
import Swarm.Language.Pipeline (processTermEither)
import Swarm.Language.Syntax (Term)
import Swarm.Language.Syntax.Pattern (Syntax, TSyntax)
import Swarm.Language.Value (Env, Value)
import Swarm.Pretty (prettyText)
import Witch (into)

instance FromJSON TSyntax where
  parseJSON = withText "Term" $ either (fail . into @String) return . processTermEither

instance ToJSON TSyntax where
  toJSON = Ae.String . prettyText

instance FromJSON Term
instance FromJSON Syntax
instance ToJSON Term
instance ToJSON Syntax

instance ToJSON Value where
  toJSON = undefined

instance FromJSON Value where
  parseJSON = undefined

instance ToJSON Env where
  toJSON = undefined

instance FromJSON Env where
  parseJSON = undefined
