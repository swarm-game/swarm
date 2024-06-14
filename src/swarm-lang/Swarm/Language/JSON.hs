{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- XXX comment me
module Swarm.Language.JSON where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON, withText)
import Data.Aeson qualified as Ae
import Swarm.Language.Pipeline (processTermEither)
import Swarm.Language.Pretty (prettyText)
import Swarm.Language.Syntax (Term)
import Swarm.Language.Syntax.Pattern (Syntax, TSyntax)
import Swarm.Language.Value (Env, Value)
import Swarm.Util.JSON (optionsMinimize)
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
  toJSON = genericToJSON optionsMinimize

instance FromJSON Value where
  parseJSON = genericParseJSON optionsMinimize

deriving instance FromJSON Env
deriving instance ToJSON Env
