{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Doc markup utilities
--
-- Utilities for generating doc markup
module Swarm.Doc.Util where

import Control.Effect.Throw (Has, Throw, throwError)
import Control.Lens (view)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Failure (SystemFailure (CustomFailure))
import Swarm.Game.Robot (Robot)
import Swarm.Game.Scenario (ScenarioLandscape, scenarioRobots)
import Swarm.Language.Syntax (Const (..))
import Swarm.Language.Syntax qualified as Syntax

-- * Text operations

wrap :: Char -> Text -> Text
wrap c = T.cons c . flip T.snoc c

codeQuote :: Text -> Text
codeQuote = wrap '`'

addLink :: Text -> Text -> Text
addLink l t = T.concat ["[", t, "](", l, ")"]

-- * Common symbols

operators :: [Const]
operators = filter Syntax.isOperator Syntax.allConst

builtinFunctions :: [Const]
builtinFunctions = filter Syntax.isBuiltinFunction Syntax.allConst

commands :: [Const]
commands = filter Syntax.isCmd Syntax.allConst

-- * Other operations

constSyntax :: Const -> Text
constSyntax = Syntax.syntax . Syntax.constInfo
