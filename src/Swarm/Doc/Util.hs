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
import Swarm.Game.Failure (SystemFailure (CustomFailure))
import Swarm.Game.Robot (Robot)
import Swarm.Game.Robot.Concrete (instantiateRobot)
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

tshow :: (Show a) => a -> Text
tshow = T.pack . show

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

instantiateBaseRobot :: Has (Throw SystemFailure) sig m => ScenarioLandscape -> m Robot
instantiateBaseRobot sLandscape = case listToMaybe $ view scenarioRobots sLandscape of
  Just r -> pure $ instantiateRobot Nothing 0 r
  Nothing -> throwError $ CustomFailure "Scenario contains no robots"
