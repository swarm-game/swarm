-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types and utilities to compute code size
-- in terms of textual length and AST nodes.
module Swarm.Game.Scenario.Scoring.CodeSize where

import Control.Monad (guard)
import Data.Aeson
import Data.Data (Data)
import GHC.Generics (Generic)
import Swarm.Language.Syntax

data CodeSizeDeterminators = CodeSizeDeterminators
  { initialCode :: Maybe TSyntax
  , hasUsedREPL :: Bool
  }
  deriving (Show)

data ScenarioCodeMetrics = ScenarioCodeMetrics
  { sourceTextLength :: Int
  , astSize :: Int
  }
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

codeMetricsFromSyntax ::
  Data a =>
  Syntax' a ->
  ScenarioCodeMetrics
codeMetricsFromSyntax s@(Syntax' srcLoc _ _ _) =
  ScenarioCodeMetrics (charCount srcLoc) (measureAstSize s)
 where
  charCount :: SrcLoc -> Int
  charCount NoLoc = 0
  charCount (SrcLoc start end) = end - start

codeSizeFromDeterminator :: CodeSizeDeterminators -> Maybe ScenarioCodeMetrics
codeSizeFromDeterminator (CodeSizeDeterminators maybeInitialCode usedRepl) = do
  guard $ not usedRepl
  codeMetricsFromSyntax <$> maybeInitialCode
