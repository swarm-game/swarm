-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types and utilities to compute code size
-- in terms of textual length and AST nodes.
module Swarm.Game.Scenario.Scoring.CodeSize where

import Control.Monad (guard)
import Data.Aeson
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Swarm.Language.Syntax
import Swarm.Language.Phase (ImportPhaseFor)

data CodeSizeDeterminators = CodeSizeDeterminators
  { initialCode :: Maybe (Syntax Raw)
  , hasUsedREPL :: Bool
  }
  deriving (Show)

data ScenarioCodeMetrics = ScenarioCodeMetrics
  { sourceTextLength :: Int
  , astSize :: Int
  }
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

codeMetricsFromSyntax ::
  (Data (SwarmType phase), Typeable phase, Typeable (ImportPhaseFor phase)) =>
  Syntax phase ->
  ScenarioCodeMetrics
codeMetricsFromSyntax s@(Syntax srcLoc _ _ _) =
  ScenarioCodeMetrics (charCount srcLoc) (measureAstSize s)
 where
  charCount :: SrcLoc -> Int
  charCount NoLoc = 0
  charCount (SrcLoc _ start end) = end - start

codeSizeFromDeterminator :: CodeSizeDeterminators -> Maybe ScenarioCodeMetrics
codeSizeFromDeterminator (CodeSizeDeterminators maybeInitialCode usedRepl) = do
  guard $ not usedRepl
  codeMetricsFromSyntax <$> maybeInitialCode
