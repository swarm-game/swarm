-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Types and utilities to compute code size
-- in terms of textual length and AST nodes.
module Swarm.Game.Scenario.Scoring.CodeSize where

import Data.Aeson
import Data.Semigroup (Sum (..))
import Data.Set qualified as S
import GHC.Generics (Generic)
import Swarm.Language.Cache (moduleCache)
import Swarm.Language.Module (Module, moduleTerm, moduleTransImports)
import Swarm.Language.Syntax
import Swarm.Util.GlobalCache qualified as GC

data CodeSizeDeterminators = CodeSizeDeterminators
  { initialCode :: Maybe (Module Elaborated)
  , hasUsedREPL :: Bool
  }
  deriving (Show)

-- | Take an AST metric, call it on on all transitive imports, and
--   collect the results.
transitiveMetric :: forall metric. Monoid metric => (Syntax Elaborated -> metric) -> Module Elaborated -> IO metric
transitiveMetric measure m = do
  let timps = S.toList $ moduleTransImports m
      measureMM :: Maybe (Module Elaborated) -> metric
      measureMM = maybe mempty measure . (>>= moduleTerm)
  impMeas <- mconcat <$> traverse (fmap measureMM . GC.lookupCached moduleCache) timps
  pure (measureMM (Just m) <> impMeas)

data ScenarioCodeMetrics = ScenarioCodeMetrics
  { sourceTextLength :: Int
  , astSize :: Int
  }
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

codeMetricsFromSyntax :: Module Elaborated -> IO ScenarioCodeMetrics
codeMetricsFromSyntax m =
  ScenarioCodeMetrics
    <$> fmap getSum (transitiveMetric measureASTChars m)
    <*> fmap getSum (transitiveMetric measureASTSize m)

codeSizeFromDeterminator :: CodeSizeDeterminators -> IO (Maybe ScenarioCodeMetrics)
codeSizeFromDeterminator (CodeSizeDeterminators maybeInitialCode usedRepl) =
  case usedRepl of
    True -> pure Nothing
    False -> traverse codeMetricsFromSyntax maybeInitialCode
