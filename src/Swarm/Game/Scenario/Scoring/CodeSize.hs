module Swarm.Game.Scenario.Scoring.CodeSize where

import Control.Monad (guard)
import Data.Aeson
import GHC.Generics (Generic)
import Swarm.Language.Module
import Swarm.Language.Pipeline
import Swarm.Language.Syntax

data CodeSizeDeterminators = CodeSizeDeterminators
  { initialCode :: Maybe ProcessedTerm
  , hasUsedREPL :: Bool
  }
  deriving (Show)

data ScenarioCodeMetrics = ScenarioCodeMetrics
  { sourceTextLength :: Int
  , astSize :: Int
  }
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

charCount :: SrcLoc -> Int
charCount NoLoc = 0
charCount (SrcLoc start end) = end - start

codeSizeFromDeterminator :: CodeSizeDeterminators -> Maybe ScenarioCodeMetrics
codeSizeFromDeterminator (CodeSizeDeterminators maybeInitialCode usedRepl) = do
  guard $ not usedRepl
  ProcessedTerm (Module s@(Syntax' srcLoc _ _) _) _ _ <- maybeInitialCode
  return $ ScenarioCodeMetrics (charCount srcLoc) (measureAstSize s)
