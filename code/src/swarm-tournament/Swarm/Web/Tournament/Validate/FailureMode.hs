{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Failure modes for validating an uploaded scenario
module Swarm.Web.Tournament.Validate.FailureMode where

import Control.Exception.Base (displayException)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Encoding.Error (UnicodeException)
import Data.Yaml (ParseException)
import Swarm.Game.Failure (SystemFailure)
import Swarm.Game.State (Sha1 (..))
import Swarm.Util (parens, showT)
import System.Time.Extra (Seconds, showDuration)

class Describable a where
  describeText :: a -> T.Text

newtype GenericUploadFailure = GenericUploadFailure FileUploadFailure

instance Describable GenericUploadFailure where
  describeText (GenericUploadFailure x) = describeText x

data FileUploadFailure
  = NoFileSupplied
  | MultipleFiles Int

instance Describable FileUploadFailure where
  describeText NoFileSupplied = "Must supply a file!"
  describeText (MultipleFiles count) =
    T.unwords
      [ "Only one file is allowed! Provided"
      , showT count
      ]

newtype ContextInitializationFailure = ContextInitializationFailure SystemFailure

instance Describable ContextInitializationFailure where
  describeText (ContextInitializationFailure x) = showT x

data SolutionEvaluationFailure
  = SolutionGameStateInitializationFailure ContextInitializationFailure
  | SolutionExecutionTimeout Seconds
  | ErrorsDuringExecution (NE.NonEmpty T.Text)

instance Describable SolutionEvaluationFailure where
  describeText (SolutionGameStateInitializationFailure x) = describeText x
  describeText (SolutionExecutionTimeout s) =
    T.unwords
      [ "Timed out - this likely means that the solution did not work."
      , "Limit is"
      , T.pack $ showDuration s
      ]
  describeText (ErrorsDuringExecution x) = T.unlines $ NE.toList x

data ScenarioInstantiationFailure
  = ScenarioEnvironmentFailure ContextInitializationFailure
  | YamlDecodeError ParseException
  | ScenarioParseFailure String

instance Describable ScenarioInstantiationFailure where
  describeText (ScenarioEnvironmentFailure x) = describeText x
  describeText (YamlDecodeError x) = T.pack $ displayException x
  describeText (ScenarioParseFailure x) = T.pack x

data ScenarioUploadValidationFailure
  = ScenarioUploadFailure GenericUploadFailure
  | NoSolutionProvided
  | ScenarioUploadInstantiationFailure ScenarioInstantiationFailure
  | ScenarioSolutionEvaluationFailure SolutionEvaluationFailure

instance Describable ScenarioUploadValidationFailure where
  describeText (ScenarioUploadFailure x) = describeText x
  describeText NoSolutionProvided = "No solution to test!"
  describeText (ScenarioUploadInstantiationFailure x) = describeText x
  describeText (ScenarioSolutionEvaluationFailure x) = describeText x

data ScenarioRetrievalFailure
  = DatabaseRetrievalFailure Sha1
  | RetrievedInstantiationFailure ScenarioInstantiationFailure
  | DecodingFailure UnicodeException
  | YamlParseFailure ParseException

instance Describable ScenarioRetrievalFailure where
  describeText (DatabaseRetrievalFailure (Sha1 h)) =
    T.unwords
      [ "Scenario with hash"
      , T.pack h
      , "not found"
      ]
  describeText (RetrievedInstantiationFailure x) = describeText x
  describeText (DecodingFailure x) = T.pack $ displayException x
  describeText (YamlParseFailure x) = T.pack $ displayException x

data SolutionSubmissionFailure
  = SolutionUploadFailure GenericUploadFailure
  | MissingScenarioParameter String
  | SubmittedSolutionEvaluationFailure SolutionEvaluationFailure
  | SolutionUnicodeError UnicodeException
  | SolutionParseError T.Text
  | ScenarioRetrievalFailure ScenarioRetrievalFailure
  | CachedSolutionScenarioMismatch Sha1 Sha1

instance Describable SolutionSubmissionFailure where
  describeText (SolutionUploadFailure x) = describeText x
  describeText (MissingScenarioParameter x) = T.pack x
  describeText (SubmittedSolutionEvaluationFailure x) = describeText x
  describeText (SolutionUnicodeError x) = T.pack $ displayException x
  describeText (SolutionParseError x) = x
  describeText (ScenarioRetrievalFailure x) = describeText x
  describeText (CachedSolutionScenarioMismatch (Sha1 userSuppliedScenarioSha1) (Sha1 retrievedScenarioHash)) =
    T.unwords
      [ "User-supplied scenario hash"
      , parens $ T.pack userSuppliedScenarioSha1
      , "did not match scenario hash for previously computed solution"
      , parens $ T.pack retrievedScenarioHash
      ]
