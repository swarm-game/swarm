{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Validates an uploaded scenario
module Swarm.Web.Tournament.Validate where

import Control.Arrow (left)
import Control.Carrier.Accum.Strict (evalAccum)
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Throw.Either (runThrow)
import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Except
import Data.ByteString.Lazy qualified as LBS
import Data.Either.Extra (maybeToEither)
import Data.Sequence (Seq)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Yaml (decodeEither', parseEither)
import Servant.Multipart
import Swarm.Failure (SystemFailure)
import Swarm.Game.CESK (continue)
import Swarm.Game.Robot.Concrete (machine)
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Scoring.CodeSize (codeMetricsFromSyntax)
import Swarm.Game.Scenario.Status (ScenarioWith (..), emptyLaunchParams)
import Swarm.Game.State
import Swarm.Game.State.Initialize (scenarioToGameState)
import Swarm.Game.State.Runtime (RuntimeOptions (..), initRuntimeState, initScenarioInputs, pauseOnObjectiveCompletion, stdGameConfigInputs)
import Swarm.Game.State.Substate (initState, seed)
import Swarm.Game.Step.Validate (playUntilWin)
import Swarm.Language.Load (SyntaxWithImports, getSyntax)
import Swarm.Language.Pipeline
import Swarm.Language.Syntax (Phase (..))
import Swarm.Pretty (prettyString, prettyText)
import Swarm.Util.Yaml
import Swarm.Web.Tournament.Database.Query
import Swarm.Web.Tournament.Type
import Swarm.Web.Tournament.Validate.FailureMode
import Swarm.Web.Tournament.Validate.Upload
import System.Time.Extra

newtype SolutionTimeout = SolutionTimeout Seconds

data CommonValidationArgs m a
  = CommonValidationArgs
      SolutionTimeout
      (PersistenceArgs m a)

validateScenarioUpload ::
  CommonValidationArgs IO ScenarioUploadResponsePayload ->
  -- | Game version
  Sha1 ->
  IO (Either ScenarioUploadValidationFailure ScenarioCharacterization)
validateScenarioUpload (CommonValidationArgs solnTimeout persistenceArgs) gameVersion =
  runExceptT $ do
    (fileMeta, solnMetrics) <-
      withFileCache
        persistenceArgs
        ScenarioUploadFailure
        computeMetrics

    pure $
      ScenarioCharacterization
        fileMeta
        (characterization solnMetrics)
 where
  computeMetrics file = do
    (gs, scenarioObject) <-
      withExceptT ScenarioUploadInstantiationFailure $
        gamestateFromScenarioText $
          fileContent file

    soln <- except $ maybeToEither NoSolutionProvided $ gs ^. winSolution

    solnMetrics <-
      withExceptT ScenarioSolutionEvaluationFailure $
        verifySolution solnTimeout soln gs

    return
      ( AssociatedSolutionCharacterization (fileHash $ fileMetadata file) solnMetrics
      , ScenarioUploadResponsePayload gameVersion $
          scenarioObject ^. scenarioMetadata . scenarioName
      )

validateSubmittedSolution ::
  CommonValidationArgs IO SolutionUploadResponsePayload ->
  -- | Scenario lookup function
  (Sha1 -> IO (Maybe LBS.ByteString)) ->
  IO (Either SolutionSubmissionFailure SolutionFileCharacterization)
validateSubmittedSolution (CommonValidationArgs solnTimeout persistenceArgs) scenarioLookupFunc =
  runExceptT $ do
    userSuppliedScenarioSha1 <-
      withExceptT MissingScenarioParameter
        . except
        . fmap (Sha1 . T.unpack)
        $ lookupInput "scenario" multipartData

    (fileMeta, solnMetrics) <-
      withFileCache
        persistenceArgs
        SolutionUploadFailure
        (computeMetrics userSuppliedScenarioSha1)

    let retrievedScenarioHash = forScenario solnMetrics

    -- We validate that the uploaded solution, if retrieved from the
    -- cache, actually is for the scenario with the hash they
    -- supplied in the upload metadata.
    -- If someone re-uploads a solution file that already happens to be
    -- stored in the database, but specifies a different scenario hash,
    -- we should alert about this mistake with an error.
    unless (userSuppliedScenarioSha1 == retrievedScenarioHash)
      . except
      . Left
      $ CachedSolutionScenarioMismatch userSuppliedScenarioSha1 retrievedScenarioHash

    pure $ SolutionFileCharacterization (fileHash fileMeta) $ characterization solnMetrics
 where
  PersistenceArgs _ multipartData _ = persistenceArgs

  computeMetrics scenarioSha1 file = do
    solText <-
      withExceptT SolutionUnicodeError
        . except
        . decodeUtf8'
        . LBS.toStrict
        $ fileContent file
    res <- liftIO . runError @SystemFailure $ requireNonEmptyTerm =<< processSource Nothing solText Nothing
    soln <- withExceptT (SolutionParseError . prettyText) . except $ res
    gs <- withExceptT ScenarioRetrievalFailure $ do
      scenarioContent <-
        withExceptT DatabaseRetrievalFailure $
          ExceptT
            ( maybeToEither scenarioSha1
                <$> scenarioLookupFunc scenarioSha1
            )

      fmap fst $
        withExceptT RetrievedInstantiationFailure $
          gamestateFromScenarioText scenarioContent

    solnMetrics <-
      withExceptT SubmittedSolutionEvaluationFailure $
        verifySolution solnTimeout soln gs

    return
      ( AssociatedSolutionCharacterization scenarioSha1 solnMetrics
      , SolutionUploadResponsePayload scenarioSha1
      )

-- * Utils

initScenarioObjectWithEnv ::
  LBS.ByteString ->
  ExceptT ScenarioInstantiationFailure IO (Scenario Elaborated)
initScenarioObjectWithEnv content = do
  scenarioInputs <-
    withExceptT (ScenarioEnvironmentFailure . ContextInitializationFailure)
      . ExceptT
      . runThrow
      $ evalAccum (mempty :: Seq SystemFailure) initScenarioInputs

  initScenarioObject scenarioInputs content

initScenarioObject ::
  ScenarioInputs ->
  LBS.ByteString ->
  ExceptT ScenarioInstantiationFailure IO (Scenario Elaborated)
initScenarioObject scenarioInputs content = do
  rawYaml <- withExceptT YamlDecodeError . except . decodeEither' $ LBS.toStrict content
  rawScenario <-
    withExceptT ScenarioParseFailure $
      except $
        parseEither (parseJSONE' Nothing scenarioInputs) rawYaml
  res <- runError @SystemFailure $ process (rawScenario :: Scenario Raw)
  withExceptT (ScenarioParseFailure . prettyString) (except res)

gamestateFromScenarioText ::
  LBS.ByteString ->
  ExceptT ScenarioInstantiationFailure IO (GameState, Scenario Elaborated)
gamestateFromScenarioText content = do
  rs <-
    withExceptT (ScenarioEnvironmentFailure . ContextInitializationFailure)
      . ExceptT
      . runThrow
      . evalAccum (mempty :: Seq SystemFailure)
      . initRuntimeState
      $ RuntimeOptions
        { startPaused = False
        , pauseOnObjectiveCompletion = False
        , loadTestScenarios = False
        }

  let scenarioInputs = gsiScenarioInputs . initState $ rs ^. stdGameConfigInputs
  scenarioObject <- initScenarioObject scenarioInputs content
  gs <- liftIO $ scenarioToGameState (ScenarioWith scenarioObject Nothing) emptyLaunchParams Nothing Nothing rs
  return (gs, scenarioObject)

verifySolution ::
  SolutionTimeout ->
  SyntaxWithImports Elaborated ->
  GameState ->
  ExceptT SolutionEvaluationFailure IO SolutionCharacterization
verifySolution (SolutionTimeout timeoutSeconds) sol gs = do
  (actualTime, eitherTickCount) <-
    ExceptT
      . fmap (maybeToEither (SolutionExecutionTimeout timeoutSeconds))
      . timeout timeoutSeconds
      . duration
      $ evalStateT playUntilWin gs'

  tickCount <- except $ left ErrorsDuringExecution eitherTickCount

  return $
    SolutionCharacterization
      actualTime
      tickCount
      (gs ^. randomness . seed)
      codeMetrics
 where
  codeMetrics = codeMetricsFromSyntax (getSyntax sol)
  gs' = gs & baseRobot . machine %~ continue sol
