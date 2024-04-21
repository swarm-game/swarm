{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Validates an uploaded scenario
module Swarm.Web.Tournament.Validate where

import Control.Arrow (left)
import Control.Carrier.Accum.FixedStrict (evalAccum)
import Control.Carrier.Throw.Either (runThrow)
import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, evalStateT, gets)
import Control.Monad.Trans.Except
import Data.ByteString.Lazy qualified as LBS
import Data.Either.Extra (maybeToEither)
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty qualified as NE
import Data.Sequence (Seq)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Yaml (decodeEither', parseEither)
import Servant.Multipart
import Swarm.Effect (runTimeIO)
import Swarm.Game.CESK (emptyStore, initMachine)
import Swarm.Game.Failure (SystemFailure)
import Swarm.Game.Robot.Concrete (machine, robotContext, robotLog)
import Swarm.Game.Robot.Context (defReqs)
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Scoring.CodeSize (codeMetricsFromSyntax)
import Swarm.Game.Scenario.Status (emptyLaunchParams)
import Swarm.Game.State
import Swarm.Game.State.Robot (robotMap)
import Swarm.Game.State.Runtime (initGameStateConfig, initScenarioInputs)
import Swarm.Game.State.Substate (
  WinCondition (WinConditions),
  WinStatus (Won),
  initState,
  messageQueue,
  seed,
 )
import Swarm.Game.Step (gameTick)
import Swarm.Game.Tick (TickNumber)
import Swarm.Language.Context qualified as Ctx
import Swarm.Language.Module (Module (..))
import Swarm.Language.Pipeline (ProcessedTerm (..), processTermEither)
import Swarm.Log
import Swarm.Util.Yaml
import Swarm.Web.Tournament.Database.Query
import Swarm.Web.Tournament.Type
import Swarm.Web.Tournament.Validate.FailureMode
import Swarm.Web.Tournament.Validate.Upload
import System.Time.Extra

newtype SolutionTimeout = SolutionTimeout Seconds

data CommonValidationArgs a
  = CommonValidationArgs
      SolutionTimeout
      (PersistenceArgs a)

validateScenarioUpload ::
  CommonValidationArgs ScenarioUploadResponsePayload ->
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
    gs <-
      withExceptT ScenarioUploadInstantiationFailure $
        gamestateFromScenarioText $
          fileContent file

    soln <- except $ maybeToEither NoSolutionProvided $ gs ^. winSolution

    solnMetrics <-
      withExceptT ScenarioSolutionEvaluationFailure $
        verifySolution solnTimeout soln gs

    return
      ( AssociatedSolutionSolutionCharacterization (fileHash $ fileMetadata file) solnMetrics
      , ScenarioUploadResponsePayload gameVersion
      )

validateSubmittedSolution ::
  CommonValidationArgs SolutionUploadResponsePayload ->
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
    soln <- withExceptT SolutionParseError . except $ processTermEither solText

    gs <- withExceptT ScenarioRetrievalFailure $ do
      scenarioContent <-
        withExceptT DatabaseRetrievalFailure $
          ExceptT
            ( maybeToEither scenarioSha1
                <$> scenarioLookupFunc scenarioSha1
            )

      withExceptT RetrievedInstantiationFailure $
        gamestateFromScenarioText scenarioContent

    solnMetrics <-
      withExceptT SubmittedSolutionEvaluationFailure $
        verifySolution solnTimeout soln gs

    return
      ( AssociatedSolutionSolutionCharacterization scenarioSha1 solnMetrics
      , SolutionUploadResponsePayload scenarioSha1
      )

-- * Utils

initScenarioObjectWithEnv ::
  LBS.ByteString ->
  ExceptT ScenarioInstantiationFailure IO Scenario
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
  ExceptT ScenarioInstantiationFailure IO Scenario
initScenarioObject scenarioInputs content = do
  rawYaml <- withExceptT YamlDecodeError . except . decodeEither' $ LBS.toStrict content
  withExceptT ScenarioParseFailure $
    except $
      parseEither (parseJSONE' scenarioInputs) rawYaml

gamestateFromScenarioText ::
  LBS.ByteString ->
  ExceptT ScenarioInstantiationFailure IO GameState
gamestateFromScenarioText content = do
  gsc <-
    withExceptT (ScenarioEnvironmentFailure . ContextInitializationFailure)
      . ExceptT
      . runThrow
      $ evalAccum (mempty :: Seq SystemFailure) initGameStateConfig

  let scenarioInputs = gsiScenarioInputs $ initState gsc
  scenarioObject <- initScenarioObject scenarioInputs content
  liftIO $ scenarioToGameState scenarioObject emptyLaunchParams gsc

-- |
-- NOTE: Borrowed from @testSolution'@ in @tests/integration/Main.hs@
verifySolution ::
  SolutionTimeout ->
  ProcessedTerm ->
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
  ProcessedTerm (Module s _) _ reqCtx = sol
  codeMetrics = codeMetricsFromSyntax s
  gs' =
    gs
      -- See #827 for an explanation of why it's important to add to
      -- the robotContext defReqs here (and also why this will,
      -- hopefully, eventually, go away).
      & baseRobot . robotContext . defReqs <>~ reqCtx
      & baseRobot . machine .~ initMachine sol Ctx.empty emptyStore

-- ** Utils shared with integration tests

playUntilWin :: StateT GameState IO (Either (NE.NonEmpty T.Text) TickNumber)
playUntilWin = do
  w <- use winCondition
  b <- gets badErrorsInLogs
  case NE.nonEmpty b of
    Just badErrs -> return $ Left badErrs
    Nothing -> case w of
      WinConditions (Won _ ts) _ -> return $ Right ts
      _ -> runTimeIO gameTick >> playUntilWin

badErrorsInLogs :: GameState -> [T.Text]
badErrorsInLogs g =
  concatMap
    (\r -> filter isBad (seqToTexts $ r ^. robotLog))
    (g ^. robotInfo . robotMap)
    <> filter isBad (seqToTexts $ g ^. messageInfo . messageQueue)
 where
  isBad m = "Fatal error:" `T.isInfixOf` m || "swarm/issues" `T.isInfixOf` m

seqToTexts :: Seq LogEntry -> [T.Text]
seqToTexts = map (view leText) . toList
