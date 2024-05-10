{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# HLINT ignore "Functor law" #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- SQL Queries for Swarm tournaments.
module Swarm.Web.Tournament.Database.Query where

import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Maybe (listToMaybe)
import Data.String.Utils (strip)
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.State (Sha1 (..))
import Swarm.Game.Tick (TickNumber (..))
import Swarm.Web.Tournament.Type
import System.Exit (ExitCode (..))
import System.Process

-- | Used for local development only
envarPostgresPasswordKey :: String
envarPostgresPasswordKey = "LOCAL_PGPASS"

newtype UserId = UserId Int

instance ToField UserId where
  toField (UserId x) = toField x

data PersistenceLayer = PersistenceLayer
  { lookupScenarioFileContent :: Sha1 -> IO (Maybe LBS.ByteString)
  -- ^ Dump scenario file
  , scenarioStorage :: ScenarioPersistence ScenarioUploadResponsePayload
  , solutionStorage :: ScenarioPersistence SolutionUploadResponsePayload
  }

data ScenarioPersistence b = ScenarioPersistence
  { lookupCache :: Sha1 -> IO (Maybe AssociatedSolutionSolutionCharacterization)
  -- ^ Looks up by key
  , storeCache :: CharacterizationResponse b -> IO Sha1
  -- ^ Stores and returns key
  }

data UserAttributedUpload = UserAttributedUpload
  { uploader :: UserAlias
  , fileUpload :: FileUpload
  }

data CharacterizationResponse a = CharacterizationResponse
  { upload :: UserAttributedUpload
  , associatedCharacterization :: AssociatedSolutionSolutionCharacterization
  , payload :: a
  }

newtype ScenarioUploadResponsePayload = ScenarioUploadResponsePayload
  { swarmGameVersion :: Sha1
  }

newtype SolutionUploadResponsePayload = SolutionUploadResponsePayload
  { scenariohash :: Sha1
  }

instance FromRow AssociatedSolutionSolutionCharacterization where
  fromRow =
    AssociatedSolutionSolutionCharacterization
      <$> (Sha1 <$> field)
      <*> fromRow

instance FromRow SolutionCharacterization where
  fromRow =
    SolutionCharacterization
      <$> field
      <*> (TickNumber <$> field)
      <*> field
      <*> (ScenarioCodeMetrics <$> field <*> field)

instance FromRow TournamentGame where
  fromRow =
    TournamentGame
      <$> field
      <*> field
      <*> (Sha1 <$> field)
      <*> field
      <*> (Sha1 <$> field)

data TokenWithExpiration = TokenWithExpiration
  { expirationTime :: UTCTime
  , loginToken :: Password
  }

type TokenRef = IORef (Maybe TokenWithExpiration)

newtype Username = Username String
newtype Password = Password String

data DbConnType
  = -- | application running directly on host connects to database running on same host
    LocalDBOverSocket Username
  | -- | application running inside docker connects to database running on the docker's host
    LocalDBFromDockerOverNetwork Password
  | -- | application deployed to EC2 inside Docker, accessing RDS database
    RemoteDB TokenRef

-- | Tokens expire after 15 minutes.
-- We shall refresh after 10 minutes.
tokenRefreshInterval :: NominalDiffTime
tokenRefreshInterval = 10 * 60

genNewToken :: ConnectInfo -> IO (Either String String)
genNewToken ci = do
  (exitCode, stdoutString, stderrString) <-
    readProcessWithExitCode
      "aws"
      [ "rds"
      , "generate-db-auth-token"
      , "--hostname"
      , connectHost ci
      , "--port"
      , show $ connectPort ci
      , "--region"
      , region
      , "--username"
      , connectUser ci
      ]
      ""
  return $ case exitCode of
    ExitSuccess -> Right $ strip stdoutString
    ExitFailure _ -> Left stderrString
 where
  region = "us-east-1"

getAwsCredentials :: TokenRef -> ConnectInfo -> IO ConnectInfo
getAwsCredentials tokRef ci = do
  currTime <- getCurrentTime
  maybePreviousTok <- readIORef tokRef
  let maybeStillValidTok = case maybePreviousTok of
        Nothing -> Nothing
        Just (TokenWithExpiration exprTime tok) ->
          guard (currTime < exprTime) >> Just tok

  case maybeStillValidTok of
    Just (Password tok) ->
      return $
        ci
          { connectPassword = tok
          }
    Nothing -> do
      eitherNewTok <- genNewToken ci
      case eitherNewTok of
        Right newTok -> do
          let nextExpirationTime = addUTCTime tokenRefreshInterval currTime
          atomicWriteIORef tokRef
            . Just
            . TokenWithExpiration nextExpirationTime
            $ Password newTok
          return $
            ci
              { connectPassword = newTok
              }
        -- NOTE: This is not exactly valid behavior:
        Left _errMsg -> return ci

mkConnectInfo :: DbConnType -> IO ConnectInfo
mkConnectInfo connType = do
  let swarmDbConnect =
        defaultConnectInfo
          { connectDatabase = "swarm"
          }

  case connType of
    LocalDBFromDockerOverNetwork (Password dbPasswd) ->
      return $
        swarmDbConnect
          { connectHost = "host.docker.internal"
          , connectUser = "swarm-app"
          , connectPassword = dbPasswd
          }
    LocalDBOverSocket (Username username) ->
      return
        swarmDbConnect
          { connectHost = "/var/run/postgresql"
          , connectUser = username
          }
    RemoteDB tokRef -> getAwsCredentials tokRef rdsConnectionInfo
 where
  rdsConnectionInfo =
    defaultConnectInfo
      { connectHost = "swarm-tournaments.cv6iymakujnb.us-east-1.rds.amazonaws.com"
      , connectUser = "swarm-app"
      , connectDatabase = "swarm"
      }

-- * Authentication

getUserId :: Connection -> UserAlias -> IO UserId
getUserId conn userAlias = do
  maybeId <-
    listToMaybe . fmap (UserId . fromOnly)
      <$> query conn "SELECT id FROM users WHERE alias = ?;" (Only userAlias)
  maybe insertNew return maybeId
 where
  insertNew =
    fmap (UserId . fromOnly . head)
      $ query
        conn
        "INSERT INTO users (alias) VALUES (?) RETURNING id;"
      $ Only userAlias

-- * Retrieval

lookupScenarioContent :: Sha1 -> ReaderT ConnectInfo IO (Maybe LBS.ByteString)
lookupScenarioContent sha1 = do
  connInfo <- ask
  liftIO . fmap (fmap fromOnly . listToMaybe) . withConnect connInfo $ \conn ->
    query conn "SELECT content FROM scenarios WHERE content_sha1 = ?;" (Only sha1)

lookupSolutionSubmission :: Sha1 -> ReaderT ConnectInfo IO (Maybe AssociatedSolutionSolutionCharacterization)
lookupSolutionSubmission contentSha1 = do
  connInfo <- ask
  liftIO $ withConnect connInfo $ \conn -> runMaybeT $ do
    evaluationId :: Int <-
      MaybeT $
        fmap fromOnly . listToMaybe
          <$> query conn "SELECT solution_evaluation FROM solution_submission WHERE content_sha1 = ?;" (Only contentSha1)

    MaybeT $
      listToMaybe
        <$> query conn "SELECT scenario, wall_time_seconds, ticks, seed, char_count, ast_size FROM evaluated_solution WHERE id = ?;" (Only evaluationId)

-- | There should only be one builtin solution for the scenario.
lookupScenarioSolution :: Sha1 -> ReaderT ConnectInfo IO (Maybe AssociatedSolutionSolutionCharacterization)
lookupScenarioSolution scenarioSha1 = do
  connInfo <- ask
  solnChar <- liftIO . fmap listToMaybe . withConnect connInfo $ \conn ->
    query conn "SELECT wall_time_seconds, ticks, seed, char_count, ast_size FROM evaluated_solution WHERE builtin AND scenario = ? LIMIT 1;" (Only scenarioSha1)
  return $ AssociatedSolutionSolutionCharacterization scenarioSha1 <$> solnChar

listGames :: ReaderT ConnectInfo IO [TournamentGame]
listGames = do
  connInfo <- ask
  liftIO $ withConnect connInfo $ \conn ->
    query_ conn "SELECT original_filename, scenario_uploader, scenario, submission_count, swarm_git_sha1 FROM submissions;"

-- * Insertion

insertScenario ::
  CharacterizationResponse ScenarioUploadResponsePayload ->
  ReaderT ConnectInfo IO Sha1
insertScenario s = do
  connInfo <- ask
  h <- liftIO $ withConnect connInfo $ \conn -> do
    uid <- getUserId conn $ uploader $ upload s
    [Only resultList] <-
      query
        conn
        "INSERT INTO scenarios (content_sha1, content, original_filename, uploader, swarm_git_sha1) VALUES (?, ?, ?, ?, ?) RETURNING content_sha1;"
        ( scenarioSha
        , fileContent $ fileUpload $ upload s
        , filename . fileMetadata . fileUpload $ upload s
        , uid
        , swarmGameVersion $ payload s
        )
    _ <- insertSolution conn True scenarioSha $ characterization $ associatedCharacterization s

    return resultList
  return $ Sha1 h
 where
  scenarioSha = fileHash . fileMetadata . fileUpload $ upload s

insertSolutionSubmission ::
  CharacterizationResponse SolutionUploadResponsePayload ->
  ReaderT ConnectInfo IO Sha1
insertSolutionSubmission (CharacterizationResponse solutionUpload s (SolutionUploadResponsePayload scenarioSha)) = do
  connInfo <- ask
  liftIO $ withConnect connInfo $ \conn -> do
    uid <- getUserId conn $ uploader solutionUpload

    solutionEvalId <- insertSolution conn False scenarioSha $ characterization s

    [Only echoedSha1] <-
      query
        conn
        "INSERT INTO solution_submission (uploader, content_sha1, solution_evaluation) VALUES (?, ?, ?) RETURNING content_sha1;"
        (uid, fileHash $ fileMetadata $ fileUpload solutionUpload, solutionEvalId)
    return $ Sha1 echoedSha1

insertSolution ::
  Connection ->
  Bool ->
  Sha1 ->
  SolutionCharacterization ->
  IO Int
insertSolution conn isBuiltin scenarioSha s = do
  [Only evaluationId] <-
    query
      conn
      "INSERT INTO evaluated_solution (scenario, builtin, wall_time_seconds, ticks, seed, char_count, ast_size) VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING id;"
      insertion_items
  return evaluationId
 where
  insertion_items =
    ( scenarioSha
    , isBuiltin
    , solutionWallTime s
    , getTickNumber $ solutionTicks s
    , scenarioSeed s
    , sourceTextLength $ solutionCodeMetrics s
    , astSize $ solutionCodeMetrics s
    )
