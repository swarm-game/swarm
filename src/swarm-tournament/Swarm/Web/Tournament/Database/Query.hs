{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- SQL Queries for Swarm tournaments.
module Swarm.Web.Tournament.Database.Query where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Maybe (listToMaybe)
import Data.Time.Clock
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.State (Sha1 (..))
import Swarm.Game.Tick (TickNumber (..))
import Swarm.Web.Tournament.Type

type ConnectInfo = String

databaseFilename :: ConnectInfo
databaseFilename = "swarm-games.db"

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

-- * Authentication

getUserId :: Connection -> UserAlias -> IO UserId
getUserId conn userAlias = do
  maybeId <-
    listToMaybe . fmap (UserId . fromOnly)
      <$> query conn "SELECT id FROM users WHERE alias = ?;" (Only userAlias)
  maybe insertNew return maybeId
 where
  -- Avoid GHC warning re: partiality of head
  queryHead = \case
    [] -> error "Query result in getUserId should never be empty!"
    hd : _ -> hd
  insertNew =
    fmap (UserId . fromOnly . queryHead)
      $ query
        conn
        "INSERT INTO users (alias) VALUES (?) RETURNING id;"
      $ Only userAlias

-- * Retrieval

lookupScenarioContent :: Sha1 -> ReaderT ConnectInfo IO (Maybe LBS.ByteString)
lookupScenarioContent sha1 = do
  connInfo <- ask
  liftIO . fmap (fmap fromOnly . listToMaybe) . withConnection connInfo $ \conn ->
    query conn "SELECT content FROM scenarios WHERE content_sha1 = ?;" (Only sha1)

lookupSolutionSubmission :: Sha1 -> ReaderT ConnectInfo IO (Maybe AssociatedSolutionSolutionCharacterization)
lookupSolutionSubmission contentSha1 = do
  connInfo <- ask
  liftIO $ withConnection connInfo $ \conn -> runMaybeT $ do
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
  solnChar <- liftIO . fmap listToMaybe . withConnection connInfo $ \conn ->
    query conn "SELECT wall_time_seconds, ticks, seed, char_count, ast_size FROM evaluated_solution WHERE builtin AND scenario = ? LIMIT 1;" (Only scenarioSha1)
  return $ AssociatedSolutionSolutionCharacterization scenarioSha1 <$> solnChar

listGames :: ReaderT ConnectInfo IO [TournamentGame]
listGames = do
  connInfo <- ask
  liftIO $ withConnection connInfo $ \conn ->
    query_ conn "SELECT original_filename, scenario_uploader, scenario, submission_count, swarm_git_sha1 FROM submissions;"

-- * Insertion

insertScenario ::
  CharacterizationResponse ScenarioUploadResponsePayload ->
  ReaderT ConnectInfo IO Sha1
insertScenario s = do
  connInfo <- ask
  h <- liftIO $ withConnection connInfo $ \conn -> do
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
  liftIO $ withConnection connInfo $ \conn -> do
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
