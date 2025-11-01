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
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time.Clock
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.State (Sha1 (..))
import Swarm.Game.Tick (TickNumber (..))
import Swarm.Web.Auth
import Swarm.Web.Tournament.Type

type ConnectInfo = String

databaseFilename :: ConnectInfo
databaseFilename = "swarm-games.db"

newtype UserId = UserId Int

instance ToField UserId where
  toField (UserId x) = toField x

data AuthenticationStorage m = AuthenticationStorage
  { usernameFromCookie :: TL.Text -> m (Maybe UserAlias)
  , cookieFromUsername :: UserAlias -> m TL.Text
  }

data PersistenceLayer m = PersistenceLayer
  { scenarioStorage :: ScenarioPersistence m ScenarioUploadResponsePayload
  , solutionStorage :: ScenarioPersistence m SolutionUploadResponsePayload
  , authenticationStorage :: AuthenticationStorage m
  }

data ScenarioPersistence m a = ScenarioPersistence
  { lookupCache :: Sha1 -> m (Maybe AssociatedSolutionCharacterization)
  -- ^ Looks up by key
  , storeCache :: CharacterizationResponse a -> m Sha1
  -- ^ Stores and returns key
  , getContent :: Sha1 -> m (Maybe LBS.ByteString)
  -- ^ Dump file contents
  }

data UserAttributedUpload = UserAttributedUpload
  { uploader :: UserAlias
  , fileUpload :: FileUpload
  }

data CharacterizationResponse a = CharacterizationResponse
  { upload :: UserAttributedUpload
  , associatedCharacterization :: AssociatedSolutionCharacterization
  , payload :: a
  }

data ScenarioUploadResponsePayload = ScenarioUploadResponsePayload
  { swarmGameVersion :: Sha1
  , sTitle :: T.Text
  }

newtype SolutionUploadResponsePayload = SolutionUploadResponsePayload
  { scenariohash :: Sha1
  }

instance FromRow AssociatedSolutionCharacterization where
  fromRow =
    AssociatedSolutionCharacterization
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
      <*> field

instance FromRow TournamentSolution where
  fromRow =
    TournamentSolution
      <$> field
      <*> field
      <*> fromRow

instance FromRow SolutionFileCharacterization where
  fromRow =
    SolutionFileCharacterization
      <$> (Sha1 <$> field)
      <*> fromRow

-- * Authentication

-- | If the username already exists, overwrite the row.
insertCookie ::
  UserAlias ->
  ReaderT Connection IO TL.Text
insertCookie gitHubUsername = do
  conn <- ask
  liftIO $ do
    [Only cookieString] <-
      query
        conn
        "REPLACE INTO users (alias) VALUES (?) RETURNING cookie;"
        (Only gitHubUsername)
    return cookieString

-- | If the username already exists, overwrite the row.
insertGitHubTokens ::
  UserAlias ->
  ReceivedTokens ->
  ReaderT Connection IO ()
insertGitHubTokens gitHubUsername gitHubTokens = do
  conn <- ask
  currentTime <- liftIO getCurrentTime
  liftIO $ do
    execute
      conn
      "REPLACE INTO github_tokens (alias, github_access_token, github_access_token_expires_at, github_refresh_token, github_refresh_token_expires_at) VALUES (?, ?, ?, ?, ?);"
      ( gitHubUsername
      , token $ accessToken gitHubTokens
      , mkExpirationTime currentTime accessToken
      , token $ refreshToken gitHubTokens
      , mkExpirationTime currentTime refreshToken
      )
    return ()
 where
  mkExpirationTime :: UTCTime -> (ReceivedTokens -> Expirable a) -> UTCTime
  mkExpirationTime currTime accessor =
    addUTCTime (fromIntegral $ expirationSeconds $ accessor gitHubTokens) currTime

getUsernameFromCookie ::
  TL.Text ->
  ReaderT Connection IO (Maybe UserAlias)
getUsernameFromCookie cookieText = do
  conn <- ask
  liftIO . fmap (fmap (UserAlias . fromOnly) . listToMaybe) $
    query conn "SELECT alias FROM users WHERE cookie = ?;" (Only cookieText)

-- * Retrieval

lookupScenarioContent :: Sha1 -> ReaderT Connection IO (Maybe LBS.ByteString)
lookupScenarioContent sha1 = do
  conn <- ask
  liftIO . fmap (fmap fromOnly . listToMaybe) $
    query conn "SELECT content FROM scenarios WHERE content_sha1 = ?;" (Only sha1)

lookupSolutionContent :: Sha1 -> ReaderT Connection IO (Maybe LBS.ByteString)
lookupSolutionContent sha1 = do
  conn <- ask
  liftIO . fmap (fmap fromOnly . listToMaybe) $
    query conn "SELECT content FROM solution_submission WHERE content_sha1 = ?;" (Only sha1)

lookupSolutionSubmission :: Sha1 -> ReaderT Connection IO (Maybe AssociatedSolutionCharacterization)
lookupSolutionSubmission contentSha1 = do
  conn <- ask
  liftIO $ runMaybeT $ do
    evaluationId :: Int <-
      MaybeT $
        fmap fromOnly . listToMaybe
          <$> query conn "SELECT solution_evaluation FROM solution_submission WHERE content_sha1 = ?;" (Only contentSha1)

    MaybeT $
      listToMaybe
        <$> query conn "SELECT scenario, wall_time_seconds, ticks, seed, char_count, ast_size FROM evaluated_solution WHERE id = ?;" (Only evaluationId)

-- | There should only be one builtin solution for the scenario.
lookupScenarioSolution :: Sha1 -> ReaderT Connection IO (Maybe AssociatedSolutionCharacterization)
lookupScenarioSolution scenarioSha1 = do
  conn <- ask
  solnChar <-
    liftIO . fmap listToMaybe $
      query conn "SELECT wall_time_seconds, ticks, seed, char_count, ast_size FROM evaluated_solution WHERE builtin AND scenario = ? LIMIT 1;" (Only scenarioSha1)
  return $ AssociatedSolutionCharacterization scenarioSha1 <$> solnChar

listGames :: ReaderT Connection IO [TournamentGame]
listGames = do
  conn <- ask
  liftIO $
    query_ conn "SELECT original_filename, scenario_uploader, scenario, submission_count, swarm_git_sha1, title FROM agg_scenario_submissions;"

listSubmissions :: Sha1 -> ReaderT Connection IO GameWithSolutions
listSubmissions scenarioSha1 = do
  conn <- ask
  liftIO $ do
    [game] <- query conn "SELECT original_filename, scenario_uploader, scenario, submission_count, swarm_git_sha1, title FROM agg_scenario_submissions WHERE scenario = ?;" (Only scenarioSha1)
    solns <- query conn "SELECT uploaded_at, solution_submitter, solution_sha1, wall_time_seconds, ticks, seed, char_count, ast_size FROM all_solution_submissions WHERE scenario = ?;" (Only scenarioSha1)
    return $ GameWithSolutions game solns

-- * Insertion

insertScenario ::
  CharacterizationResponse ScenarioUploadResponsePayload ->
  ReaderT Connection IO Sha1
insertScenario s = do
  conn <- ask
  h <- liftIO $ do
    [Only resultList] <-
      query
        conn
        "INSERT INTO scenarios (content_sha1, content, original_filename, title, uploader, swarm_git_sha1) VALUES (?, ?, ?, ?, ?, ?) RETURNING content_sha1;"
        ( scenarioSha
        , fileContent $ fileUpload $ upload s
        , filename . fileMetadata . fileUpload $ upload s
        , sTitle $ payload s
        , uploader $ upload s
        , swarmGameVersion $ payload s
        )
    _ <- insertSolution conn True scenarioSha $ characterization $ associatedCharacterization s

    return resultList
  return $ Sha1 h
 where
  scenarioSha = fileHash . fileMetadata . fileUpload $ upload s

insertSolutionSubmission ::
  CharacterizationResponse SolutionUploadResponsePayload ->
  ReaderT Connection IO Sha1
insertSolutionSubmission (CharacterizationResponse solutionUpload s (SolutionUploadResponsePayload scenarioSha)) = do
  conn <- ask
  liftIO $ do
    solutionEvalId <- insertSolution conn False scenarioSha $ characterization s
    [Only echoedSha1] <-
      query
        conn
        "INSERT INTO solution_submission (uploader, content_sha1, solution_evaluation, content) VALUES (?, ?, ?, ?) RETURNING content_sha1;"
        ( uploader solutionUpload
        , fileHash $ fileMetadata $ fileUpload solutionUpload
        , solutionEvalId
        , fileContent $ fileUpload solutionUpload
        )
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
