-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Control.Monad.Trans.Reader (runReaderT)
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import Swarm.Game.State (Sha1 (..))
import Swarm.Web.Tournament
import Swarm.Web.Tournament.Database.Query
import System.Environment (lookupEnv)
import System.Posix.User (getEffectiveUserName)

data AppOpts = AppOpts
  { userWebPort :: Maybe Port
  -- ^ Explicit port on which to run the web API
  , gameGitVersion :: Sha1
  , isLocalSocketConnection :: Bool
  }

webPort :: Parser (Maybe Int)
webPort =
  optional $
    option
      auto
      ( long "port"
          <> metavar "PORT"
          <> help ("Set the web service port (or disable it with 0). Default to " <> show defaultPort <> ".")
      )

gameVersion :: Parser Sha1
gameVersion =
  Sha1
    <$> option
      str
      ( long "version"
          <> metavar "VERSION"
          <> help "Set the git version of the game"
      )

parseNativeDev :: Parser Bool
parseNativeDev =
  switch
    (long "native-dev" <> help "Running locally outside of a Docker container for development")

cliParser :: Parser AppOpts
cliParser = AppOpts <$> webPort <*> gameVersion <*> parseNativeDev

cliInfo :: ParserInfo AppOpts
cliInfo =
  info
    (cliParser <**> helper)
    ( header "Swarm tournament"
        <> progDesc "Hosts a tournament server."
        <> fullDesc
    )

deduceConnType :: Bool -> IO DbConnType
deduceConnType isLocalSocketConn =
  if isLocalSocketConn
    then LocalDBOverSocket . Username <$> getEffectiveUserName
    else do
      maybeDbPassword <- lookupEnv envarPostgresPasswordKey
      case maybeDbPassword of
        Just dbPasswordEnvar -> return $ LocalDBFromDockerOverNetwork $ Password dbPasswordEnvar
        Nothing -> RemoteDB <$> newIORef Nothing

main :: IO ()
main = do
  opts <- execParser cliInfo
  connType <- deduceConnType $ isLocalSocketConnection opts
  webMain
    (AppData (gameGitVersion opts) (persistenceFunctions connType) connType)
    (fromMaybe defaultPort $ userWebPort opts)
 where
  persistenceFunctions connMode =
    PersistenceLayer
      { lookupScenarioFileContent = withConnInfo lookupScenarioContent
      , scenarioStorage =
          ScenarioPersistence
            { lookupCache = withConnInfo lookupScenarioSolution
            , storeCache = withConnInfo insertScenario
            }
      , solutionStorage =
          ScenarioPersistence
            { lookupCache = withConnInfo lookupSolutionSubmission
            , storeCache = withConnInfo insertSolutionSubmission
            }
      }
   where
    withConnInfo f x = do
      -- This gets deferred and re-executed upon each invocation
      -- of a DB interaction function.
      -- We need this behavior because the password fetched via API
      -- expires after 15 min.
      connInfo <- mkConnectInfo connMode
      runReaderT (f x) connInfo
