-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Control.Monad.Trans.Reader (runReaderT)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import Swarm.Game.State (Sha1 (..))
import Swarm.Web.Tournament
import Swarm.Web.Tournament.Database.Query

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

main :: IO ()
main = do
  opts <- execParser cliInfo
  webMain
    (AppData (gameGitVersion opts) persistenceFunctions)
    (fromMaybe defaultPort $ userWebPort opts)
 where
  persistenceFunctions =
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
    withConnInfo f x =
      runReaderT (f x) databaseFilename
