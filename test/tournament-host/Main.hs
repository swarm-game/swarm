{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Tournament hosting
module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.List.NonEmpty qualified as NE
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEM
import Data.String (fromString)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types (ok200)
import Network.Wai.Handler.Warp (testWithApplication)
import Swarm.Game.State (Sha1 (..))
import Swarm.Web.Tournament qualified as Tournament
import Swarm.Web.Tournament.Database.Query
import Swarm.Web.Tournament.Type (UserAlias (..))
import System.Directory
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

main :: IO ()
main = do
  currDir <- getCurrentDirectory

  dirContents <- listDirectory currDir
  putStrLn $ unwords ["DEBUG:", "CWD is:", currDir]
  putStrLn $ unwords ["DEBUG:", "Contents of CWD:", show dirContents]

  scenariosMap <- buildScenariosMap $ pure "data/scenarios/Challenges/arbitrage.yaml"
  let appData = mkAppData scenariosMap
  defaultMain $
    testGroup
      "Test database-agnostic server interactions"
      [ testCase "Test scenario upload" $ testScenarioUpload scenariosMap appData
      , testCase "Test solution upload" $ testSolutionUpload scenariosMap appData
      ]
 where
  noPersistence =
    ScenarioPersistence
      { lookupCache = const $ return Nothing
      , storeCache = const $ return $ Sha1 "bogus"
      , getContent = const $ return Nothing
      }

  mkPersistenceLayer scenariosMap =
    PersistenceLayer
      { scenarioStorage =
          noPersistence
            { getContent = return . fmap content . (`NEM.lookup` scenariosMap)
            }
      , solutionStorage = noPersistence
      , authenticationStorage =
          AuthenticationStorage
            { usernameFromCookie = const $ return $ Just fakeUser
            , cookieFromUsername = const $ return "fake-cookie-value"
            }
      }

  fakeUser = UserAlias "test-user"

  mkAppData scenariosMap =
    Tournament.AppData
      { Tournament.swarmGameGitVersion = Sha1 "abcdef"
      , Tournament.gitHubCredentials = Tournament.GitHubCredentials "" ""
      , Tournament.persistence = mkPersistenceLayer scenariosMap
      , Tournament.developmentMode = Tournament.LocalDevelopment fakeUser
      }

type LocalFileLookup = NEMap Sha1 FilePathAndContent

data FilePathAndContent = FilePathAndContent
  { filePath :: FilePath
  , content :: LBS.ByteString
  }

buildScenariosMap :: NE.NonEmpty FilePath -> IO LocalFileLookup
buildScenariosMap pathList =
  NEM.fromList <$> traverse getWithHash pathList
 where
  getWithHash fp = do
    fileContent <- LBS.readFile fp
    let h = Sha1 $ showDigest $ sha1 fileContent
    return (h, FilePathAndContent fp fileContent)

testScenarioUpload :: LocalFileLookup -> Tournament.AppData -> Assertion
testScenarioUpload fileLookup appData =
  mapM_ f testScenarioPaths
 where
  f x = uploadForm appData "/api/private/upload/scenario" [partFileSource "file" x]
  testScenarioPaths = map filePath $ NE.toList $ NEM.elems fileLookup

testSolutionUpload :: LocalFileLookup -> Tournament.AppData -> Assertion
testSolutionUpload fileLookup appData =
  uploadForm appData "/api/private/upload/solution" form
 where
  solutionFilePath = "data/scenarios/Challenges/_arbitrage/solution.sw"
  Sha1 scenarioSha1 = NE.head $ NEM.keys fileLookup
  form =
    [ partBS "scenario" $ fromString scenarioSha1
    , partFileSource "file" solutionFilePath
    ]

-- * Utils

uploadForm :: Tournament.AppData -> String -> [PartM IO] -> Assertion
uploadForm appData urlPath form =
  testWithApplication (pure tournamentApp) $ \p -> do
    manager <- newManager defaultManagerSettings

    let baseUrl = "http://localhost:" ++ show p
    reqLogin <- parseRequest $ baseUrl ++ "/api/private/login/local"
    respLogin <- httpLbs reqLogin manager

    let apiUrl = baseUrl ++ urlPath
    req <- parseRequest apiUrl
    resp <-
      flip httpLbs manager
        =<< formDataBody form (req {cookieJar = Just $ responseCookieJar respLogin})

    let assertionMsg =
          unwords
            [ "Server response from"
            , apiUrl
            , "should be 200;"
            , "'respLogin' was:"
            , show respLogin
            , "and 'resp' was:"
            , show resp
            ]
    assertEqual assertionMsg ok200 $ responseStatus resp
 where
  tournamentApp = Tournament.app appData
