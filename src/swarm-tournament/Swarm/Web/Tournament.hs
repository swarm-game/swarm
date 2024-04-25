{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A web service for serving Swarm tournaments.
module Swarm.Web.Tournament (
  defaultPort,
  AppData (..),

  -- ** Development
  webMain,
  app,
) where

import Commonmark qualified as Mark (commonmark, renderHtml)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Either.Extra (maybeToEither)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8', encodeUtf8)
import Data.Yaml (decodeEither', defaultEncodeOptions, encodeWith)
import Network.HTTP.Types (ok200)
import Network.Wai (responseLBS)
import Network.Wai.Application.Static (defaultFileServerSettings, ssIndices)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Parse (
  defaultParseRequestBodyOptions,
  setMaxRequestFileSize,
  setMaxRequestKeyLength,
  setMaxRequestNumFiles,
 )
import Servant
import Servant.Docs qualified as SD
import Servant.Docs.Internal qualified as SD (renderCurlBasePath)
import Servant.Multipart
import Swarm.Game.Scenario (ScenarioMetadata (ScenarioMetadata), scenarioMetadata)
import Swarm.Game.Scenario.Scoring.CodeSize (ScenarioCodeMetrics (..))
import Swarm.Game.State (Sha1 (..))
import Swarm.Game.Tick (TickNumber (..))
import Swarm.Web.Tournament.Database.Query
import Swarm.Web.Tournament.Type
import Swarm.Web.Tournament.Validate
import Swarm.Web.Tournament.Validate.FailureMode
import Swarm.Web.Tournament.Validate.Upload
import WaiAppStatic.Types (unsafeToPiece)

placeholderAlias :: UserAlias
placeholderAlias = UserAlias "Karl"

defaultPort :: Warp.Port
defaultPort = 5500

-- | NOTE: The default Servant server timeout is 30 sec;
-- see https://hackage.haskell.org/package/http-client-0.7.17/docs/Network-HTTP-Client-Internal.html#t:ResponseTimeout
defaultSolutionTimeout :: SolutionTimeout
defaultSolutionTimeout = SolutionTimeout 15

data AppData = AppData
  { swarmGameGitVersion :: Sha1
  , persistence :: PersistenceLayer
  , dbConnType :: DbConnType
  }

type TournamentAPI =
  "upload" :> "scenario" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] ScenarioCharacterization
    :<|> "upload" :> "solution" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] SolutionFileCharacterization
    :<|> "scenario" :> Capture "sha1" Sha1 :> "metadata" :> Get '[JSON] ScenarioMetadata
    :<|> "scenario" :> Capture "sha1" Sha1 :> "fetch" :> Get '[PlainText] TL.Text
    :<|> "games" :> Get '[JSON] [TournamentGame]

swarmApi :: Proxy TournamentAPI
swarmApi = Proxy

type ToplevelAPI =
  TournamentAPI
    :<|> "api" :> Raw
    :<|> Raw

api :: Proxy ToplevelAPI
api = Proxy

swarmApiHtml :: ByteString
swarmApiHtml =
  encodeUtf8
    . either (error . show) (Mark.renderHtml @())
    . Mark.commonmark ""
    $ T.pack swarmApiMarkdown

swarmApiMarkdown :: String
swarmApiMarkdown =
  SD.markdownWith
    ( SD.defRenderingOptions
        & SD.requestExamples .~ SD.FirstContentType
        & SD.responseExamples .~ SD.FirstContentType
        & SD.renderCurlBasePath ?~ "http://localhost:" <> show defaultPort
    )
    $ SD.docsWithIntros [intro] swarmApi
 where
  intro =
    SD.DocIntro
      "Swarm tournament hosting API"
      [ "All of the valid endpoints are documented below."
      ]

toServantError :: Describable a => a -> ServerError
toServantError x = err500 {errBody = encodeUtf8 $ TL.fromStrict $ describeText x}

-- * Handlers

mkApp :: AppData -> Servant.Server TournamentAPI
mkApp appData =
  uploadScenario appData
    :<|> uploadSolution appData
    :<|> getScenarioMetadata appData
    :<|> downloadRedactedScenario appData
    :<|> listScenarios appData

uploadScenario :: AppData -> MultipartData Mem -> Handler ScenarioCharacterization
uploadScenario (AppData gameVersion persistenceLayer _) multipartData =
  Handler . withExceptT toServantError . ExceptT $
    validateScenarioUpload
      args
      gameVersion
 where
  args =
    CommonValidationArgs
      defaultSolutionTimeout
      $ PersistenceArgs
        placeholderAlias
        multipartData
        (scenarioStorage persistenceLayer)

uploadSolution :: AppData -> MultipartData Mem -> Handler SolutionFileCharacterization
uploadSolution (AppData _ persistenceLayer _) multipartData =
  Handler . withExceptT toServantError . ExceptT $
    validateSubmittedSolution
      args
      (lookupScenarioFileContent persistenceLayer)
 where
  args =
    CommonValidationArgs
      defaultSolutionTimeout
      $ PersistenceArgs
        placeholderAlias
        multipartData
        (solutionStorage persistenceLayer)

getScenarioMetadata :: AppData -> Sha1 -> Handler ScenarioMetadata
getScenarioMetadata (AppData _ persistenceLayer _) scenarioSha1 =
  Handler . withExceptT toServantError $ do
    doc <-
      ExceptT $
        maybeToEither (DatabaseRetrievalFailure scenarioSha1)
          <$> lookupScenarioFileContent persistenceLayer scenarioSha1

    s <- withExceptT RetrievedInstantiationFailure $ initScenarioObjectWithEnv doc
    return $ view scenarioMetadata s

downloadRedactedScenario :: AppData -> Sha1 -> Handler TL.Text
downloadRedactedScenario (AppData _ persistenceLayer _) scenarioSha1 = do
  Handler . withExceptT toServantError $ do
    doc <-
      ExceptT $
        maybeToEither (DatabaseRetrievalFailure scenarioSha1)
          <$> lookupScenarioFileContent persistenceLayer scenarioSha1

    rawYamlDict :: Map Key Value <- withExceptT YamlParseFailure . except . decodeEither' $ LBS.toStrict doc
    let redactedDict = M.delete "solution" rawYamlDict
    withExceptT DecodingFailure . except . decodeUtf8' . LBS.fromStrict $
      encodeWith defaultEncodeOptions redactedDict

-- NOTE: This is currently the only API endpoint that invokes
-- 'mkConnectInfo' directly
listScenarios :: AppData -> Handler [TournamentGame]
listScenarios (AppData _ _ connMode) =
  Handler $ liftIO $ do
    connInfo <- mkConnectInfo connMode
    runReaderT listGames connInfo

-- * Web app declaration

app :: AppData -> Application
app appData = Servant.serveWithContext api context server
 where
  size100kB = 100_000 :: Int64

  multipartOpts :: MultipartOptions Mem
  multipartOpts =
    (defaultMultipartOptions (Proxy :: Proxy Mem))
      { generalOptions =
          setMaxRequestFileSize size100kB
            . setMaxRequestKeyLength 64
            . setMaxRequestNumFiles 1
            $ defaultParseRequestBodyOptions
      }

  context = multipartOpts :. EmptyContext

  server :: Server ToplevelAPI
  server =
    mkApp appData
      :<|> Tagged serveDocs
      :<|> serveDirectoryWith
        (defaultFileServerSettings "tournament/web")
          { ssIndices = [unsafeToPiece "index.html"]
          }
   where
    serveDocs _ resp =
      resp $ responseLBS ok200 [plain] swarmApiHtml
    plain = ("Content-Type", "text/html")

webMain ::
  AppData ->
  Warp.Port ->
  IO ()
webMain appData port = Warp.runSettings settings $ app appData
 where
  settings = Warp.setPort port Warp.defaultSettings

-- * Instances for documentation

instance SD.ToSample T.Text where
  toSamples _ = SD.samples ["foo"]

instance SD.ToSample TL.Text where
  toSamples _ = SD.samples ["foo"]

instance SD.ToSample TournamentGame where
  toSamples _ = SD.samples [TournamentGame "foo" "bar" (Sha1 "abc") 10 (Sha1 "def")]

fakeSolnCharacterization :: SolutionCharacterization
fakeSolnCharacterization =
  SolutionCharacterization
    10
    (TickNumber 100)
    0
    (ScenarioCodeMetrics 10 5)

instance SD.ToSample ScenarioMetadata where
  toSamples _ = SD.samples [ScenarioMetadata 1 "foo" $ Just "bar"]

instance SD.ToSample SolutionFileCharacterization where
  toSamples _ = SD.samples [SolutionFileCharacterization (Sha1 "abcdef") fakeSolnCharacterization]

instance SD.ToSample ScenarioCharacterization where
  toSamples _ = SD.samples [ScenarioCharacterization (FileMetadata "foo.yaml" (Sha1 "abcdef")) fakeSolnCharacterization]

instance ToMultipartSample Mem (MultipartData Mem) where
  toMultipartSamples _proxy =
    [
      ( "sample 1"
      , MultipartData
          [Input "username" "Elvis Presley"]
          [ FileData
              "scenario-file"
              "my-scenario.yaml"
              "application/yaml"
              "tmpservant-multipart000.buf"
          ]
      )
    ]
