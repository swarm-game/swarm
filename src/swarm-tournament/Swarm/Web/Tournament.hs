{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A web service for serving Swarm tournaments.
module Swarm.Web.Tournament (
  defaultPort,
  AppData (..),
  GitHubCredentials (..),
  DeploymentEnvironment (..),

  -- ** Development
  webMain,
  app,
) where

import Commonmark qualified as Mark (commonmark, renderHtml)
import Control.Lens hiding (Context)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either.Extra (maybeToEither)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.Yaml (decodeEither', defaultEncodeOptions, encodeWith)
import Database.SQLite.Simple (withConnection)
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hCookie, ok200, renderSimpleQuery)
import Network.Wai (Request, requestHeaders, responseLBS)
import Network.Wai.Application.Static (defaultFileServerSettings, ssIndices)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Parse (
  defaultParseRequestBodyOptions,
  setMaxRequestFileSize,
  setMaxRequestKeyLength,
  setMaxRequestNumFiles,
 )
import Servant
import Servant.Multipart
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Swarm.Game.Scenario (ScenarioMetadata, scenarioMetadata)
import Swarm.Game.State (Sha1 (..))
import Swarm.Web.Auth
import Swarm.Web.Tournament.Database.Query
import Swarm.Web.Tournament.Type
import Swarm.Web.Tournament.Validate
import Swarm.Web.Tournament.Validate.FailureMode
import Swarm.Web.Tournament.Validate.Upload
import WaiAppStatic.Types (unsafeToPiece)
import Web.Cookie

defaultPort :: Warp.Port
defaultPort = 5500

-- | NOTE: The default Servant server timeout is 30 sec;
-- see https://hackage.haskell.org/package/http-client-0.7.17/docs/Network-HTTP-Client-Internal.html#t:ResponseTimeout
defaultSolutionTimeout :: SolutionTimeout
defaultSolutionTimeout = SolutionTimeout 20

data DeploymentEnvironment
  = LocalDevelopment UserAlias
  | ProdDeployment

data AppData = AppData
  { swarmGameGitVersion :: Sha1
  , gitHubCredentials :: GitHubCredentials
  , persistence :: PersistenceLayer IO
  , developmentMode :: DeploymentEnvironment
  }

type LoginType = Headers '[Header "Location" TL.Text, Header "Set-Cookie" SetCookie] NoContent
type LoginHandler = Handler LoginType

type TournamentAPI =
  "api" :> "private" :> "upload" :> "scenario" :> Header "Referer" TL.Text :> AuthProtect "cookie-auth" :> MultipartForm Mem (MultipartData Mem) :> Verb 'POST 303 '[JSON] (Headers '[Header "Location" TL.Text] ScenarioCharacterization)
    :<|> "api" :> "private" :> "upload" :> "solution" :> Header "Referer" TL.Text :> AuthProtect "cookie-auth" :> MultipartForm Mem (MultipartData Mem) :> Verb 'POST 303 '[JSON] (Headers '[Header "Location" TL.Text] SolutionFileCharacterization)
    :<|> "scenario" :> Capture "sha1" Sha1 :> "metadata" :> Get '[JSON] ScenarioMetadata
    :<|> "scenario" :> Capture "sha1" Sha1 :> "fetch" :> Get '[PlainText] TL.Text
    :<|> "solution" :> Capture "sha1" Sha1 :> "fetch" :> Get '[PlainText] TL.Text
    :<|> "list" :> "games" :> Get '[JSON] [TournamentGame]
    :<|> "list" :> "game" :> Capture "sha1" Sha1 :> Get '[JSON] GameWithSolutions
    :<|> "api" :> "private" :> "login" :> "status" :> AuthProtect "cookie-auth" :> Get '[JSON] UserAlias
    :<|> "github-auth-callback" :> QueryParam "code" TokenExchangeCode :> Verb 'GET 303 '[JSON] LoginType
    :<|> "api" :> "private" :> "login" :> "local" :> Header "Referer" TL.Text :> Verb 'GET 303 '[JSON] LoginType
    :<|> "api" :> "private" :> "login" :> "logout" :> Header "Referer" TL.Text :> Verb 'GET 303 '[JSON] LoginType

mkApp :: AppData -> Servant.Server TournamentAPI
mkApp appData =
  uploadScenario appData
    :<|> uploadSolution appData
    :<|> getScenarioMetadata appData
    :<|> downloadRedactedScenario appData
    :<|> downloadSolution appData
    :<|> listScenarios
    :<|> listSolutions
    :<|> echoUsername
    :<|> doGithubCallback (authenticationStorage $ persistence appData) (gitHubCredentials appData)
    :<|> doLocalDevelopmentLogin (authenticationStorage $ persistence appData) (developmentMode appData)
    :<|> doLogout
 where
  echoUsername = return

type ToplevelAPI =
  TournamentAPI
    :<|> "api" :> Raw
    :<|> Raw

tournamentsApiHtml :: LBS.ByteString
tournamentsApiHtml =
  encodeUtf8
    . either (error . show) (Mark.renderHtml @())
    . Mark.commonmark ""
    $ T.pack "No documentation at this time."

toServantError :: Describable a => a -> ServerError
toServantError x = err500 {errBody = encodeUtf8 $ TL.fromStrict $ describeText x}

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie-auth") = UserAlias

myAppCookieName :: BS.ByteString
myAppCookieName = "servant-auth-cookie"

data LoginProblem = LoginProblem
  { problemMessage :: TL.Text
  , loginLink :: TL.Text
  }
  deriving (Generic, ToJSON)

--- | The auth handler wraps a function from Request -> Handler UserAlias.
--- We look for a token in the request headers that we expect to be in the cookie.
--- The token is then passed to our `lookupAccount` function.
authHandler ::
  AuthenticationStorage IO ->
  GitHubCredentials ->
  DeploymentEnvironment ->
  AuthHandler Request UserAlias
authHandler authStorage creds deployMode = mkAuthHandler handler
 where
  url = case deployMode of
    LocalDevelopment _ -> "api/private/login/local"
    ProdDeployment -> decodeUtf8 . LBS.fromStrict $ genLoginUrl creds

  throw401 msg = throwError $ err401 {errBody = encode $ LoginProblem msg url}
  handler req = either throw401 lookupAccount $ do
    cookie <- maybeToEither "Missing cookie header" $ lookup hCookie $ requestHeaders req
    maybeToEither "Missing token in cookie"
      . fmap (decodeUtf8 . LBS.fromStrict)
      . lookup myAppCookieName
      $ parseCookies cookie

  -- A method that, when given a cookie/password, will return a 'UserAlias'.
  lookupAccount :: TL.Text -> Handler UserAlias
  lookupAccount cookieText = do
    maybeUser <- liftIO $ usernameFromCookie authStorage cookieText
    case maybeUser of
      Nothing -> throwError (err403 {errBody = encode $ LoginProblem "Invalid cookie password" url})
      Just usr -> return usr

-- * Handlers

uploadScenario ::
  AppData ->
  Maybe TL.Text ->
  UserAlias ->
  MultipartData Mem ->
  Handler (Headers '[Header "Location" TL.Text] ScenarioCharacterization)
uploadScenario (AppData gameVersion _ persistenceLayer _) maybeRefererUrl userName multipartData =
  Handler . fmap addH . withExceptT toServantError . ExceptT $
    validateScenarioUpload
      args
      gameVersion
 where
  addH = addHeader (fromMaybe "/list-games.html" maybeRefererUrl)
  args =
    CommonValidationArgs
      defaultSolutionTimeout
      $ PersistenceArgs
        userName
        multipartData
        (scenarioStorage persistenceLayer)

uploadSolution ::
  AppData ->
  Maybe TL.Text ->
  UserAlias ->
  MultipartData Mem ->
  Handler (Headers '[Header "Location" TL.Text] SolutionFileCharacterization)
uploadSolution (AppData _ _ persistenceLayer _) maybeRefererUrl userName multipartData =
  Handler . fmap addH . withExceptT toServantError . ExceptT $
    validateSubmittedSolution
      args
      ((getContent . scenarioStorage) persistenceLayer)
 where
  addH = addHeader (fromMaybe "/list-solutions.html" maybeRefererUrl)
  args =
    CommonValidationArgs
      defaultSolutionTimeout
      $ PersistenceArgs
        userName
        multipartData
        (solutionStorage persistenceLayer)

getScenarioMetadata :: AppData -> Sha1 -> Handler ScenarioMetadata
getScenarioMetadata (AppData _ _ persistenceLayer _) scenarioSha1 =
  Handler . withExceptT toServantError $ do
    doc <-
      ExceptT $
        maybeToEither (DatabaseRetrievalFailure scenarioSha1)
          <$> (getContent . scenarioStorage) persistenceLayer scenarioSha1

    s <- withExceptT RetrievedInstantiationFailure $ initScenarioObjectWithEnv doc
    return $ view scenarioMetadata s

genLoginUrl :: GitHubCredentials -> BS.ByteString
genLoginUrl creds =
  "https://github.com/login/oauth/authorize"
    <> renderSimpleQuery
      True
      [ ("client_id", clientId creds)
      ]

downloadSolution :: AppData -> Sha1 -> Handler TL.Text
downloadSolution (AppData _ _ persistenceLayer _) solutionSha1 = do
  Handler . withExceptT toServantError $ do
    ExceptT $
      maybeToEither (DatabaseRetrievalFailure solutionSha1)
        <$> (fmap $ fmap decodeUtf8) ((getContent . solutionStorage) persistenceLayer solutionSha1)

downloadRedactedScenario :: AppData -> Sha1 -> Handler TL.Text
downloadRedactedScenario (AppData _ _ persistenceLayer _) scenarioSha1 = do
  Handler . withExceptT toServantError $ do
    doc <-
      ExceptT $
        maybeToEither (DatabaseRetrievalFailure scenarioSha1)
          <$> (getContent . scenarioStorage) persistenceLayer scenarioSha1

    rawYamlDict :: Map Key Value <- withExceptT YamlParseFailure . except . decodeEither' $ LBS.toStrict doc
    let redactedDict = M.delete "solution" rawYamlDict
    withExceptT DecodingFailure . except . decodeUtf8' . LBS.fromStrict $
      encodeWith defaultEncodeOptions redactedDict

listScenarios :: Handler [TournamentGame]
listScenarios =
  Handler . liftIO . withConnection databaseFilename $ runReaderT listGames

listSolutions :: Sha1 -> Handler GameWithSolutions
listSolutions sha1 =
  Handler . liftIO . withConnection databaseFilename . runReaderT $ listSubmissions sha1

doGithubCallback ::
  AuthenticationStorage IO ->
  GitHubCredentials ->
  Maybe TokenExchangeCode ->
  LoginHandler
doGithubCallback authStorage creds maybeCode = do
  c <- maybe (fail "Missing 'code' parameter") return maybeCode

  manager <- liftIO $ HC.newManager tlsManagerSettings
  receivedTokens <- exchangeCode manager creds c

  let aToken = token $ accessToken receivedTokens
  userInfo <- fetchAuthenticatedUser manager aToken
  let user = UserAlias $ login userInfo
  x <- doLoginResponse authStorage refererUrl user
  liftIO . withConnection databaseFilename . runReaderT $ do
    insertGitHubTokens user receivedTokens
  return x
 where
  refererUrl = "/list-games.html"

doLocalDevelopmentLogin ::
  AuthenticationStorage IO ->
  DeploymentEnvironment ->
  Maybe TL.Text ->
  LoginHandler
doLocalDevelopmentLogin authStorage envType maybeRefererUrl =
  case envType of
    ProdDeployment -> error "Login bypass not available in production"
    LocalDevelopment user ->
      doLoginResponse authStorage refererUrl user
 where
  refererUrl = fromMaybe "/list-games.html" maybeRefererUrl

makeCookieHeader :: BS.ByteString -> SetCookie
makeCookieHeader val =
  defaultSetCookie
    { setCookieName = myAppCookieName
    , setCookieValue = val
    , setCookiePath = Just "/api/private"
    }

doLogout :: Maybe TL.Text -> LoginHandler
doLogout maybeRefererUrl =
  return $
    addHeader (fromMaybe "/list-games.html" maybeRefererUrl) $
      addHeader ((makeCookieHeader "") {setCookieMaxAge = Just 0}) NoContent

doLoginResponse ::
  AuthenticationStorage IO ->
  TL.Text ->
  UserAlias ->
  LoginHandler
doLoginResponse authStorage refererUrl userAlias = do
  cookieString <-
    liftIO $ cookieFromUsername authStorage userAlias
  return $
    addHeader refererUrl $
      addHeader (makeCookieHeader $ LBS.toStrict $ encodeUtf8 cookieString) NoContent

-- * Web app declaration

app :: AppData -> Application
app appData = Servant.serveWithContext (Proxy :: Proxy ToplevelAPI) context server
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

  thisAuthHandler =
    authHandler
      (authenticationStorage $ persistence appData)
      (gitHubCredentials appData)
      (developmentMode appData)
  context = thisAuthHandler :. multipartOpts :. EmptyContext

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
      resp $ responseLBS ok200 [htmlType] tournamentsApiHtml
    htmlType = ("Content-Type", "text/html")

webMain ::
  AppData ->
  Warp.Port ->
  IO ()
webMain appData port = Warp.runSettings settings $ app appData
 where
  settings = Warp.setPort port Warp.defaultSettings
