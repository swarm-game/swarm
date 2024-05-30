{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Authentication logic for Swarm tournament server.
module Swarm.Web.Auth where

import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.UTF8 as BSU
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as DTE
import Data.Text.Lazy qualified as TL
import Database.SQLite.Simple.ToField
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types (hAccept, hUserAgent, parseSimpleQuery, renderSimpleQuery)
import Servant
import Text.Read (readMaybe)

data GitHubCredentials = GitHubCredentials
  { clientId :: BS.ByteString
  , clientSecret :: BS.ByteString
  }

instance FromJSON GitHubCredentials where
  parseJSON = withObject "GitHubCredentials" $ \v -> do
    clientId <- BSU.fromString <$> v .: "CLIENT_ID"
    clientSecret <- BSU.fromString <$> v .: "CLIENT_SECRET"
    pure GitHubCredentials {..}

newtype TokenExchangeCode = TokenExchangeCode BS.ByteString

instance FromHttpApiData TokenExchangeCode where
  parseUrlPiece = return . TokenExchangeCode . DTE.encodeUtf8

newtype AccessToken = AccessToken BS.ByteString

instance ToField AccessToken where
  toField (AccessToken x) = toField x

newtype RefreshToken = RefreshToken BS.ByteString

instance ToField RefreshToken where
  toField (RefreshToken x) = toField x

data UserApiResponse = UserApiResponse
  { login :: TL.Text
  , id :: Int
  , name :: TL.Text
  }
  deriving (Generic, FromJSON)

data Expirable a = Expirable
  { token :: a
  , expirationSeconds :: Int
  }

fetchAuthenticatedUser ::
  (MonadIO m, MonadThrow m, MonadFail m) =>
  HC.Manager ->
  AccessToken ->
  m UserApiResponse
fetchAuthenticatedUser manager (AccessToken tok) = do
  req <- HC.parseUrlThrow "https://api.github.com/user"
  resp <-
    liftIO
      . flip HC.httpLbs manager
      . HC.applyBearerAuth tok
      $ req
        { HC.requestHeaders =
            [ (hAccept, "application/vnd.github+json")
            , (hUserAgent, "Swarm Gaming Hub")
            , ("X-GitHub-Api-Version", "2022-11-28")
            ]
        }
  either fail return $ eitherDecode $ HC.responseBody resp

data ReceivedTokens = ReceivedTokens
  { accessToken :: Expirable AccessToken
  , refreshToken :: Expirable RefreshToken
  }

packExchangeResponse ::
  M.Map ByteString ByteString ->
  Maybe ReceivedTokens
packExchangeResponse valMap =
  ReceivedTokens
    <$> (Expirable <$> atVal <*> toInt "expires_in")
    <*> (Expirable <$> rtVal <*> toInt "refresh_token_expires_in")
 where
  toInt k = readMaybe . BSU.toString =<< M.lookup k valMap

  atVal = AccessToken <$> M.lookup "access_token" valMap
  rtVal = RefreshToken <$> M.lookup "refresh_token" valMap

exchangeCode ::
  (MonadIO m, MonadThrow m, MonadFail m) =>
  HC.Manager ->
  GitHubCredentials ->
  TokenExchangeCode ->
  m ReceivedTokens
exchangeCode manager creds (TokenExchangeCode code) = do
  let qParms =
        T.unpack . DTE.decodeUtf8 $
          renderSimpleQuery
            True
            [ ("client_id", clientId creds)
            , ("client_secret", clientSecret creds)
            , ("code", code)
            ]
  req <- HC.parseUrlThrow $ "https://github.com/login/oauth/access_token" <> qParms
  resp <- liftIO $ flip HC.httpLbs manager $ req {HC.method = "POST"}

  let parms = parseSimpleQuery $ LBS.toStrict $ HC.responseBody resp
      valMap = M.fromList parms

  maybe
    (fail "Response did not include access token")
    return
    $ packExchangeResponse valMap
