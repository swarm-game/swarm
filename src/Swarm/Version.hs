{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Swarm.Version
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Query current and upstream Swarm version.
module Swarm.Version where

import Control.Monad (forM)
import Data.Aeson (Array, Value (..), (.:))
import Data.Aeson.Types (parseMaybe)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isDigit)
import Data.Foldable (find, toList)
import Data.List.Extra (breakOnEnd)
import Data.Maybe (catMaybes)
import Data.Version (showVersion)
import Data.Yaml (ParseException, decodeEither')
import GitHash (GitInfo, giBranch, giHash, giTag, tGitInfoCwdTry)
import Network.HTTP.Client (
  Request (requestHeaders),
  Response (responseBody),
  httpLbs,
  newManager,
  parseRequest,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hUserAgent)
import Paths_swarm qualified

gitInfo :: Either String GitInfo
gitInfo = $$tGitInfoCwdTry

commitInfo :: String
commitInfo = case gitInfo of
  Left _ -> ""
  Right git -> " (" <> giBranch git <> "@" <> take 10 (giHash git) <> ")"

type Hash = String

isSwarmReleaseTag :: String -> Bool
isSwarmReleaseTag = all (\c -> isDigit c || c == '.')

tagVersion :: Maybe (Hash, String)
tagVersion = case gitInfo of
  Left _ -> Nothing
  Right gi ->
    let t = giTag gi
        ((ta, _num), ghash) = first (first init . breakOnEnd "-" . init) $ breakOnEnd "-" t
     in if isSwarmReleaseTag ta
          then Just (ghash, ta)
          else Nothing

version :: String
version =
  let v = showVersion Paths_swarm.version
   in if v == "0.0.0.1" then "pre-alpha version" else v

upstreamReleaseVersion :: IO (Maybe String)
upstreamReleaseVersion = do
  manager <- newManager tlsManagerSettings
  -- -------------------------------------------------------------------------------
  -- SEND REQUEST
  request <- parseRequest "https://api.github.com/repos/swarm-game/swarm/releases"
  response <-
    httpLbs
      request {requestHeaders = [(hUserAgent, "swarm-game/swarm-swarmversion")]}
      manager
  -- -------------------------------------------------------------------------------
  -- PARSE RESPONSE
  -- putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  let result = decodeEither' (BS.pack . BSL.unpack $ responseBody response) :: Either ParseException Array
  case result of
    Left _e -> do
      -- print e
      return Nothing
    Right rs -> do
      ts <- forM (toList rs) $ \r -> do
        return . flip parseMaybe r $ \case
          Object o -> o .: "tag_name"
          _ -> fail "The JSON list does not contain structures!"
      return $ find isSwarmReleaseTag . catMaybes $ ts

data NewReleaseFailure where
  NoUpstreamRelease :: NewReleaseFailure
  OnDevelopmentBranch :: String -> NewReleaseFailure
  OldUpstreamRelease :: String -> String -> NewReleaseFailure

instance Show NewReleaseFailure where
  show = \case
    NoUpstreamRelease -> "No upstream releases found."
    OnDevelopmentBranch br -> "Currently on development branch '" <> br <> "', skipping release query."
    OldUpstreamRelease upTag myTag -> "Upstream release '" <> upTag <> "' is not newer than mine ('" <> myTag <> "')."

getNewerReleaseVersion :: IO (Either NewReleaseFailure String)
getNewerReleaseVersion =
  case gitInfo of
    -- when using cabal install, the git info is unavailable, which is of no interest to players
    Left _e -> maybe (Left NoUpstreamRelease) Right <$> upstreamReleaseVersion
    Right gi ->
      if giBranch gi /= "main"
        then return . Left . OnDevelopmentBranch $ giBranch gi
        else do
          mUpTag <- upstreamReleaseVersion
          return $ case mUpTag of
            Nothing -> Left NoUpstreamRelease
            Just upTag -> case snd <$> tagVersion of
              Nothing -> Right upTag
              Just myTag ->
                if myTag >= upTag
                  then Left $ OldUpstreamRelease upTag myTag
                  else Right upTag
