{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Swarm.Version
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Query current and upstream Swarm version.
module Swarm.Version (
  -- * PVP version
  isSwarmReleaseTag,
  version,

  -- ** Upstream release
  tagToVersion,
  upstreamReleaseVersion,
  getNewerReleaseVersion,
  NewReleaseFailure (..),
) where

import Control.Exception (catch, displayException)
import Data.Aeson (Array, Value (..), (.:))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isDigit)
import Data.Either (lefts, rights)
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import Data.Version (Version (..), parseVersion, showVersion)
import Data.Yaml (ParseException, Parser, decodeEither', parseEither)
import GitHash (GitInfo, giBranch)
import Network.HTTP.Client (
  HttpException,
  Request (requestHeaders),
  Response (responseBody),
  httpLbs,
  newManager,
  parseRequest,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hUserAgent)
import Paths_swarm qualified
import Text.ParserCombinators.ReadP (readP_to_S)

-- $setup
-- >>> import Data.Bifunctor (first)
-- >>> import Data.Version (Version (..), parseVersion)
-- >>> import Text.ParserCombinators.ReadP (readP_to_S)

-- | Check that the tag follows the PVP versioning policy.
--
-- Note that this filters out VS Code plugin releases.
isSwarmReleaseTag :: String -> Bool
isSwarmReleaseTag = all (\c -> isDigit c || c == '.')

version :: String
version =
  let v = showVersion Paths_swarm.version
   in if v == "0.0.0.1" then "pre-alpha version" else v

-- | Get the current upstream release version if any.
upstreamReleaseVersion :: IO (Either NewReleaseFailure String)
upstreamReleaseVersion =
  catch
    (either parseFailure getRelease . decodeResp <$> sendRequest)
    (return . Left . queryFailure)
 where
  -- ------------------------------
  -- send request to GitHub API
  sendRequest :: IO (Response BSL.ByteString)
  sendRequest = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "https://api.github.com/repos/swarm-game/swarm/releases"
    httpLbs
      request {requestHeaders = [(hUserAgent, "swarm-game/swarm-swarmversion")]}
      manager
  -- ------------------------------
  -- get the latest actual release
  getRelease :: Array -> Either NewReleaseFailure String
  getRelease rs =
    let ts = parseReleases rs
        maybeRel = listToMaybe $ rights ts
     in case maybeRel of
          Nothing -> Left $ NoMainUpstreamRelease (lefts ts)
          Just rel -> Right rel
  -- ------------------------------
  -- pretty print failures
  parseFailure :: ParseException -> Either NewReleaseFailure String
  parseFailure e = Left . FailedReleaseQuery $ "Failure during response parsing: " <> displayException e
  queryFailure :: HttpException -> NewReleaseFailure
  queryFailure e = FailedReleaseQuery $ "Failure requesting GitHub releases: " <> displayException e
  -- ------------------------------
  -- parsing helpers
  decodeResp :: Response BSL.ByteString -> Either ParseException Array
  decodeResp resp = decodeEither' (BS.pack . BSL.unpack $ responseBody resp)
  parseReleases :: Array -> [Either String String]
  parseReleases = map (parseEither parseRelease) . toList

parseRelease :: Value -> Parser String
parseRelease = \case
  Object o -> do
    pre <- o .: "prerelease"
    if pre
      then fail "Not a real release!"
      else do
        t <- o .: "tag_name"
        if isSwarmReleaseTag t
          then return t
          else fail $ "The release '" <> t <> "' is not main Swarm release!"
  _otherValue -> fail "The JSON release is not an Object!"

data NewReleaseFailure where
  FailedReleaseQuery :: String -> NewReleaseFailure
  NoMainUpstreamRelease :: [String] -> NewReleaseFailure
  OnDevelopmentBranch :: String -> NewReleaseFailure
  OldUpstreamRelease :: Version -> Version -> NewReleaseFailure

instance Show NewReleaseFailure where
  show = \case
    FailedReleaseQuery e -> "Failed to query upstream release: " <> e
    NoMainUpstreamRelease fs ->
      "No upstream releases found."
        <> if null fs
          then ""
          else " Rejected:\n" <> unlines (zipWith ((<>) . show @Int) [1 ..] fs)
    OnDevelopmentBranch br -> "Currently on development branch '" <> br <> "', skipping release query."
    OldUpstreamRelease up my ->
      "Upstream release '"
        <> showVersion up
        <> "' is not newer than mine ('"
        <> showVersion my
        <> "')."

-- | Read Swarm tag as Version.
--
-- Swarm tags follow the PVP versioning scheme, so comparing them makes sense.
--
-- >>> map (first versionBranch) $ readP_to_S parseVersion "0.1.0.0"
-- [([0],".1.0.0"),([0,1],".0.0"),([0,1,0],".0"),([0,1,0,0],"")]
-- >>> Version [0,0,0,1] [] < tagToVersion "0.1.0.0"
-- True
tagToVersion :: String -> Version
tagToVersion = fst . last . readP_to_S parseVersion

-- | Get a newer upstream release version.
--
-- This function can fail if the current branch is not main,
-- if there is no Internet connection or no newer release.
getNewerReleaseVersion :: Maybe GitInfo -> IO (Either NewReleaseFailure String)
getNewerReleaseVersion mgi =
  case mgi of
    -- when using cabal install, the git info is unavailable, which is of no interest to players
    Nothing -> (>>= getUpVer) <$> upstreamReleaseVersion
    Just gi ->
      if giBranch gi /= "main"
        then return . Left . OnDevelopmentBranch $ giBranch gi
        else (>>= getUpVer) <$> upstreamReleaseVersion
 where
  myVer :: Version
  myVer = Paths_swarm.version
  getUpVer :: String -> Either NewReleaseFailure String
  getUpVer upTag =
    let upVer = tagToVersion upTag
     in if myVer >= upVer
          then Left $ OldUpstreamRelease upVer myVer
          else Right upTag
