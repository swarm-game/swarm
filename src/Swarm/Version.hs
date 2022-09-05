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
module Swarm.Version (
  -- * Git info
  gitInfo,
  commitInfo,
  CommitHash,
  tagVersion,

  -- * PVP version
  isSwarmReleaseTag,
  version,

  -- ** Upstream release
  tagToVersion,
  upstreamReleaseVersion,
  getNewerReleaseVersion,
  NewReleaseFailure (..),
) where

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
import Data.Version (Version (..), parseVersion, showVersion)
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
import Text.ParserCombinators.ReadP (readP_to_S)

-- $setup
-- >>> import Data.Bifunctor (first)
-- >>> import Data.Version (Version (..), parseVersion)
-- >>> import Text.ParserCombinators.ReadP (readP_to_S)

gitInfo :: Either String GitInfo
gitInfo = $$tGitInfoCwdTry

commitInfo :: String
commitInfo = case gitInfo of
  Left _ -> ""
  Right git -> " (" <> giBranch git <> "@" <> take 10 (giHash git) <> ")"

type CommitHash = String

-- | Check that the tag follows the PVP versioning policy.
--
-- Note that this filters out VS Code plugin releases.
isSwarmReleaseTag :: String -> Bool
isSwarmReleaseTag = all (\c -> isDigit c || c == '.')

tagVersion :: Maybe (CommitHash, String)
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

-- | Get the current upstream release version if any.
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
          Object o -> do
            pre <- o .: "prerelease"
            if pre
              then fail "Not a real release!"
              else o .: "tag_name"
          _ -> fail "The JSON list does not contain structures!"
      return $ find isSwarmReleaseTag . catMaybes $ ts

data NewReleaseFailure where
  NoUpstreamRelease :: NewReleaseFailure
  OnDevelopmentBranch :: String -> NewReleaseFailure
  OldUpstreamRelease :: Version -> Version -> NewReleaseFailure

instance Show NewReleaseFailure where
  show = \case
    NoUpstreamRelease -> "No upstream releases found."
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
getNewerReleaseVersion :: IO (Either NewReleaseFailure String)
getNewerReleaseVersion =
  case gitInfo of
    -- when using cabal install, the git info is unavailable, which is of no interest to players
    Left _e -> maybe (Left NoUpstreamRelease) Right <$> upstreamReleaseVersion
    Right gi ->
      if giBranch gi /= "main"
        then return . Left . OnDevelopmentBranch $ giBranch gi
        else getUpVer <$> upstreamReleaseVersion
 where
  myVer :: Version
  myVer = Paths_swarm.version
  getUpVer :: Maybe String -> Either NewReleaseFailure String
  getUpVer = \case
    Nothing -> Left NoUpstreamRelease
    Just upTag ->
      let upVer = tagToVersion upTag
       in if myVer >= upVer
            then Left $ OldUpstreamRelease upVer myVer
            else Right upTag
