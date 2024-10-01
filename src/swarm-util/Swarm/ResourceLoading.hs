{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Fetching game data
--
-- Various utilities related to loading game data files.
module Swarm.ResourceLoading (
  -- * Generic data access
  getDataDirSafe,
  getDataFileNameSafe,

  -- * Concrete data access
  getSwarmConfigIniFile,
  getSwarmSavePath,
  getSwarmHistoryPath,
  getSwarmAchievementsPath,

  -- ** Loading text files
  readAppData,
  NameGenerator (..),
  initNameGenerator,
) where

import Control.Algebra (Has)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Throw (Throw, liftEither, throwError)
import Control.Exception (catch)
import Control.Exception.Base (IOException)
import Control.Monad (forM, when, (<=<))
import Data.Array (Array, listArray)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Paths_swarm (getDataDir)
import Swarm.Failure
import Swarm.Util
import System.Directory (
  XdgDirectory (..),
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  getXdgDirectory,
  listDirectory,
 )
import System.FilePath
import Witch

-- | Read-only lists of adjectives and words for use in building random robot names
data NameGenerator = NameGenerator
  { adjList :: Array Int Text
  , nameList :: Array Int Text
  }

-- | Get subdirectory from swarm data directory.
--
-- This will first look in Cabal generated path and then
-- try a @data@ directory in 'XdgData' path.
--
-- The idea is that when installing with Cabal/Stack the first
-- is preferred, but when the players install a binary they
-- need to extract the `data` archive to the XDG directory.
getDataDirSafe ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  AssetData ->
  FilePath ->
  m FilePath
getDataDirSafe asset p = do
  d <- (`appDir` p) <$> sendIO getDataDir
  de <- sendIO $ doesDirectoryExist d
  if de
    then return d
    else do
      xd <- (`appDir` p) <$> sendIO (getSwarmXdgDataSubdir False "data")
      xde <- sendIO $ doesDirectoryExist xd
      if xde then return xd else throwError $ AssetNotLoaded (Data asset) xd $ DoesNotExist Directory
 where
  appDir r = \case
    "" -> r
    "." -> r
    d -> r </> d

-- | Get file from swarm data directory.
--
-- See the note in 'getDataDirSafe'.
getDataFileNameSafe ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  AssetData ->
  FilePath ->
  m FilePath
getDataFileNameSafe asset name = do
  d <- getDataDirSafe asset "."
  let fp = d </> name
  fe <- sendIO $ doesFileExist fp
  if fe
    then return fp
    else throwError $ AssetNotLoaded (Data asset) fp $ DoesNotExist File

getSwarmConfigIniFile :: Bool -> IO (Bool, FilePath)
getSwarmConfigIniFile createDirs = do
  swarmConfig <- getXdgDirectory XdgConfig "swarm"
  when createDirs (createDirectoryIfMissing True swarmConfig)
  let ini = swarmConfig </> "config.ini"
  iniExists <- doesFileExist ini
  return (iniExists, ini)

-- | Get path to swarm data, optionally creating necessary
--   directories. This could fail if user has bad permissions
--   on his own @$HOME@ or @$XDG_DATA_HOME@ which is unlikely.
getSwarmXdgDataSubdir :: Bool -> FilePath -> IO FilePath
getSwarmXdgDataSubdir createDirs subDir = do
  swarmData <- (</> subDir) <$> getXdgDirectory XdgData "swarm"
  when createDirs (createDirectoryIfMissing True swarmData)
  pure swarmData

getSwarmXdgDataFile :: Bool -> FilePath -> IO FilePath
getSwarmXdgDataFile createDirs filepath = do
  let (subDir, file) = splitFileName filepath
  d <- getSwarmXdgDataSubdir createDirs subDir
  return $ d </> file

-- | Get path to swarm saves, optionally creating necessary
--   directories.
getSwarmSavePath :: Bool -> IO FilePath
getSwarmSavePath createDirs = getSwarmXdgDataSubdir createDirs "saves"

-- | Get path to swarm history, optionally creating necessary
--   directories.
getSwarmHistoryPath :: Bool -> IO FilePath
getSwarmHistoryPath createDirs = getSwarmXdgDataFile createDirs "history"

-- | Get a path to the directory where achievement records are
--   stored. If the argument is set to @True@, create the directory if
--   it does not exist.
getSwarmAchievementsPath :: Bool -> IO FilePath
getSwarmAchievementsPath createDirs = getSwarmXdgDataSubdir createDirs "achievement"

-- | Read all the @.txt@ files in the @data/@ directory.
readAppData ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  m (Map Text Text)
readAppData = do
  d <- getDataDirSafe AppAsset "."
  dirMembers :: [FilePath] <-
    (liftEither <=< sendIO) $
      (pure <$> listDirectory d) `catch` \(e :: IOException) ->
        return . Left . AssetNotLoaded (Data AppAsset) d . CustomMessage . T.pack $ show e
  let fs = filter ((== ".txt") . takeExtension) dirMembers

  filesList <- sendIO $ forM fs (\f -> (into @Text (dropExtension f),) <$> readFileMayT (d </> f))
  return $ M.fromList . mapMaybe sequenceA $ filesList

initNameGenerator :: Has (Throw SystemFailure) sig m => Map Text Text -> m NameGenerator
initNameGenerator appDataMap = do
  adjs <- getDataLines "adjectives"
  names <- getDataLines "names"
  return $
    NameGenerator
      { adjList = makeArr adjs
      , nameList = makeArr names
      }
 where
  makeArr xs = listArray (0, length xs - 1) xs
  getDataLines f = case M.lookup f appDataMap of
    Nothing ->
      throwError $
        AssetNotLoaded (Data NameGeneration) (into @FilePath f <.> "txt") (DoesNotExist File)
    Just content -> return . drop 1 . T.lines $ content
