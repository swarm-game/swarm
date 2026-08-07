{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Fetching game data
--
-- Various utilities related to loading game data files.
module Swarm.ResourceLoading (
  -- * Generic data access
  getDataDirSafe,
  getDataDirThrow,
  getDataFileNameThrow,

  -- * Concrete data access
  getSwarmConfigIniFile,
  getSwarmSavePath,
  getSwarmHistoryPath,
  getSwarmAchievementsPath,

  -- ** Loading text files
  readAppData,
  NameGenerator (..),
  initNameGenerator,

  -- ** Loading recursive collections
  Collection (..),
  CollectionItem (..),
  _Single,
  _SubCollection,
  emptyCollection,
  collectionItemName,
  collectionToList,
  flattenCollection,
  collectionItemByPath,
  atPath,
  CollectionConfig (..),
  loadCollection,
) where

import Control.Exception (catch)
import Control.Exception.Base (IOException)
import Control.Monad (forM, guard, when, (<=<))
import Data.Array (Array, listArray)
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Paths_swarm (getDataDir)
import Swarm.Failure (
  Asset (Data),
  AssetData (AppAsset, NameGeneration),
  Entry (Directory, File),
  LoadingFailure (DoesNotExist, SystemFailure),
  SystemFailure (AssetNotLoaded, CustomFailure),
 )
import Swarm.ResourceLoading.Collection
import Swarm.Util (Encoding (UTF8), readFileMayT)
import Swarm.Util.Effect (liftEither, (???))
import System.Directory (
  XdgDirectory (..),
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  getXdgDirectory,
  listDirectory,
 )
import System.FilePath (
  dropExtension,
  normalise,
  splitFileName,
  takeExtension,
  (<.>),
  (</>),
 )
import Witch (into)

-- | Read-only lists of adjectives and words for use in building random robot names
data NameGenerator = NameGenerator
  { adjList :: Array Int Text
  , nameList :: Array Int Text
  }

-- | Ensure that a given directory exists, wrapping it in 'Just' if it
--   does exist and yielding 'Nothing' otherwise.
guardDir :: IOE :> es => FilePath -> Eff es (Maybe FilePath)
guardDir dir = do
  ex <- liftIO $ doesDirectoryExist dir
  pure $ guard ex $> dir

-- | Get subdirectory from swarm data directory.  Return Nothing if
--   not found. This will first look in Cabal generated path and then
--   try a @data@ directory in 'XdgData' path.
getDataDirSafe :: IOE :> es => FilePath -> Eff es (Maybe FilePath)
getDataDirSafe p = do
  md <- tryDir getDataDir
  case md of
    Nothing -> tryDir (getSwarmXdgDataSubdir False "data")
    Just d -> pure (Just d)
 where
  tryDir m = liftIO m >>= guardDir . normalise . (</> p)

-- | Get subdirectory from swarm data directory; throw an error if not
--   found. This will first look in Cabal generated path and then
--   try a @data@ directory in 'XdgData' path.
--
--   The idea is that when installing with Cabal/Stack the first is
--   preferred, but when the players install a binary they need to
--   extract the `data` archive to the XDG directory.
getDataDirThrow ::
  (Error SystemFailure :> es, IOE :> es) =>
  AssetData ->
  FilePath ->
  Eff es FilePath
getDataDirThrow asset p = do
  getDataDirSafe p
    ??? throwError (AssetNotLoaded (Data asset) p $ DoesNotExist Directory)

-- | Get file from swarm data directory.
--
-- See the note in 'getDataDirSafe'.
getDataFileNameThrow ::
  (Error SystemFailure :> es, IOE :> es) =>
  AssetData ->
  FilePath ->
  Eff es FilePath
getDataFileNameThrow asset name = do
  d <- getDataDirThrow asset "."
  let fp = d </> name
  fe <- liftIO $ doesFileExist fp
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
  (Error SystemFailure :> es, IOE :> es) =>
  Eff es (Map Text Text)
readAppData = do
  d <- getDataDirThrow AppAsset "."
  dirMembers :: [FilePath] <-
    (liftEither <=< liftIO) $
      (pure <$> listDirectory d) `catch` \(e :: IOException) ->
        return . Left . AssetNotLoaded (Data AppAsset) d . SystemFailure . CustomFailure . T.pack $ show e
  let fs = filter ((== ".txt") . takeExtension) dirMembers

  filesList <- liftIO $ forM fs (\f -> (into @Text (dropExtension f),) <$> readFileMayT UTF8 (d </> f))
  return $ M.fromList . mapMaybe sequenceA $ filesList

initNameGenerator :: Error SystemFailure :> es => Map Text Text -> Eff es NameGenerator
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
