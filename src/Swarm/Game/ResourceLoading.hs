{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Various utilities related to loading game data files.
module Swarm.Game.ResourceLoading where

import Control.Exception (catch)
import Control.Exception.Base (IOException)
import Control.Monad (forM, when)
import Control.Monad.Except (ExceptT (..), MonadIO, liftIO)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Paths_swarm (getDataDir)
import Swarm.Game.Failure
import Swarm.Util
import System.Directory (
  XdgDirectory (XdgData),
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  getXdgDirectory,
  listDirectory,
 )
import System.FilePath
import Witch

-- | Get subdirectory from swarm data directory.
--
-- This will first look in Cabal generated path and then
-- try a `data` directory in 'XdgData' path.
--
-- The idea is that when installing with Cabal/Stack the first
-- is preferred, but when the players install a binary they
-- need to extract the `data` archive to the XDG directory.
getDataDirSafe :: MonadIO m => AssetData -> FilePath -> m (Either SystemFailure FilePath)
getDataDirSafe asset p = do
  d <- (`appDir` p) <$> liftIO getDataDir
  de <- liftIO $ doesDirectoryExist d
  if de
    then return $ Right d
    else do
      xd <- (`appDir` p) <$> liftIO (getSwarmXdgDataSubdir False "data")
      xde <- liftIO $ doesDirectoryExist xd
      return $ if xde then Right xd else Left $ AssetNotLoaded (Data asset) xd $ DoesNotExist Directory
 where
  appDir r = \case
    "" -> r
    "." -> r
    d -> r </> d

-- | Get file from swarm data directory.
--
-- See the note in 'getDataDirSafe'.
getDataFileNameSafe ::
  MonadIO m =>
  AssetData ->
  FilePath ->
  ExceptT SystemFailure m FilePath
getDataFileNameSafe asset name = do
  d <- ExceptT $ getDataDirSafe asset "."
  let fp = d </> name
  fe <- liftIO $ doesFileExist fp
  ExceptT $
    return $
      if fe
        then Right fp
        else Left $ AssetNotLoaded (Data asset) fp $ DoesNotExist File

-- | Get a nice message suggesting to download `data` directory to 'XdgData'.
dataNotFound :: FilePath -> IO LoadingFailure
dataNotFound f = do
  d <- getSwarmXdgDataSubdir False ""
  let squotes = squote . T.pack
  return $
    CustomMessage $
      T.unlines
        [ "Could not find the data: " <> squotes f
        , "Try downloading the Swarm 'data' directory to: " <> squotes (d </> "data")
        ]

-- | Get path to swarm data, optionally creating necessary
--   directories. This could fail if user has bad permissions
--   on his own $HOME or $XDG_DATA_HOME which is unlikely.
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

-- | Read all the .txt files in the data/ directory.
readAppData :: ExceptT SystemFailure IO (Map Text Text)
readAppData = do
  d <- ExceptT $ getDataDirSafe AppAsset "."
  dirMembers <-
    ExceptT $
      fmap pure (listDirectory d) `catch` \(e :: IOException) ->
        return . Left . AssetNotLoaded (Data AppAsset) d . CustomMessage . T.pack $ show e
  let fs = filter ((== ".txt") . takeExtension) dirMembers

  filesList <- liftIO $ forM fs (\f -> (into @Text (dropExtension f),) <$> readFileMayT (d </> f))
  return $ M.fromList . mapMaybe sequenceA $ filesList
