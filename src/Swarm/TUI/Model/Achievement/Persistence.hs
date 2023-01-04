{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.Model.Achievement.Persistence where

import Control.Arrow (left)
import Control.Carrier.Lift (sendIO)
import Control.Monad (forM, forM_)
import Data.Either (partitionEithers)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import Data.Yaml.Aeson (prettyPrintParseException)
import Swarm.TUI.Model.Achievement.Attainment
import Swarm.Util
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
 )
import System.FilePath
import System.IO (hPutStrLn, stderr)

-- | Get path to swarm achievements, optionally creating necessary
--   directories.
getSwarmAchievementsPath :: Bool -> IO FilePath
getSwarmAchievementsPath createDirs = getSwarmXdgDataSubdir createDirs "achievement"

-- | Load saved info about achievements from XDG data directory.
loadAchievementsInfo ::
  IO (Either Text [Attainment])
loadAchievementsInfo = do
  savedAchievementsPath <- getSwarmAchievementsPath False
  doesParentExist <- doesDirectoryExist savedAchievementsPath
  if doesParentExist
    then do
      contents <- listDirectory savedAchievementsPath
      eithersList <- forM contents $ \p -> do
        let fullPath = savedAchievementsPath </> p
        isFile <- doesFileExist fullPath
        if isFile
          then do
            eitherDecodedFile <- sendIO (Y.decodeFileEither fullPath)
            return $ left (pack . prettyPrintParseException) eitherDecodedFile
          else
            return $
              Left $
                T.unwords
                  [ "Entry"
                  , quote $ T.pack fullPath
                  , "is not a file!"
                  ]
      let (bads, goods) = partitionEithers eithersList
      forM_ bads $ \b ->
        hPutStrLn stderr $ T.unpack b
      return $ pure goods
    else return $ pure []

-- | Save info about achievements to XDG data directory.
saveAchievementsInfo ::
  [Attainment] ->
  IO ()
saveAchievementsInfo attainmentList = do
  savedAchievementsPath <- getSwarmAchievementsPath True
  forM_ attainmentList $ \x -> do
    let achievementName =
          T.unpack $
            T.takeWhileEnd (/= ' ') $
              T.pack $
                show (_achievement x)
        fullPath = savedAchievementsPath </> achievementName
    Y.encodeFile fullPath x
