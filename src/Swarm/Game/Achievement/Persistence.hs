{-# LANGUAGE OverloadedStrings #-}

module Swarm.Game.Achievement.Persistence where

import Control.Arrow (left)
import Control.Carrier.Lift (sendIO)
import Control.Monad (forM, forM_)
import Data.Either (partitionEithers)
import Data.Yaml qualified as Y
import Swarm.Failure
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.ResourceLoading (getSwarmXdgDataSubdir)
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
 )
import System.FilePath ((</>))

-- | Get path to swarm achievements, optionally creating necessary
--   directories.
getSwarmAchievementsPath :: Bool -> IO FilePath
getSwarmAchievementsPath createDirs = getSwarmXdgDataSubdir createDirs "achievement"

-- | Load saved info about achievements from XDG data directory.
-- Returns a tuple of warnings and attained achievements.
loadAchievementsInfo ::
  IO ([SystemFailure], [Attainment])
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
            return $ left (AssetNotLoaded Achievement . PathLoadFailure p . CanNotParse) eitherDecodedFile
          else return . Left $ AssetNotLoaded Achievement $ PathLoadFailure p (EntryNot File)
      return $ partitionEithers eithersList
    else return ([AssetNotLoaded Achievement $ PathLoadFailure "." $ DoesNotExist Directory], [])

-- | Save info about achievements to XDG data directory.
saveAchievementsInfo ::
  [Attainment] ->
  IO ()
saveAchievementsInfo attainmentList = do
  savedAchievementsPath <- getSwarmAchievementsPath True
  forM_ attainmentList $ \x -> do
    let achievementName = case _achievement x of
          GlobalAchievement y -> show y
          GameplayAchievement y -> show y
        fullPath = savedAchievementsPath </> achievementName
    Y.encodeFile fullPath x
