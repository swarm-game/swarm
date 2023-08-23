{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Load/save logic for achievements.
-- Each achievement is saved to its own file to better
-- support forward-compatibility.
module Swarm.Game.Achievement.Persistence where

import Control.Arrow (left)
import Control.Effect.Accum
import Control.Effect.Lift
import Control.Monad (forM_)
import Data.Sequence (Seq)
import Data.Yaml qualified as Y
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.Game.Failure
import Swarm.Game.ResourceLoading (getSwarmXdgDataSubdir)
import Swarm.Util.Effect (forMW, warn)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

-- | Get path to swarm achievements, optionally creating necessary
--   directories.
getSwarmAchievementsPath :: Bool -> IO FilePath
getSwarmAchievementsPath createDirs = getSwarmXdgDataSubdir createDirs "achievement"

-- | Load saved info about achievements from XDG data directory.
-- Returns a tuple of warnings and attained achievements.
loadAchievementsInfo ::
  (Has (Accum (Seq SystemFailure)) sig m, Has (Lift IO) sig m) =>
  m [Attainment]
loadAchievementsInfo = do
  savedAchievementsPath <- sendIO $ getSwarmAchievementsPath False
  doesParentExist <- sendIO $ doesDirectoryExist savedAchievementsPath
  if doesParentExist
    then do
      contents <- sendIO $ listDirectory savedAchievementsPath
      forMW contents $ \p -> do
        let fullPath = savedAchievementsPath </> p
        isFile <- sendIO $ doesFileExist fullPath
        if isFile
          then do
            eitherDecodedFile <- sendIO (Y.decodeFileEither fullPath)
            return $ left (AssetNotLoaded Achievement p . CanNotParseYaml) eitherDecodedFile
          else return . Left $ AssetNotLoaded Achievement p (EntryNot File)
    else do
      warn $ AssetNotLoaded Achievement "." $ DoesNotExist Directory
      return []

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
