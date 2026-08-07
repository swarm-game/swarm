-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Achievements load/save
--
-- Load/save logic for achievements.
-- Each achievement is saved to its own file to better
-- support forward-compatibility.
module Swarm.Game.Achievement.Persistence where

import Control.Arrow (left)
import Control.Monad (forM_)
import Data.Yaml qualified as Y
import Effectful
import Swarm.Effect.Warn.Local (Warn)
import Swarm.Failure
import Swarm.Game.Achievement.Attainment
import Swarm.Game.Achievement.Definitions
import Swarm.ResourceLoading (getSwarmAchievementsPath)
import Swarm.Util.Effect (forMW)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

-- | Load saved info about achievements from XDG data directory.
--   Returns a list of attained achievements.
loadAchievementsInfo ::
  (Warn SystemFailure :> es, IOE :> es) =>
  Eff es [Attainment]
loadAchievementsInfo = do
  savedAchievementsPath <- liftIO $ getSwarmAchievementsPath False
  doesParentExist <- liftIO $ doesDirectoryExist savedAchievementsPath
  if doesParentExist
    then do
      contents <- liftIO $ listDirectory savedAchievementsPath
      forMW contents $ \p -> do
        let fullPath = savedAchievementsPath </> p
        isFile <- liftIO $ doesFileExist fullPath
        if isFile
          then do
            eitherDecodedFile <- liftIO (Y.decodeFileEither fullPath)
            return $ left (AssetNotLoaded Achievement p . CanNotParseYaml) eitherDecodedFile
          else return . Left $ AssetNotLoaded Achievement p (EntryNot File)
    else do
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
