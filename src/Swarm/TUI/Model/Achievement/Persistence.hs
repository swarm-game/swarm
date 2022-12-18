{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.Model.Achievement.Persistence where

import Control.Arrow (left)
import Control.Carrier.Lift (sendIO)
import Data.Text (Text, pack)
import Data.Yaml qualified as Y
import Data.Yaml.Aeson (prettyPrintParseException)
import Swarm.TUI.Model.Achievement.Attainment
import Swarm.Util
import System.Directory (
  doesFileExist,
 )
import System.FilePath

-- | Get path to swarm achievements, optionally creating necessary
--   directories.
getSwarmAchievementsPath :: Bool -> IO FilePath
getSwarmAchievementsPath createDirs =
  (</> "achievements") <$> getSwarmDataPath createDirs

-- | Load saved info about achievements from XDG data directory.
loadAchievementsInfo ::
  IO (Either Text [Attainment])
loadAchievementsInfo = do
  savedAchievementsPath <- getSwarmAchievementsPath True
  hasSavedAchievements <- sendIO $ doesFileExist savedAchievementsPath
  if not hasSavedAchievements
    then return $ pure []
    else
      left (pack . prettyPrintParseException)
        <$> sendIO (Y.decodeFileEither savedAchievementsPath)

-- | Save info about achievements to XDG data directory.
saveAchievementsInfo ::
  [Attainment] ->
  IO ()
saveAchievementsInfo si = do
  savedAchievementsPath <- getSwarmAchievementsPath True
  Y.encodeFile savedAchievementsPath si
