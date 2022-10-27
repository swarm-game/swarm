{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.Model.Achievement.Persistence where

import System.FilePath
import Data.Text (Text)
import Data.Time (ZonedTime)
import Swarm.Util

-- | Get path to swarm achievements, optionally creating necessary
--   directories.
getSwarmAchievementsPath :: Bool -> IO FilePath
getSwarmAchievementsPath createDirs =
  (</> "achievements") <$> getSwarmDataPath createDirs

{-
-- | Load saved info about achievements from XDG data directory.
loadAchievementsInfo ::
  (Has (Lift IO) sig m, Has (Throw Text) sig m) =>
  FilePath ->
  m ScenarioInfo
loadAchievementsInfo p = do
  path <- sendIO $ normalizeScenarioPath (SC Nothing mempty) p
  infoPath <- sendIO $ scenarioPathToSavePath path <$> getSwarmSavePath False
  hasInfo <- sendIO $ doesFileExist infoPath
  if not hasInfo
    then do
      return $ ScenarioInfo path NotStarted NotStarted NotStarted
    else
      sendIO (decodeFileEither infoPath)
        >>= either (throwError . pack . prettyPrintParseException) return


-- | Save info about achievements to XDG data directory.
saveAchievementsInfo ::
  FilePath ->
  ScenarioInfo ->
  IO ()
saveAchievementsInfo path si = do
  infoPath <- scenarioPathToSavePath path <$> getSwarmSavePath True
  encodeFile infoPath si

-}