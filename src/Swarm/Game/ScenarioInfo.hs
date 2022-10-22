{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- -Wno-orphans is for the Eq/Ord Time instances

-- |
-- Module      :  Swarm.Game.ScenarioStatus
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Saving and loading info about scenarios (status, path, etc.) as
-- well as loading recursive scenario collections.
module Swarm.Game.ScenarioInfo (
  -- * Scenario info
  ScenarioStatus (..),
  _NotStarted,
  _InProgress,
  _Complete,
  ScenarioInfo (..),
  scenarioPath,
  scenarioStatus,
  scenarioBestTime,
  scenarioBestTicks,
  updateScenarioInfoOnQuit,
  ScenarioInfoPair,

  -- * Scenario collection
  ScenarioCollection (..),
  scenarioCollectionToList,
  scenarioItemByPath,
  normalizeScenarioPath,
  ScenarioItem (..),
  scenarioItemName,
  _SISingle,

  -- * Loading and saving scenarios
  loadScenarios,
  loadScenarioInfo,
  saveScenarioInfo,

  -- * Re-exports
  module Swarm.Game.Scenario,
) where

import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, sendIO)
import Control.Carrier.Throw.Either (Throw, runThrow, throwError)
import Control.Lens hiding (from, (<.>))
import Control.Monad (unless, when)
import Data.Aeson (
  Options (..),
  defaultOptions,
  genericParseJSON,
  genericToEncoding,
  genericToJSON,
 )
import Data.Char (isSpace, toLower)
import Data.Function (on)
import Data.List (intercalate, stripPrefix, (\\))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Text (Text, pack)
import Data.Time (NominalDiffTime, ZonedTime, diffUTCTime, zonedTimeToUTC)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Swarm.Game.Entity
import Swarm.Game.Scenario
import Swarm.Util (dataNotFound, getDataDirSafe, getSwarmSavePath)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (pathSeparator, splitDirectories, takeBaseName, takeExtensions, (-<.>), (</>))
import Witch (into)

-- Some orphan ZonedTime instances

instance Eq ZonedTime where
  (==) = (==) `on` zonedTimeToUTC

instance Ord ZonedTime where
  (<=) = (<=) `on` zonedTimeToUTC

-- | A @ScenarioStatus@ stores the status of a scenario along with
--   appropriate metadata: not started, in progress, or complete.
--   Note that "in progress" is currently a bit of a misnomer since
--   games cannot be saved; at the moment it really means more like
--   "you played this scenario before but didn't win".
data ScenarioStatus
  = NotStarted
  | InProgress
      { -- | Time when the scenario was started including time zone.
        _scenarioStarted :: ZonedTime
      , -- | Time elapsed until quitting the scenario.
        _scenarioElapsed :: NominalDiffTime
      , -- | Ticks elapsed until quitting the scenario.
        _scenarioElapsedTicks :: Integer
      }
  | Complete
      { -- | Time when the scenario was started including time zone.
        _scenarioStarted :: ZonedTime
      , -- | Time elapsed until quitting the scenario.
        _scenarioElapsed :: NominalDiffTime
      , -- | Ticks elapsed until quitting the scenario.
        _scenarioElapsedTicks :: Integer
      }
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON ScenarioStatus where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON ScenarioStatus where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

-- | A @ScenarioInfo@ record stores metadata about a scenario: its
--   canonical path, most recent status, and best-ever status.
data ScenarioInfo = ScenarioInfo
  { _scenarioPath :: FilePath
  , _scenarioStatus :: ScenarioStatus
  , _scenarioBestTime :: ScenarioStatus
  , _scenarioBestTicks :: ScenarioStatus
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON ScenarioInfo where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON ScenarioInfo where
  toEncoding = genericToEncoding scenarioOptions
  toJSON = genericToJSON scenarioOptions

type ScenarioInfoPair = (Scenario, ScenarioInfo)

scenarioOptions :: Options
scenarioOptions =
  defaultOptions
    { fieldLabelModifier = map toLower . drop (length "_scenario")
    }

makeLensesWith (lensRules & generateSignatures .~ False) ''ScenarioInfo

-- | The path of the scenario, relative to @data/scenarios@.
scenarioPath :: Lens' ScenarioInfo FilePath

-- | The status of the scenario.
scenarioStatus :: Lens' ScenarioInfo ScenarioStatus

-- | The best status of the scenario, measured in real world time.
scenarioBestTime :: Lens' ScenarioInfo ScenarioStatus

-- | The best status of the scenario, measured in game ticks.
scenarioBestTicks :: Lens' ScenarioInfo ScenarioStatus

-- | Update the current @ScenarioInfo@ record when quitting a game.
--
-- Note that when comparing "best" times, shorter is not always better!
-- As long as the scenario is not completed (e.g. some do not have win condition)
-- we consider having fun _longer_ to be better.
updateScenarioInfoOnQuit :: ZonedTime -> Integer -> Bool -> ScenarioInfo -> ScenarioInfo
updateScenarioInfoOnQuit z ticks completed (ScenarioInfo p s bTime bTicks) = case s of
  InProgress start _ _ ->
    let el = (diffUTCTime `on` zonedTimeToUTC) z start
        cur = (if completed then Complete else InProgress) start el ticks
        best f b = case b of
          Complete {} | not completed || f b <= f cur -> b -- keep faster completed
          InProgress {} | not completed && f b > f cur -> b -- keep longer progress (fun!)
          _ -> cur -- otherwise update with current
     in ScenarioInfo p cur (best _scenarioElapsed bTime) (best _scenarioElapsedTicks bTicks)
  _ -> error "Logical error: trying to quit scenario which is not in progress!"

-- ----------------------------------------------------------------------------
-- Scenario Item
-- ----------------------------------------------------------------------------

-- | A scenario item is either a specific scenario, or a collection of
--   scenarios (*e.g.* the scenarios contained in a subdirectory).
data ScenarioItem = SISingle ScenarioInfoPair | SICollection Text ScenarioCollection
  deriving (Eq, Show)

-- | Retrieve the name of a scenario item.
scenarioItemName :: ScenarioItem -> Text
scenarioItemName (SISingle (s, _ss)) = s ^. scenarioName
scenarioItemName (SICollection name _) = name

-- | A scenario collection is a tree of scenarios, keyed by name,
--   together with an optional order.  Invariant: every item in the
--   scOrder exists as a key in the scMap.
data ScenarioCollection = SC
  { scOrder :: Maybe [FilePath]
  , scMap :: Map FilePath ScenarioItem
  }
  deriving (Eq, Show)

-- | Access and modify ScenarioItems in collection based on their path.
scenarioItemByPath :: FilePath -> Traversal' ScenarioCollection ScenarioItem
scenarioItemByPath path = ixp ps
 where
  ps = splitDirectories path
  ixp :: Applicative f => [String] -> (ScenarioItem -> f ScenarioItem) -> ScenarioCollection -> f ScenarioCollection
  ixp [] _ col = pure col
  ixp [s] f (SC n m) = SC n <$> ix s f m
  ixp (d : xs) f (SC n m) = SC n <$> ix d inner m
   where
    inner si = case si of
      SISingle {} -> pure si
      SICollection n' col -> SICollection n' <$> ixp xs f col

-- | Canonicalize a scenario path, making it usable as a unique key.
normalizeScenarioPath :: ScenarioCollection -> FilePath -> IO FilePath
normalizeScenarioPath col p =
  let path = p -<.> "yaml"
   in if isJust $ col ^? scenarioItemByPath path
        then return path
        else do
          canonPath <- canonicalizePath path
          Just ddir <- getDataDirSafe "." -- no way we got this far without data directory
          d <- canonicalizePath ddir
          let n =
                stripPrefix (d </> "scenarios") canonPath
                  & maybe canonPath (dropWhile (== pathSeparator))
          return n

-- | Convert a scenario collection to a list of scenario items.
scenarioCollectionToList :: ScenarioCollection -> [ScenarioItem]
scenarioCollectionToList (SC Nothing m) = M.elems m
scenarioCollectionToList (SC (Just order) m) = (m M.!) <$> order

-- | Load all the scenarios from the scenarios data directory.
loadScenarios :: (Has (Lift IO) sig m) => EntityMap -> m (Either Text ScenarioCollection)
loadScenarios em = runThrow $ do
  let p = "scenarios"
  mdataDir <- sendIO $ getDataDirSafe p
  case mdataDir of
    Nothing -> sendIO (dataNotFound p) >>= throwError
    Just dataDir -> loadScenarioDir em dataDir

-- | The name of the special file which indicates the order of
--   scenarios in a folder.
orderFileName :: FilePath
orderFileName = "00-ORDER.txt"

-- | Recursively load all scenarios from a particular directory, and also load
--   the 00-ORDER file (if any) giving the order for the scenarios.
loadScenarioDir ::
  (Has (Lift IO) sig m, Has (Throw Text) sig m) =>
  EntityMap ->
  FilePath ->
  m ScenarioCollection
loadScenarioDir em dir = do
  let orderFile = dir </> orderFileName
      dirName = takeBaseName dir
  orderExists <- sendIO $ doesFileExist orderFile
  morder <- case orderExists of
    False -> do
      when (dirName /= "Testing") $
        sendIO . putStrLn $
          "Warning: no " <> orderFileName <> " file found in " <> dirName
            <> ", using alphabetical order"
      return Nothing
    True -> Just . filter (not . null) . lines <$> sendIO (readFile orderFile)
  fs <- sendIO $ keepYamlOrDirectory <$> listDirectory dir

  case morder of
    Just order -> do
      let missing = fs \\ order
          dangling = order \\ fs

      unless (null missing) $
        sendIO . putStr . unlines $
          ( "Warning: while processing " <> (dirName </> orderFileName) <> ": files not listed in "
              <> orderFileName
              <> " will be ignored"
          ) :
          map ("  - " <>) missing

      unless (null dangling) $
        sendIO . putStr . unlines $
          ( "Warning: while processing " <> (dirName </> orderFileName)
              <> ": nonexistent files will be ignored"
          ) :
          map ("  - " <>) dangling
    Nothing -> pure ()

  -- Only keep the files from 00-ORDER.txt that actually exist.
  let morder' = filter (`elem` fs) <$> morder
  SC morder' . M.fromList <$> mapM (\item -> (item,) <$> loadScenarioItem em (dir </> item)) fs
 where
  keepYamlOrDirectory = filter (\f -> takeExtensions f `elem` ["", ".yaml"])

-- | How to transform scenario path to save path.
scenarioPathToSavePath :: FilePath -> FilePath -> FilePath
scenarioPathToSavePath path swarmData = swarmData </> Data.List.intercalate "_" (splitDirectories path)

-- | Load saved info about played scenario from XDG data directory.
loadScenarioInfo ::
  (Has (Lift IO) sig m, Has (Throw Text) sig m) =>
  FilePath ->
  m ScenarioInfo
loadScenarioInfo p = do
  path <- sendIO $ normalizeScenarioPath (SC Nothing mempty) p
  infoPath <- sendIO $ scenarioPathToSavePath path <$> getSwarmSavePath False
  hasInfo <- sendIO $ doesFileExist infoPath
  if not hasInfo
    then do
      return $ ScenarioInfo path NotStarted NotStarted NotStarted
    else
      sendIO (decodeFileEither infoPath)
        >>= either (throwError . pack . prettyPrintParseException) return

-- | Save info about played scenario to XDG data directory.
saveScenarioInfo ::
  FilePath ->
  ScenarioInfo ->
  IO ()
saveScenarioInfo path si = do
  infoPath <- scenarioPathToSavePath path <$> getSwarmSavePath True
  encodeFile infoPath si

-- | Load a scenario item (either a scenario, or a subdirectory
--   containing a collection of scenarios) from a particular path.
loadScenarioItem ::
  (Has (Lift IO) sig m, Has (Throw Text) sig m) =>
  EntityMap ->
  FilePath ->
  m ScenarioItem
loadScenarioItem em path = do
  isDir <- sendIO $ doesDirectoryExist path
  let collectionName = into @Text . dropWhile isSpace . takeBaseName $ path
  case isDir of
    True -> SICollection collectionName <$> loadScenarioDir em path
    False -> do
      s <- loadScenarioFile em path
      si <- loadScenarioInfo path
      return $ SISingle (s, si)

------------------------------------------------------------
-- Some lenses + prisms
------------------------------------------------------------

makePrisms ''ScenarioItem
makePrisms ''ScenarioStatus
