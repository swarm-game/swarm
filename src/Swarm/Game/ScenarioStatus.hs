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
-- Code to save and restore the status of scenarios, i.e. which have been
-- started, which have been completed, etc.
module Swarm.Game.ScenarioStatus (
  ScenarioStatus (..),
  ScenarioInfo,
  scenarioPath,
  scenarioStatus,

  -- * Loading from disk
  ScenarioCollection (..),
  scenarioCollectionToList,
  ScenarioItem (..),
  _SISingle,
  scenarioItemName,
  loadScenarios,

  -- * Re-exports
  module Swarm.Game.Scenario,
) where

import Control.Algebra (Has)
import Control.Carrier.Lift (Lift, sendIO)
import Control.Carrier.Throw.Either (Throw, runThrow)
import Control.Lens hiding (from, (<.>))
import Control.Monad (unless, when)
import Data.Aeson (
  Options (..),
  defaultOptions,
  genericParseJSON,
  genericToEncoding,
 )
import Data.Char (isSpace, toLower)
import Data.Function (on)
import Data.List ((\\))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Time (NominalDiffTime, ZonedTime, zonedTimeToUTC)
import Data.Yaml as Y
import GHC.Generics (Generic)
import Paths_swarm (getDataDir)
import Swarm.Game.Entity
import Swarm.Game.Scenario
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeBaseName, takeExtensions, (</>))
import Witch (into)

instance Eq ZonedTime where
  (==) = (==) `on` zonedTimeToUTC

instance Ord ZonedTime where
  (<=) = (<=) `on` zonedTimeToUTC

data ScenarioStatus
  = NotStarted
  | InProgress
      { -- | Time when the scenario was started including time zone.
        _scenarioStarted :: ZonedTime
      }
  | Complete
      { -- | Time when the scenario was started including time zone.
        _scenarioStarted :: ZonedTime
      , -- | Time elapsed until scenario completion.
        _scenarioElapsed :: NominalDiffTime
      }
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON ScenarioStatus where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON ScenarioStatus where
  toEncoding = genericToEncoding scenarioOptions

data ScenarioInfo = ScenarioInfo
  { _scenarioPath :: FilePath
  , _scenarioStatus :: ScenarioStatus
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON ScenarioInfo where
  parseJSON = genericParseJSON scenarioOptions

instance ToJSON ScenarioInfo where
  toEncoding = genericToEncoding scenarioOptions

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

-- ----------------------------------------------------------------------------
-- Scenario Item
-- ----------------------------------------------------------------------------

-- | A scenario item is either a specific scenario, or a collection of
--   scenarios (*e.g.* the scenarios contained in a subdirectory).
data ScenarioItem = SISingle Scenario | SICollection Text ScenarioCollection

-- | Retrieve the name of a scenario item.
scenarioItemName :: ScenarioItem -> Text
scenarioItemName (SISingle s) = s ^. scenarioName
scenarioItemName (SICollection name _) = name

-- | A scenario collection is a tree of scenarios, keyed by name,
--   together with an optional order.  Invariant: every item in the
--   scOrder exists as a key in the scMap.
data ScenarioCollection = SC
  { scOrder :: Maybe [FilePath]
  , scMap :: Map FilePath ScenarioItem
  }

-- | Convert a scenario collection to a list of scenario items.
scenarioCollectionToList :: ScenarioCollection -> [ScenarioItem]
scenarioCollectionToList (SC Nothing m) = M.elems m
scenarioCollectionToList (SC (Just order) m) = (m M.!) <$> order

-- | Load all the scenarios from the scenarios data directory.
loadScenarios :: (Has (Lift IO) sig m) => EntityMap -> m (Either Text ScenarioCollection)
loadScenarios em = runThrow $ do
  dataDir <- sendIO getDataDir
  loadScenarioDir em (dataDir </> "scenarios")

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
    False -> SISingle <$> loadScenarioFile em path

------------------------------------------------------------
-- Some lenses + prisms
------------------------------------------------------------

makePrisms ''ScenarioItem
