{-# LANGUAGE TemplateHaskell #-}

-- -Wno-orphans is for the Eq/Ord Time instances

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Saving and loading info about scenarios (status, path, etc.) as
-- well as loading recursive scenario collections.
module Swarm.Game.ScenarioInfo (
  -- * Scenario info
  ScenarioStatus (..),
  _NotStarted,
  ScenarioInfo (..),
  scenarioPath,
  scenarioStatus,
  scenarioBestByTime,
  scenarioBestByTicks,
  scenarioBestByCharCount,
  scenarioBestByAstSize,
  CodeSizeDeterminators (CodeSizeDeterminators),
  updateScenarioInfoOnFinish,
  ScenarioInfoPair,

  -- * Scenario collection
  ScenarioCollection (..),
  scenarioCollectionToList,
  flatten,
  scenarioItemByPath,
  normalizeScenarioPath,
  ScenarioItem (..),
  scenarioItemName,
  _SISingle,

  -- * Loading and saving scenarios
  loadScenarios,
  loadScenariosWithWarnings,
  loadScenarioInfo,
  saveScenarioInfo,

  -- * Re-exports
  module Swarm.Game.Scenario,
) where

import Control.Lens hiding (from, (<.>))
import Control.Monad (filterM, unless, when)
import Control.Monad.Except (ExceptT (..), MonadIO, liftIO, runExceptT, withExceptT)
import Data.Aeson (
  Options (..),
  defaultOptions,
  genericParseJSON,
  genericToEncoding,
  genericToJSON,
 )
import Data.Char (isSpace, toLower)
import Data.Either.Extra (fromRight')
import Data.Function (on)
import Data.List (intercalate, isPrefixOf, stripPrefix, (\\))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Yaml as Y
import Swarm.Game.CESK (TickNumber)
import Swarm.Game.Entity
import Swarm.Game.Failure
import Swarm.Game.ResourceLoading (getDataDirSafe, getSwarmSavePath)
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.Scenario.Status
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (pathSeparator, splitDirectories, takeBaseName, takeExtensions, (-<.>), (</>))
import Witch (into)

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
normalizeScenarioPath ::
  MonadIO m =>
  ScenarioCollection ->
  FilePath ->
  m FilePath
normalizeScenarioPath col p =
  let path = p -<.> "yaml"
   in if isJust $ col ^? scenarioItemByPath path
        then return path
        else liftIO $ do
          canonPath <- canonicalizePath path
          eitherDdir <- getDataDirSafe Scenarios "." -- no way we got this far without data directory
          d <- canonicalizePath $ fromRight' eitherDdir
          let n =
                stripPrefix (d </> "scenarios") canonPath
                  & maybe canonPath (dropWhile (== pathSeparator))
          return n

-- | Convert a scenario collection to a list of scenario items.
scenarioCollectionToList :: ScenarioCollection -> [ScenarioItem]
scenarioCollectionToList (SC Nothing m) = M.elems m
scenarioCollectionToList (SC (Just order) m) = (m M.!) <$> order

flatten :: ScenarioItem -> [ScenarioInfoPair]
flatten (SISingle p) = [p]
flatten (SICollection _ c) = concatMap flatten $ scenarioCollectionToList c

-- | Load all the scenarios from the scenarios data directory.
loadScenarios ::
  EntityMap ->
  ExceptT [SystemFailure] IO ([SystemFailure], ScenarioCollection)
loadScenarios em = do
  dataDir <- withExceptT pure $ ExceptT $ getDataDirSafe Scenarios p
  loadScenarioDir em dataDir
 where
  p = "scenarios"

loadScenariosWithWarnings :: EntityMap -> IO ([SystemFailure], ScenarioCollection)
loadScenariosWithWarnings entities = do
  eitherLoadedScenarios <- runExceptT $ loadScenarios entities
  return $ case eitherLoadedScenarios of
    Left xs -> (xs, SC mempty mempty)
    Right (warnings, x) -> (warnings, x)

-- | The name of the special file which indicates the order of
--   scenarios in a folder.
orderFileName :: FilePath
orderFileName = "00-ORDER.txt"

readOrderFile ::
  MonadIO m =>
  FilePath ->
  ExceptT [SystemFailure] m [String]
readOrderFile orderFile =
  filter (not . null) . lines <$> liftIO (readFile orderFile)

-- | Recursively load all scenarios from a particular directory, and also load
--   the 00-ORDER file (if any) giving the order for the scenarios.
loadScenarioDir ::
  MonadIO m =>
  EntityMap ->
  FilePath ->
  ExceptT [SystemFailure] m ([SystemFailure], ScenarioCollection)
loadScenarioDir em dir = do
  let orderFile = dir </> orderFileName
      dirName = takeBaseName dir
  orderExists <- liftIO $ doesFileExist orderFile
  morder <- case orderExists of
    False -> do
      when (dirName /= "Testing") $
        liftIO . putStrLn $
          "Warning: no "
            <> orderFileName
            <> " file found in "
            <> dirName
            <> ", using alphabetical order"
      return Nothing
    True -> Just <$> readOrderFile orderFile
  fs <- liftIO $ keepYamlOrPublicDirectory dir =<< listDirectory dir

  case morder of
    Just order -> do
      let missing = fs \\ order
          dangling = order \\ fs

      unless (null missing) $
        liftIO . putStr . unlines $
          ( "Warning: while processing "
              <> (dirName </> orderFileName)
              <> ": files not listed in "
              <> orderFileName
              <> " will be ignored"
          )
            : map ("  - " <>) missing

      unless (null dangling) $
        liftIO . putStr . unlines $
          ( "Warning: while processing "
              <> (dirName </> orderFileName)
              <> ": nonexistent files will be ignored"
          )
            : map ("  - " <>) dangling
    Nothing -> pure ()

  -- Only keep the files from 00-ORDER.txt that actually exist.
  let morder' = filter (`elem` fs) <$> morder
  let f filepath = do
        (warnings, item) <- loadScenarioItem em (dir </> filepath)
        return (warnings, (filepath, item))
  warningsAndScenarios <- mapM f fs
  let (allWarnings, allPairs) = unzip warningsAndScenarios
      collection = SC morder' . M.fromList $ allPairs
  return (concat allWarnings, collection)
 where
  -- Keep only files which are .yaml files or directories that start
  -- with something other than an underscore.
  keepYamlOrPublicDirectory = filterM . isCatalogEntry

  -- Whether the directory or file should be included in the scenario catalog.
  isCatalogEntry d f = do
    isDir <- doesDirectoryExist $ d </> f
    return $
      if isDir
        then not $ "_" `isPrefixOf` f
        else takeExtensions f == ".yaml"

-- | How to transform scenario path to save path.
scenarioPathToSavePath :: FilePath -> FilePath -> FilePath
scenarioPathToSavePath path swarmData = swarmData </> Data.List.intercalate "_" (splitDirectories path)

-- | Load saved info about played scenario from XDG data directory.
loadScenarioInfo ::
  MonadIO m =>
  FilePath ->
  ExceptT [SystemFailure] m ScenarioInfo
loadScenarioInfo p = do
  path <- liftIO $ normalizeScenarioPath (SC Nothing mempty) p
  infoPath <- liftIO $ scenarioPathToSavePath path <$> getSwarmSavePath False
  hasInfo <- liftIO $ doesFileExist infoPath
  if not hasInfo
    then do
      return $
        ScenarioInfo path NotStarted
    else
      withExceptT (pure . AssetNotLoaded (Data Scenarios) infoPath . CanNotParse)
        . ExceptT
        . liftIO
        $ decodeFileEither infoPath

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
  MonadIO m =>
  EntityMap ->
  FilePath ->
  ExceptT [SystemFailure] m ([SystemFailure], ScenarioItem)
loadScenarioItem em path = do
  isDir <- liftIO $ doesDirectoryExist path
  let collectionName = into @Text . dropWhile isSpace . takeBaseName $ path
  case isDir of
    True -> do
      (warnings, d) <- loadScenarioDir em path
      return (warnings, SICollection collectionName d)
    False -> do
      s <- withExceptT pure $ loadScenarioFile em path
      eitherSi <- runExceptT $ loadScenarioInfo path
      return $ case eitherSi of
        Right si -> ([], SISingle (s, si))
        Left warnings -> (warnings, SISingle (s, ScenarioInfo path NotStarted NotStarted NotStarted))

------------------------------------------------------------
-- Some lenses + prisms
------------------------------------------------------------

makePrisms ''ScenarioItem
makePrisms ''ScenarioStatus
