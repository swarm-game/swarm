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
  loadScenarioInfo,
  saveScenarioInfo,

  -- * Re-exports
  module Swarm.Game.Scenario,
) where

import Control.Algebra (Has)
import Control.Carrier.Lift (runM)
import Control.Carrier.Throw.Either (runThrow)
import Control.Effect.Accum (Accum, add)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Throw (Throw, liftEither)
import Control.Lens hiding (from, (<.>))
import Control.Monad (filterM, unless, when, (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Either.Extra (fromRight')
import Data.List (intercalate, isPrefixOf, stripPrefix, (\\))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Yaml as Y
import Swarm.Game.Entity
import Swarm.Game.Failure
import Swarm.Game.ResourceLoading (getDataDirSafe, getSwarmSavePath)
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.Scenario.Status
import Swarm.Util.Effect (withThrow)
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
  ixp :: (Applicative f) => [String] -> (ScenarioItem -> f ScenarioItem) -> ScenarioCollection -> f ScenarioCollection
  ixp [] _ col = pure col
  ixp [s] f (SC n m) = SC n <$> ix s f m
  ixp (d : xs) f (SC n m) = SC n <$> ix d inner m
   where
    inner si = case si of
      SISingle {} -> pure si
      SICollection n' col -> SICollection n' <$> ixp xs f col

-- | Canonicalize a scenario path, making it usable as a unique key.
normalizeScenarioPath ::
  (MonadIO m) =>
  ScenarioCollection ->
  FilePath ->
  m FilePath
normalizeScenarioPath col p =
  let path = p -<.> "yaml"
   in if isJust $ col ^? scenarioItemByPath path
        then return path
        else liftIO $ do
          canonPath <- canonicalizePath path
          eitherDdir <- runM . runThrow @SystemFailure $ getDataDirSafe Scenarios "." -- no way we got this far without data directory
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
  (Has (Accum (Seq SystemFailure)) sig m, Has (Lift IO) sig m) =>
  EntityMap ->
  m ScenarioCollection
loadScenarios em = do
  res <- runThrow @SystemFailure $ getDataDirSafe Scenarios "scenarios"
  case res of
    Left err -> do
      add (Seq.singleton err)
      return $ SC mempty mempty
    Right dataDir -> loadScenarioDir em dataDir

-- | The name of the special file which indicates the order of
--   scenarios in a folder.
orderFileName :: FilePath
orderFileName = "00-ORDER.txt"

readOrderFile :: (Has (Lift IO) sig m) => FilePath -> m [String]
readOrderFile orderFile =
  filter (not . null) . lines <$> sendIO (readFile orderFile)

-- | Recursively load all scenarios from a particular directory, and also load
--   the 00-ORDER file (if any) giving the order for the scenarios.
loadScenarioDir ::
  (Has (Accum (Seq SystemFailure)) sig m, Has (Lift IO) sig m) =>
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
          "Warning: no "
            <> orderFileName
            <> " file found in "
            <> dirName
            <> ", using alphabetical order"
      return Nothing
    True -> Just <$> readOrderFile orderFile
  itemPaths <- sendIO $ keepYamlOrPublicDirectory dir =<< listDirectory dir

  case morder of
    Just order -> do
      let missing = itemPaths \\ order
          dangling = order \\ itemPaths

      unless (null missing) $
        sendIO . putStr . unlines $
          ( "Warning: while processing "
              <> (dirName </> orderFileName)
              <> ": files not listed in "
              <> orderFileName
              <> " will be ignored"
          )
            : map ("  - " <>) missing

      unless (null dangling) $
        sendIO . putStr . unlines $
          ( "Warning: while processing "
              <> (dirName </> orderFileName)
              <> ": nonexistent files will be ignored"
          )
            : map ("  - " <>) dangling
    Nothing -> pure ()

  -- Only keep the files from 00-ORDER.txt that actually exist.
  let morder' = filter (`elem` itemPaths) <$> morder
      loadItem filepath = do
        item <- loadScenarioItem em (dir </> filepath)
        return (filepath, item)
  scenarios <- mapM (runThrow @SystemFailure . loadItem) itemPaths
  let (failures, successes) = partitionEithers scenarios
      scenarioMap = M.fromList successes
      -- Now only keep the files that successfully parsed.
      morder'' = filter (`M.member` scenarioMap) <$> morder'
      collection = SC morder'' scenarioMap
  add (Seq.fromList failures) -- Register failed individual scenarios as warnings
  return collection
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
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  FilePath ->
  m ScenarioInfo
loadScenarioInfo p = do
  path <- sendIO $ normalizeScenarioPath (SC Nothing mempty) p
  infoPath <- sendIO $ scenarioPathToSavePath path <$> getSwarmSavePath False
  hasInfo <- sendIO $ doesFileExist infoPath
  if not hasInfo
    then do
      return $
        ScenarioInfo path NotStarted
    else
      withThrow (AssetNotLoaded (Data Scenarios) infoPath . CanNotParse)
        . (liftEither <=< sendIO)
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
  ( Has (Throw SystemFailure) sig m
  , Has (Accum (Seq SystemFailure)) sig m
  , Has (Lift IO) sig m
  ) =>
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
      eitherSi <- runThrow @SystemFailure (loadScenarioInfo path)
      case eitherSi of
        Right si -> return $ SISingle (s, si)
        Left warning -> do
          add $ Seq.singleton warning
          return $ SISingle (s, ScenarioInfo path NotStarted)

------------------------------------------------------------
-- Some lenses + prisms
------------------------------------------------------------

makePrisms ''ScenarioItem
makePrisms ''ScenarioStatus
