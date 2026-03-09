{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Saving and loading info about scenarios (status, path, etc.) as
-- well as loading recursive scenario collections.
module Swarm.Game.ScenarioInfo (
  -- * Scenario info
  ScenarioStatus (..),
  _NotStarted,
  ScenarioInfo,
  scenarioPath,
  normalizeScenarioPath,
  scenarioStatus,
  CodeSizeDeterminators (CodeSizeDeterminators),
  ScenarioWith,

  -- * Scenario collection
  ScenarioCollection,
  ScenarioItem,
  pathify,
  scenarioItemName,

  -- ** Tutorials
  tutorialsDirname,
  getTutorials,

  -- * Loading and saving scenarios
  loadScenarios,
  loadScenarioInfo,
  saveScenarioInfo,
) where

import Control.Algebra (Has)
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Lift (runM)
import Control.Carrier.Throw.Either (runThrow)
import Control.Effect.Accum (Accum)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Throw (Throw, liftEither)
import Control.Lens hiding (from, (<.>))
import Control.Monad (void, (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either.Extra (fromRight')
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.Map.Ordered qualified as OM
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Yaml as Y
import Swarm.Failure
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.Scenario.Status
import Swarm.ResourceLoading
import Swarm.Util.Effect (warn, withThrow)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath (pathSeparator, splitDirectories, takeExtensions, (-<.>), (</>))

------------------------------------------------------------
-- Scenario collection utilities
------------------------------------------------------------

type ScenarioCollection a = Collection (ScenarioWith a)
type ScenarioItem a = CollectionItem (ScenarioWith a)

pathify :: ScenarioInfo -> ScenarioPath
pathify = ScenarioPath . view scenarioPath

-- | Get the name of a scenario item, suitable for e.g. displaying in
--   a menu (either a specific scenario, or a subcollection).
scenarioItemName :: ScenarioItem a -> Text
scenarioItemName = collectionItemName name
 where
  name (ScenarioWith s _) = s ^. scenarioMetadata . scenarioName

-- | Subdirectory of the scenarios directory where tutorials are stored.
tutorialsDirname :: FilePath
tutorialsDirname = "Tutorials"

-- | Extract just the collection of tutorial scenarios from the entire
--   scenario collection.
getTutorials :: ScenarioCollection a -> ScenarioCollection a
getTutorials sc = case OM.lookup tutorialsDirname (collectionMap sc) of
  Just (SubCollection _ c) -> c
  _ -> emptyCollection

-- | Canonicalize a scenario path, making it usable as a unique key.
normalizeScenarioPath ::
  (MonadIO m) =>
  ScenarioCollection a ->
  FilePath ->
  m FilePath
normalizeScenarioPath col p =
  let path = p -<.> "yaml"
   in if isJust $ col ^? collectionItemByPath path
        then return path
        else liftIO $ do
          canonPath <- canonicalizePath path
          eitherDataDir <- runM . runThrow @SystemFailure $ getDataDirThrow Scenarios "." -- no way we got this far without data directory
          d <- canonicalizePath $ fromRight' eitherDataDir
          let n =
                stripPrefix (d </> "scenarios") canonPath
                  & maybe canonPath (dropWhile (== pathSeparator))
          return n

------------------------------------------------------------
-- Scenario collection loading
------------------------------------------------------------

-- | Load all the scenarios from the scenarios data directory.
loadScenarios ::
  (Has (Accum (Seq SystemFailure)) sig m, Has (Lift IO) sig m) =>
  ScenarioInputs ->
  Bool ->
  m (ScenarioCollection ScenarioInfo)
loadScenarios scenarioInputs loadTestScenarios = do
  res <- runThrow @SystemFailure $ getDataDirThrow Scenarios "scenarios"
  case res of
    Left err -> warn err >> pure emptyCollection
    Right dataDir -> loadCollection (scenarioCollectionConfig scenarioInputs loadTestScenarios) dataDir

-- | Configuration record for recursively loading the scenario collection.
scenarioCollectionConfig :: ScenarioInputs -> Bool -> CollectionConfig (ScenarioWith ScenarioInfo)
scenarioCollectionConfig scenarioInputs loadTestScenarios =
  CollectionConfig
    { shouldLoad = isYamlOrPublicDirectory
    , warnUnordered = True
    , loadItem = loadScenarioItem scenarioInputs
    }
 where
  -- Keep only files which are .yaml files or directories not starting
  -- with an underscore.  Marked directories contain scenarios that
  -- can't be parsed (failure tests) or only script solutions.
  isYamlOrPublicDirectory :: FilePath -> FilePath -> IO Bool
  isYamlOrPublicDirectory d f = do
    isDir <- doesDirectoryExist $ d </> f
    return $
      if isDir
        then not ("_" `isPrefixOf` f || isHiddenDir f)
        else takeExtensions f == ".yaml"

  isHiddenDir :: String -> Bool
  isHiddenDir f = not loadTestScenarios && f == "Testing"

-- | Load a single scenario from a path, returning either a loading
--   error, or a scenario along with a list of warnings.
loadScenarioItem :: ScenarioInputs -> FilePath -> IO (Either SystemFailure ([SystemFailure], ScenarioWith ScenarioInfo))
loadScenarioItem scenarioInputs path = runError @SystemFailure $ do
  s <- loadScenarioFile scenarioInputs path
  eitherSi <- runThrow @SystemFailure (loadScenarioInfo path)
  case eitherSi of
    Right si -> pure ([], ScenarioWith s si)
    Left warning -> pure ([warning], ScenarioWith s $ ScenarioInfo path NotStarted)

-- | How to transform scenario path to save path.
scenarioPathToSavePath :: FilePath -> FilePath -> FilePath
scenarioPathToSavePath path swarmData = swarmData </> Data.List.intercalate "_" (splitDirectories path)

-- | Load saved info about played scenario from XDG data directory.
loadScenarioInfo ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  FilePath ->
  m ScenarioInfo
loadScenarioInfo p = do
  path <- sendIO $ normalizeScenarioPath (Collection OM.empty) p
  infoPath <- sendIO $ scenarioPathToSavePath path <$> getSwarmSavePath False
  hasInfo <- sendIO $ doesFileExist infoPath
  if not hasInfo
    then do
      return $
        ScenarioInfo path NotStarted
    else do
      si <-
        withThrow (AssetNotLoaded (Data Scenarios) infoPath . CanNotParseYaml)
          . (liftEither <=< sendIO)
          $ decodeFileEither infoPath
      -- We overwrite the (void) path that was saved inside the yaml file, so that there
      -- is only a single authoritative path "key": the original scenario path.
      return $ path <$ (si :: ScenarioInfoT ())

-- | Save info about played scenario to XDG data directory.
saveScenarioInfo ::
  FilePath ->
  ScenarioInfo ->
  IO ()
saveScenarioInfo path si = do
  infoPath <- scenarioPathToSavePath path <$> getSwarmSavePath True
  -- We do not store the path in the save file (see #2390).
  encodeFile infoPath $ void si

------------------------------------------------------------
-- Some lenses + prisms
------------------------------------------------------------

makePrisms ''ScenarioStatus
