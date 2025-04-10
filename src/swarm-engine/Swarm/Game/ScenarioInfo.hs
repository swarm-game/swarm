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
  ScenarioInfo (..),
  scenarioPath,
  scenarioStatus,
  CodeSizeDeterminators (CodeSizeDeterminators),
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

  -- ** Tutorials
  tutorialsDirname,
  getTutorials,

  -- * Loading and saving scenarios
  loadScenarios,
  loadScenarioInfo,
  saveScenarioInfo,
) where

import Control.Algebra (Has)
import Control.Carrier.Lift (runM)
import Control.Carrier.Throw.Either (runThrow)
import Control.Effect.Accum (Accum, add)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Throw (Throw, liftEither)
import Control.Lens hiding (from, (<.>))
import Control.Monad (filterM, forM_, when, (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Either.Extra (fromRight')
import Data.List (intercalate, isPrefixOf, stripPrefix, (\\))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OM
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Yaml as Y
import Swarm.Failure
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Scoring.CodeSize
import Swarm.Game.Scenario.Status
import Swarm.ResourceLoading (getDataDirSafe, getSwarmSavePath)
import Swarm.Util (lookupEither)
import Swarm.Util.Effect (warn, withThrow)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (pathSeparator, splitDirectories, takeBaseName, takeExtensions, (-<.>), (</>))
import System.IO (readFile')
import System.IO.Error (catchIOError)
import Witch (into)

------------------------------------------------------------

-- * Utilities

-- | Given an ordered list of keys and a map, return a partition consisting of:
-- * Left: the keys that were not present
-- * Right: the retrievable key-value pairs in corresponding order to the provided keys
lookupInOrder :: Ord k => Map k v -> [k] -> ([k], [(k, v)])
lookupInOrder m = partitionEithers . map produceKeyValuePair
 where
  produceKeyValuePair k = sequenceA (k, lookupEither k m)

-- ** Ordered Map utilities

type instance Index (OMap k a) = k
type instance IxValue (OMap k a) = a

-- | Adapted from:
-- https://hackage.haskell.org/package/lens-5.3.4/docs/src/Control.Lens.At.html#line-319
instance Ord k => Ixed (OMap k a) where
  ix k f m = case OM.lookup k m of
    Just v -> f v <&> \v' -> OM.alter (const $ Just v') k m
    Nothing -> pure m

-- | Strangely, an 'elems' function is missing from the 'OMap' API.
orderedElems :: OMap k a -> [a]
orderedElems = map snd . OM.assocs

fromMapOM :: Ord k => Map k a -> OMap k a
fromMapOM = OM.fromList . M.toList

-- ----------------------------------------------------------------------------
-- Scenario Item
-- ----------------------------------------------------------------------------

-- | A scenario item is either a specific scenario, or a collection of
--   scenarios (/e.g./ the scenarios contained in a subdirectory).
data ScenarioItem a = SISingle (ScenarioInfoPair a) | SICollection Text (ScenarioCollection a)
  deriving (Functor)

-- | Retrieve the name of a scenario item.
scenarioItemName :: ScenarioItem a -> Text
scenarioItemName (SISingle (s, _ss)) = s ^. scenarioMetadata . scenarioName
scenarioItemName (SICollection name _) = name

-- | A scenario collection is a tree of scenarios, keyed by name,
--   together with an optional order.
--
--   /Invariant:/ every item in the
--   'scOrder' exists as a key in the 'scMap'.
newtype ScenarioCollection a = SC
  { scMap :: OMap FilePath (ScenarioItem a)
  }
  deriving (Functor)

-- | Access and modify 'ScenarioItem's in collection based on their path.
scenarioItemByPath :: FilePath -> Traversal' (ScenarioCollection a) (ScenarioItem a)
scenarioItemByPath path = ixp ps
 where
  ps = splitDirectories path
  ixp :: (Applicative f) => [String] -> (ScenarioItem a -> f (ScenarioItem a)) -> ScenarioCollection a -> f (ScenarioCollection a)
  ixp [] _ col = pure col
  ixp [s] f (SC m) = SC <$> ix s f m
  ixp (d : xs) f (SC m) = SC <$> ix d inner m
   where
    inner si = case si of
      SISingle {} -> pure si
      SICollection n' col -> SICollection n' <$> ixp xs f col

-- | Subdirectory of the scenarios directory where tutorials are stored.
tutorialsDirname :: FilePath
tutorialsDirname = "Tutorials"

-- | Extract just the collection of tutorial scenarios from the entire
--   scenario collection.
getTutorials :: ScenarioCollection a -> ScenarioCollection a
getTutorials sc = case OM.lookup tutorialsDirname (scMap sc) of
  Just (SICollection _ c) -> c
  _ -> error "No tutorials exist"

-- | Canonicalize a scenario path, making it usable as a unique key.
normalizeScenarioPath ::
  (MonadIO m) =>
  ScenarioCollection a ->
  FilePath ->
  m FilePath
normalizeScenarioPath col p =
  let path = p -<.> "yaml"
   in if isJust $ col ^? scenarioItemByPath path
        then return path
        else liftIO $ do
          canonPath <- canonicalizePath path
          eitherDataDir <- runM . runThrow @SystemFailure $ getDataDirSafe Scenarios "." -- no way we got this far without data directory
          d <- canonicalizePath $ fromRight' eitherDataDir
          let n =
                stripPrefix (d </> "scenarios") canonPath
                  & maybe canonPath (dropWhile (== pathSeparator))
          return n

-- | Convert a scenario collection to a list of scenario items.
scenarioCollectionToList :: ScenarioCollection a -> [ScenarioItem a]
scenarioCollectionToList (SC xs) = orderedElems xs

flatten :: ScenarioItem a -> [ScenarioInfoPair a]
flatten (SISingle p) = [p]
flatten (SICollection _ c) = concatMap flatten $ scenarioCollectionToList c

-- | Load all the scenarios from the scenarios data directory.
loadScenarios ::
  (Has (Accum (Seq SystemFailure)) sig m, Has (Lift IO) sig m) =>
  ScenarioInputs ->
  Bool ->
  m (ScenarioCollection ScenarioInfo)
loadScenarios scenarioInputs loadTestScenarios = do
  res <- runThrow @SystemFailure $ getDataDirSafe Scenarios "scenarios"
  case res of
    Left err -> do
      warn err
      return $ SC OM.empty
    Right dataDir -> loadScenarioDir scenarioInputs loadTestScenarios dataDir

-- | The name of the special file which indicates the order of
--   scenarios in a folder.
orderFileName :: FilePath
orderFileName = "00-ORDER.txt"

testingDirectory :: FilePath
testingDirectory = "Testing"

readOrderFile :: FilePath -> IO (Maybe [String])
readOrderFile orderFile = fmap nonEmptyLines <$> readFileMaybe orderFile
 where
  nonEmptyLines :: String -> [String]
  nonEmptyLines = filter (not . null) . lines
  readFileMaybe :: FilePath -> IO (Maybe String)
  readFileMaybe path = (Just <$> readFile' path) `catchIOError` (\_ -> return Nothing)

loadScenarioDir ::
  forall m sig.
  (Has (Accum (Seq SystemFailure)) sig m, Has (Lift IO) sig m) =>
  ScenarioInputs ->
  Bool ->
  FilePath ->
  m (ScenarioCollection ScenarioInfo)
loadScenarioDir scenarioInputs loadTestScenarios dir = do
  itemPaths <- sendIO $ filterM (isYamlOrPublicDirectory dir) =<< listDirectory dir
  scenarioMap <- loadItems itemPaths
  sendIO (readOrderFile orderFile) >>= \case
    Nothing -> loadUnorderedScenarioDir scenarioMap
    Just order -> loadOrderedScenarioDir order scenarioMap
 where
  dirName, orderFile, orderFileShortPath :: FilePath
  dirName = takeBaseName dir
  orderFile = dir </> orderFileName
  orderFileShortPath = dirName </> orderFileName

  -- The function for individual directory items either warns about SystemFailure,
  -- or has thrown SystemFailure. The following code just adds that thrown failure to others.
  loadItems :: [FilePath] -> m (Map FilePath (ScenarioItem ScenarioInfo))
  loadItems items = do
    let loadItem f = runThrow @SystemFailure $ (f,) <$> loadScenarioItem scenarioInputs loadTestScenarios (dir </> f)
    (scenarioFailures, okScenarios) <- partitionEithers <$> mapM loadItem items
    add (Seq.fromList scenarioFailures)
    return $ M.fromList okScenarios

  isHiddenDir :: String -> Bool
  isHiddenDir f = not loadTestScenarios && f == testingDirectory

  -- Keep only files which are .yaml files or directories not starting with an underscore.
  -- Marked directories contain scenarios that can't be parsed (failure tests) or only script solutions.
  isYamlOrPublicDirectory :: FilePath -> String -> IO Bool
  isYamlOrPublicDirectory d f = do
    isDir <- doesDirectoryExist $ d </> f
    return $
      if isDir
        then not ("_" `isPrefixOf` f || isHiddenDir f)
        else takeExtensions f == ".yaml"

  -- warn that the ORDER file is missing
  loadUnorderedScenarioDir :: Map FilePath (ScenarioItem a) -> m (ScenarioCollection a)
  loadUnorderedScenarioDir scenarioMap = do
    when (dirName /= testingDirectory) (warn $ OrderFileWarning orderFileShortPath NoOrderFile)
    pure . SC $ fromMapOM scenarioMap

  -- warn if the ORDER file does not match directory contents
  loadOrderedScenarioDir :: [String] -> Map FilePath (ScenarioItem a) -> m (ScenarioCollection a)
  loadOrderedScenarioDir order scenarioMap = do
    let missing = M.keys scenarioMap \\ order
        (notPresent, loaded) = lookupInOrder scenarioMap order
        dangling = filter (not . isHiddenDir) notPresent

    forM_ (NE.nonEmpty missing) (warn . OrderFileWarning orderFileShortPath . MissingFiles)
    forM_ (NE.nonEmpty dangling) (warn . OrderFileWarning orderFileShortPath . DanglingFiles)

    pure $ SC $ OM.fromList loaded

-- | How to transform scenario path to save path.
scenarioPathToSavePath :: FilePath -> FilePath -> FilePath
scenarioPathToSavePath path swarmData = swarmData </> Data.List.intercalate "_" (splitDirectories path)

-- | Load saved info about played scenario from XDG data directory.
loadScenarioInfo ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  FilePath ->
  m ScenarioInfo
loadScenarioInfo p = do
  path <- sendIO $ normalizeScenarioPath (SC OM.empty) p
  infoPath <- sendIO $ scenarioPathToSavePath path <$> getSwarmSavePath False
  hasInfo <- sendIO $ doesFileExist infoPath
  if not hasInfo
    then do
      return $
        ScenarioInfo path NotStarted
    else do
      ScenarioInfo _storedPath status <-
        withThrow (AssetNotLoaded (Data Scenarios) infoPath . CanNotParseYaml)
          . (liftEither <=< sendIO)
          $ decodeFileEither infoPath
      -- We discard the path that was saved inside the yaml file, so that there
      -- is only a single authoritative path "key": the original scenario path.
      --
      -- TODO(#2390) Maybe we shouldn't store that path.
      return $ ScenarioInfo path status

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
  ScenarioInputs ->
  Bool ->
  FilePath ->
  m (ScenarioItem ScenarioInfo)
loadScenarioItem scenarioInputs loadTestScenarios path = do
  isDir <- sendIO $ doesDirectoryExist path
  let collectionName = into @Text . dropWhile isSpace . takeBaseName $ path
  case isDir of
    True -> SICollection collectionName <$> loadScenarioDir scenarioInputs loadTestScenarios path
    False -> do
      s <- loadScenarioFile scenarioInputs path
      eitherSi <- runThrow @SystemFailure (loadScenarioInfo path)
      case eitherSi of
        Right si -> return $ SISingle (s, si)
        Left warning -> do
          warn warning
          return $ SISingle (s, ScenarioInfo path NotStarted)

------------------------------------------------------------
-- Some lenses + prisms
------------------------------------------------------------

makePrisms ''ScenarioItem
makePrisms ''ScenarioStatus
