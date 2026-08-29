{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Collections
--
-- Nested, tree-structured collections (e.g. of scenarios,
-- documentation, etc.) loaded from a directory hierarchy in a
-- filesystem, with optional 00-ORDER files to indicate the order of
-- items in each subcollection.
module Swarm.ResourceLoading.Collection (
  Collection (..),
  CollectionItem (..),
  _Single,
  _SubCollection,
  emptyCollection,
  collectionItemName,
  collectionToList,
  flattenCollection,
  collectionItemByPath,
  atPath,
  CollectionConfig (..),
  loadCollection,
  loadCollectionConcurrent,
) where

import Control.Lens (Ixed (..), Traversal', makePrisms)
import Control.Monad (filterM, forM_, when)
import Data.List ((\\))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OM
import Data.Text (Text)
import Debug.Trace (traceEventIO)
import Effectful
import Effectful.Concurrent.Async (Concurrent, pooledMapConcurrently, runConcurrent)
import Effectful.Error.Static
import Swarm.Effect.Warn.Local (Warn, warn)
import Swarm.Failure (
  OrderFileWarning (DanglingFiles, MissingFiles, NoOrderFile),
  SystemFailure (OrderFileWarning),
 )
import Swarm.Util (Encoding (UTF8), readFileMay)
import Swarm.Util.Effect (traverseW)
import Swarm.Util.OrderedMap qualified as OM
import System.Directory (
  doesDirectoryExist,
  listDirectory,
 )
import System.FilePath (splitDirectories, takeBaseName, (</>))
import Witch (into)
import Witherable (Filterable, Witherable)
import Witherable qualified as W (Filterable (..), Witherable (..))

-- | A collection of @a@ is a tree, where at each level we map
--   FilePaths to either singleton items of type @a@, or nested
--   subcollections.
newtype Collection a = Collection
  {collectionMap :: OMap FilePath (CollectionItem a)}
  deriving (Functor, Foldable, Traversable)

-- | Either a singleton item, or a nested subcollection with a label.
data CollectionItem a = Single a | SubCollection Text (Collection a)
  deriving (Functor, Foldable, Traversable)

makePrisms ''CollectionItem

instance Filterable Collection where
  catMaybes :: Collection (Maybe a) -> Collection a
  catMaybes (Collection m) = Collection (W.mapMaybe filterItem m)

instance Ord k => Filterable (OMap k) where
  catMaybes :: OMap k (Maybe a) -> OMap k a
  catMaybes = OM.fromList . W.mapMaybe strength . OM.assocs
   where
    strength (k, ma) = (k,) <$> ma

filterItem :: CollectionItem (Maybe a) -> Maybe (CollectionItem a)
filterItem = \case
  Single ma -> Single <$> ma
  SubCollection label c -> Just $ SubCollection label (W.catMaybes c)

instance Witherable Collection

-- | The empty collection with no items.
emptyCollection :: Collection a
emptyCollection = Collection OM.empty

-- | Get the name of a collection item, given a way to extract names
-- from leaves.
collectionItemName :: (a -> Text) -> CollectionItem a -> Text
collectionItemName singleName = \case
  Single a -> singleName a
  SubCollection name _ -> name

-- | Extract the top-level list of items from a collection.
collectionToList :: Collection a -> [CollectionItem a]
collectionToList = OM.elems . collectionMap

-- | Recursively flatten a collection into a list of leaf items.
flattenCollection :: Collection a -> [a]
flattenCollection = concatMap flattenCollectionItem . collectionToList
 where
  flattenCollectionItem :: CollectionItem a -> [a]
  flattenCollectionItem = \case
    Single a -> [a]
    SubCollection _ c -> flattenCollection c

-- | Access and modify 'CollectionItem's based on their path.
collectionItemByPath :: FilePath -> Traversal' (Collection a) (CollectionItem a)
collectionItemByPath path = ixp ps
 where
  ps = splitDirectories path
  ixp :: (Applicative f) => [String] -> (CollectionItem a -> f (CollectionItem a)) -> Collection a -> f (Collection a)
  ixp [] _ col = pure col
  ixp [s] f (Collection m) = Collection <$> ix s f m
  ixp (d : xs) f (Collection m) = Collection <$> ix d inner m
   where
    inner si = case si of
      Single {} -> pure si
      SubCollection n' col -> SubCollection n' <$> ixp xs f col

-- | Traversal to directly access a Single item in a collection at a
--   given path, if it exists.
atPath :: FilePath -> Traversal' (Collection a) a
atPath p = collectionItemByPath p . _Single

-- | The name of the special file which indicates the order of
--   items in a directory.
orderFileName :: FilePath
orderFileName = "00-ORDER.txt"

-- | Read a special 00-ORDER.txt file indicating the order of
--   items in a directory.
readOrderFile :: FilePath -> IO (Maybe [String])
readOrderFile orderFile = fmap nonEmptyLines <$> readFileMay UTF8 orderFile
 where
  nonEmptyLines :: String -> [String]
  nonEmptyLines = filter (not . null) . lines

-- | Configuration record to control the way a collection is loaded.
data CollectionConfig a = CollectionConfig
  { shouldLoad :: FilePath -> FilePath -> IO Bool
  -- ^ Decide whether to load a particular item in a particular folder.
  --   The first parameter is the folder, the second is the path of the item.
  , warnUnordered :: Bool
  -- ^ Should we warn if an 00-ORDER file is missing?
  , loadItem :: FilePath -> IO (Either SystemFailure ([SystemFailure], a))
  -- ^ Function for loading an item from a path.  Can either fail with
  -- a SystemFailure, or return an item along with a list of warnings
  }

loadCollectionConcurrent ::
  forall es a.
  (Warn SystemFailure :> es, IOE :> es) =>
  CollectionConfig a ->
  FilePath ->
  Eff es (Collection a)
loadCollectionConcurrent cfg dir = do
  collectedPaths <- loadCollection cfg {loadItem = loadPathOnly} dir
  eItems <- runConcurrent $ pooledMapConcurrently loadItemE collectedPaths
  traverseW pure eItems
 where
  loadPathOnly :: FilePath -> IO (Either SystemFailure ([SystemFailure], FilePath))
  loadPathOnly p = pure (Right ([], p))
  loadItemE :: FilePath -> Eff (Concurrent : es) (Either SystemFailure a)
  loadItemE fp = pairToWarnAnd =<< liftIO (marked fp $ loadItem cfg fp)
  pairToWarnAnd :: Either SystemFailure ([SystemFailure], a) -> Eff (Concurrent : es) (Either SystemFailure a)
  pairToWarnAnd = \case
    Right (ws, item) -> mapM_ warn ws >> pure (Right item)
    Left e -> pure $ Left e
  marked path a = do
    liftIO $ traceEventIO $ "START load " <> path
    r <- a
    liftIO $ traceEventIO $ "STOP load " <> path
    pure r

-- | Recursively load a collection from a specified folder.  Mutually
--   recursive with 'loadCollectionItem'.
loadCollection ::
  forall es a.
  (Warn SystemFailure :> es, IOE :> es) =>
  CollectionConfig a ->
  FilePath ->
  Eff es (Collection a)
loadCollection cfg dir = do
  itemPaths <- liftIO $ filterM (shouldLoad cfg dir) =<< listDirectory dir
  cMap <- loadItems itemPaths
  liftIO (readOrderFile orderFile) >>= \case
    Nothing -> loadUnorderedCollection cMap
    Just order -> loadOrderedCollection order cMap
 where
  dirName, orderFile, orderFileShortPath :: FilePath
  dirName = takeBaseName dir
  orderFile = dir </> orderFileName
  orderFileShortPath = dirName </> orderFileName

  -- The function for individual directory items either warns about SystemFailure,
  -- or has thrown SystemFailure. The following code just adds that thrown failure to others.
  loadItems :: [FilePath] -> Eff es (Map FilePath (CollectionItem a))
  loadItems items = do
    let loadItem fp = runErrorNoCallStack @SystemFailure $ (fp,) <$> loadCollectionItem cfg (dir </> fp)
    okItems <- traverseW loadItem items
    return $ M.fromList okItems

  -- Load a collection with items sorted alphabetically by file path, and
  -- optionally warn that the ORDER file is missing.
  loadUnorderedCollection :: Map FilePath (CollectionItem a) -> Eff es (Collection a)
  loadUnorderedCollection collectionItemMap = do
    when (warnUnordered cfg) (warn $ OrderFileWarning orderFileShortPath NoOrderFile)
    pure . Collection $ OM.fromMap collectionItemMap

  -- Load an ordered collection, and warn if the ORDER file does not
  -- match the directory contents.
  loadOrderedCollection :: [String] -> Map FilePath (CollectionItem a) -> Eff es (Collection a)
  loadOrderedCollection order collectionItemMap = do
    let missing = M.keys collectionItemMap \\ order
        (notPresent, loaded) = OM.lookupInOrder collectionItemMap order
    dangling <- filterM (liftIO . shouldLoad cfg dir) notPresent
    forM_ (NE.nonEmpty missing) (warn . OrderFileWarning orderFileShortPath . MissingFiles)
    forM_ (NE.nonEmpty dangling) (warn . OrderFileWarning orderFileShortPath . DanglingFiles)

    pure . Collection . OM.fromList $ loaded

-- | Load a collection item from the given path: either a leaf item,
--   or a subcollection.
loadCollectionItem ::
  ( Error SystemFailure :> es
  , Warn SystemFailure :> es
  , IOE :> es
  ) =>
  CollectionConfig a ->
  FilePath ->
  Eff es (CollectionItem a)
loadCollectionItem cfg path = do
  isDir <- liftIO $ doesDirectoryExist path
  let collectionName = into @Text . takeBaseName $ path
  case isDir of
    True -> SubCollection collectionName <$> loadCollection cfg path
    False -> do
      eitherItem <- liftIO $ loadItem cfg path
      case eitherItem of
        Right (ws, item) -> mapM_ warn ws >> pure (Single item)
        Left loadFailure -> throwError loadFailure
