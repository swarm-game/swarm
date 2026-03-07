{-# LANGUAGE TemplateHaskell #-}

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
) where

import Control.Algebra (Has)
import Control.Carrier.Error.Either (runError)
import Control.Effect.Accum (Accum, add)
import Control.Effect.Error (Error)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Throw (throwError)
import Control.Lens (Ixed (..), Traversal', makePrisms)
import Control.Monad (filterM, forM_, when)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.List ((\\))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OM
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Swarm.Failure (
  OrderFileWarning (DanglingFiles, MissingFiles, NoOrderFile),
  SystemFailure (OrderFileWarning),
 )
import Swarm.Util (Encoding (UTF8), readFileMay)
import Swarm.Util.Effect (warn)
import Swarm.Util.OrderedMap qualified as OM
import System.Directory (
  doesDirectoryExist,
  listDirectory,
 )
import System.FilePath (splitDirectories, takeBaseName, (</>))
import Witch (into)

-- | A collection of @a@ is a tree, where at each level we map
--   FilePaths to either singleton items of type @a@, or nested
--   subcollections.
newtype Collection a = Collection
  {collectionMap :: OMap FilePath (CollectionItem a)}
  deriving (Functor)

-- | Either a singleton item, or a nested subcollection with a label.
data CollectionItem a = Single a | SubCollection Text (Collection a)
  deriving (Functor)

makePrisms ''CollectionItem

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
--   scenarios in a folder.
orderFileName :: FilePath
orderFileName = "00-ORDER.txt"

-- | Read a special 00-ORDER.txt file indicating the order of
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

-- | Recursively load a collection from a specified folder.  Mutually
--   recursive with 'loadCollectionItem'.
loadCollection ::
  forall sig m a.
  (Has (Accum (Seq SystemFailure)) sig m, Has (Lift IO) sig m) =>
  CollectionConfig a ->
  FilePath ->
  m (Collection a)
loadCollection cfg dir = do
  itemPaths <- sendIO $ filterM (shouldLoad cfg dir) =<< listDirectory dir
  cMap <- loadItems itemPaths
  sendIO (readOrderFile orderFile) >>= \case
    Nothing -> loadUnorderedCollection cMap
    Just order -> loadOrderedCollection order cMap
 where
  dirName, orderFile, orderFileShortPath :: FilePath
  dirName = takeBaseName dir
  orderFile = dir </> orderFileName
  orderFileShortPath = dirName </> orderFileName

  -- The function for individual directory items either warns about SystemFailure,
  -- or has thrown SystemFailure. The following code just adds that thrown failure to others.
  loadItems :: [FilePath] -> m (Map FilePath (CollectionItem a))
  loadItems items = do
    let loadItem f = runError @SystemFailure $ (f,) <$> loadCollectionItem cfg (dir </> f)
    (itemFailures, okItems) <- partitionEithers <$> mapM loadItem items
    add (Seq.fromList itemFailures)
    return $ M.fromList okItems

  -- Load a collection with items in no particular order, and
  -- optionally warn that the ORDER file is missing.
  loadUnorderedCollection :: Map FilePath (CollectionItem a) -> m (Collection a)
  loadUnorderedCollection collectionItemMap = do
    when (warnUnordered cfg) (warn $ OrderFileWarning orderFileShortPath NoOrderFile)
    pure . Collection $ OM.fromMap collectionItemMap

  -- Load an ordered collection, and warn if the ORDER file does not
  -- match the directory contents.
  loadOrderedCollection :: [String] -> Map FilePath (CollectionItem a) -> m (Collection a)
  loadOrderedCollection order collectionItemMap = do
    let missing = M.keys collectionItemMap \\ order
        (notPresent, loaded) = OM.lookupInOrder collectionItemMap order
    dangling <- filterM (sendIO . shouldLoad cfg dir) notPresent
    forM_ (NE.nonEmpty missing) (warn . OrderFileWarning orderFileShortPath . MissingFiles)
    forM_ (NE.nonEmpty dangling) (warn . OrderFileWarning orderFileShortPath . DanglingFiles)

    pure . Collection . OM.fromList $ loaded

-- | Load a collection item from the given path: either a leaf item,
--   or a subcollection.
loadCollectionItem ::
  ( Has (Error SystemFailure) sig m
  , Has (Accum (Seq SystemFailure)) sig m
  , Has (Lift IO) sig m
  ) =>
  CollectionConfig a ->
  FilePath ->
  m (CollectionItem a)
loadCollectionItem cfg path = do
  isDir <- sendIO $ doesDirectoryExist path
  let collectionName = into @Text . dropWhile isSpace . takeBaseName $ path
  case isDir of
    True -> SubCollection collectionName <$> loadCollection cfg path
    False -> do
      eitherItem <- sendIO $ loadItem cfg path
      case eitherItem of
        Right (ws, item) -> mapM_ warn ws >> pure (Single item)
        Left loadFailure -> throwError loadFailure
