{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Loading + displaying user manual / help system.
module Swarm.Language.Help (
  -- * Help pages
  FamilyLinks (..),
  nextPage,
  prevPage,
  parentPage,
  HelpPage (..),
  helpMetadata,
  helpDoc,
  familyLinks,

  -- * Loading help collection
  loadHelp,
  parseMetadata,
  HelpWarning (..),
) where

import Commonmark.Types (ListSpacing (..), ListType (..))
import Control.Lens (at, both, imap, itraverse_, ix, makeLenses, non, over, toListOf, (&), (.~), (^?))
import Control.Monad (unless)
import Data.Bifunctor (first, second)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Functor (($>))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))
import Data.Tree qualified as Tree
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local (State, evalState, get, modify)
import Swarm.Effect.Warn.Local
import Swarm.Failure (Asset (..), AssetData (Help), Entry (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Language.Syntax (Phase (Raw), Raw, Syntax)
import Swarm.ResourceLoading (Collection, atPath, getDataDirThrow)
import Swarm.ResourceLoading.Collection (
  CollectionConfig (..),
  loadCollection,
 )
import Swarm.Text.Markdown (Document, fromTextE)
import Swarm.Text.Markdown.Document (Document (..), Node (..), Paragraph (..), Target (..), mapDocument, pureP, traverseDocument, traverseParagraph, txt, _TOCTree)
import Swarm.Util (Encoding (UTF8), readFileMayT)
import Swarm.Util.Lens ((<+=))
import Swarm.Util.Traversal qualified as Tree -- TODO(#2808): Replace this import with Data.Tree once we can require containers >= 0.8
import System.Directory (doesDirectoryExist)
import System.FilePath (takeExtension, (</>))

-- | The @FamilyLinks@ of a page are links to its siblings (previous,
--   next) and parent, as determined by the structure of the table of
--   contents.  Repeatedly following "next" performs a preorder
--   traversal of the document tree.
data FamilyLinks = FamilyLinks
  { _prevPage :: Maybe FilePath
  , _nextPage :: Maybe FilePath
  , _parentPage :: Maybe FilePath
  }

emptyFamilyLinks :: FamilyLinks
emptyFamilyLinks = FamilyLinks Nothing Nothing Nothing

makeLenses ''FamilyLinks

-- | A single page in the help collection. Contains a parsed document
--   with arbitrary metadata, and links to sibling and parent
--   documents.
data HelpPage = HelpPage
  { _helpMetadata :: Map Text (Document (Syntax Raw))
  , _helpDoc :: Document (Syntax Raw)
  , _familyLinks :: FamilyLinks
  }

makeLenses ''HelpPage

-- | Configuration used to load help pages with the generic collection loader.
helpCollectionConfig :: CollectionConfig HelpPage
helpCollectionConfig =
  CollectionConfig
    { shouldLoad = \d path -> liftA2 (||) (doesDirectoryExist (d </> path)) (pure $ takeExtension path == ".md")
    , warnUnordered = False
    , loadItem = loadHelpPage
    }

-- | Load a single help page from a path.
loadHelpPage :: FilePath -> IO (Either SystemFailure ([SystemFailure], HelpPage))
loadHelpPage path = do
  readFileMayT UTF8 path >>= \case
    Nothing -> pure . Left $ AssetNotLoaded (Data Help) path (DoesNotExist File)
    Just p -> do
      let (metas, rest) = span (T.isPrefixOf "%") (T.lines p)
          (warns, metadata) = parseMetadata metas
      case fromTextE (T.unlines rest) of
        Left err -> pure . Left $ AssetNotLoaded (Data Help) path (CanNotParse err)
        Right doc -> pure (Right (warns, HelpPage metadata doc emptyFamilyLinks))

-- | Parse metadata fields from a list of lines that start with '%',
--   returning a list of parse failures along with a key-value map
--   representing the parsed metadata fields.
parseMetadata :: [Text] -> ([SystemFailure], Map Text (Document (Syntax Raw)))
parseMetadata = second M.fromList . partitionEithers . map (parseField . stripPunct)
 where
  stripPunct :: Text -> Text
  stripPunct = T.dropWhile isSpace . T.drop 1

  parseField :: Text -> Either SystemFailure (Text, Document (Syntax Raw))
  parseField (T.break (== ':') -> (field, content))
    | T.length content == 0 = Left (CustomFailure $ "Metadata line with no colon: " <> field)
    | otherwise = (field,) <$> first CustomFailure (fromTextE (stripPunct content))

-- | Render all the tables of contents in a document to lists of links.
renderTOCs :: Collection HelpPage -> Document (Syntax Raw) -> Document (Syntax Raw)
renderTOCs help = mapDocument renderTOC
 where
  renderTOC = \case
    TOCTree hps -> ListParagraph (BulletList '*') TightList (map mkTOCEntry hps)
    p -> p

  mkTOCEntry hp = case help ^? atPath (hp <> ".md") . helpMetadata . ix "title" of
    Nothing -> [pureP $ txt (T.pack hp)]
    Just title -> [pureP $ LeafLink (Internal (T.pack (hp <> ".md"))) 0 Nothing (getFirstPara title)]

  getFirstPara = \case
    Document (SimpleParagraph ns : _) -> ns
    _ -> []

-- | Disambiguate links in a document by giving unique ID numbers to
--   links with duplicate targets.  It would probably be simpler
--   to just assign consecutive ID numbers to all the links but this
--   seems conceptually cleaner.
disambiguateLinks :: Document c -> Document c
disambiguateLinks = runPureEff . evalState M.empty . (traverseDocument . traverseParagraph) disambiguate
 where
  disambiguate :: Node c -> Eff '[State (Map Target Int)] (Node c)
  disambiguate = \case
    LeafLink tgt _ t c -> do
      -- Increment the value associated to tgt in the map (default 0) and return the new value
      i <- (at @(Map Target Int) tgt . non 0) <+= 1
      pure $ LeafLink tgt i t c
    n -> pure n

-- | Warnings that are specific to loading the help system.
data HelpWarning
  = -- | Help page occurs twice in the tree, linked from the TOC of two
    --   other help pages.
    Dup FilePath (FilePath, FilePath)
  | -- | Missing f t means help page f, linked from TOC in help page
    --   t, was not found.
    Missing FilePath FilePath
  | -- | Orphan f means help page f exists in the collection but was
    --   not linked transitively from any TOC.
    Orphan FilePath
  deriving (Show)

-- | Return a list of all the table of contents entries on a page, in
--   order of occurrence.
allTOCEntries :: HelpPage -> [FilePath]
allTOCEntries = toListOf $ helpDoc . traverseDocument . _TOCTree . traverse

-- | Build a tree of pages, with the index page at the root and each
--   page having children corresponding to all the pages listed in its
--   table(s) of contents.  Along the way, warn if (1) a page is
--   linked multiple times, (2) a broken link is encountered, or (3)
--   there exist pages not linked from any TOC.
--
--   One might wonder why we need to do this.  Isn't a Collection
--   HelpPage already a tree?  Yes, it is, but it corresponds to the
--   structure of how the help content is stored in the filesystem:
--   each node is a folder, with children being either a file or a
--   nested folder.  The tree we build here takes its structure from
--   the nesting structure of the tables of contents.  The structures
--   may be the same, but they need not be.  For example, suppose
--   there are files A, B, C, D, and E all in the same folder, where
--   A's table of contents contains B and C, and C's table of contents
--   links to D and E.  The Collection will be flat, corresponding to
--   the fact that all the files are in the same folder, whereas the
--   tree output by docTree will have A at the root, B and C as its
--   children, and D and E as children of C.
docTree :: (Warn HelpWarning :> es) => Collection HelpPage -> Eff es (Maybe (Tree (FilePath, HelpPage)))
docTree help = evalState (M.empty :: Map FilePath FilePath) $ do
  t <- dfs "ROOT" "index.md"
  warnOrphans
  pure t
 where
  -- Warn about pages that exist in the collection but were not seen
  -- during the TOC traversal.  Such pages are "orphans" that cannot
  -- be accessed by following table of contents links (though it may
  -- still be possible to access them by following other links).
  warnOrphans :: (State (Map FilePath FilePath) :> es, Warn HelpWarning :> es) => Eff es ()
  warnOrphans = do
    parentOf <- get @(Map FilePath FilePath)
    itraverse_ (\path _ -> unless (M.member path parentOf) (warn $ Orphan path)) help

  dfs :: (State (Map FilePath FilePath) :> es, Warn HelpWarning :> es) => FilePath -> FilePath -> Eff es (Maybe (Tree (FilePath, HelpPage)))
  dfs parent path = do
    -- First, check whether we've already seen this path.  If so,
    -- there is either a directed cycle (which is bad) or the same
    -- page is linked from multiple places, which is a bit strange.
    parentOf <- get @(Map FilePath FilePath)
    case M.lookup path parentOf of
      -- The path has been seen before, linked from 'otherParent'.  Emit a warning and stop processing.
      Just otherParent -> warn (Dup path (otherParent, parent)) $> Nothing
      Nothing -> do
        -- Otherwise, mark the path seen (recording its parent) and proceed to process it.
        modify @(Map FilePath FilePath) $ M.insert path parent
        case help ^? atPath path of
          -- Warn if the path is not found in the collection.
          Nothing -> warn (Missing path parent) $> Nothing
          -- Otherwise, recursively process everything in its TOCs and assemble into a tree.
          Just page -> Just . Tree.Node (path, page) . catMaybes <$> mapM (dfs path . (<> ".md")) (allTOCEntries page)

-- | Elaborate a help collection by computing and caching each page's
--   "family" links (i.e. sibling and parent links based on tables of
--   contents).
computeFamilyLinks :: (Warn HelpWarning :> es) => Collection HelpPage -> Eff es (Collection HelpPage)
computeFamilyLinks help = do
  mHelpTree <- docTree help
  case mHelpTree of
    Nothing -> pure help
    -- Get tree of only paths, discard actual HelpPages since we don't need them
    Just (fmap fst -> helpTree@(Node root _)) -> do
      -- 'Tree.edges' gives us all edges (parent-child pairs) in
      -- preorder.  We also add (Nothing, root) to the beginning so
      -- the list now contains every node in the tree, in preorder,
      -- with each node paired with its parent (if any); then we add
      -- (Nothing, Nothing) sentinels at both ends so that we can
      -- process every node just by taking all size-3 windows.
      let es = (Nothing, Nothing) : (Nothing, Just root) : map (over both Just) (Tree.edges helpTree) ++ [(Nothing, Nothing)]

          buildFamilies :: [(Maybe FilePath, Maybe FilePath)] -> [(FilePath, FamilyLinks)]
          buildFamilies = \case
            ((_, leftSib) : rest@((parent, Just node) : (_, rightSib) : _)) -> (node, FamilyLinks leftSib rightSib parent) : buildFamilies rest
            _ -> []

          familyMap = M.fromList $ buildFamilies es

      -- Rebuild the collection with family links inserted.
      pure (imap (\path page -> page & familyLinks .~ fromMaybe emptyFamilyLinks (familyMap M.!? path)) help)

-- | Elaborate the help collection by
--
--   1. computing + caching "family" (sibling + parent) links based on tables of contents,
--   2. rendering each table of contents as a list, and
--   3. inserting counters to disambiguate duplicate links.
elaborateHelp :: (Warn HelpWarning :> es) => Collection HelpPage -> Eff es (Collection HelpPage)
elaborateHelp help = (fmap . fmap . over helpDoc) (disambiguateLinks . renderTOCs help) . computeFamilyLinks $ help

-- | Load the collection of help pages from the standard location (data/help).
loadHelp ::
  ( Warn SystemFailure :> es
  , Warn HelpWarning :> es
  , Error SystemFailure :> es
  , IOE :> es
  ) =>
  Eff es (Collection HelpPage)
loadHelp = do
  helpFolder <- getDataDirThrow Help "help"
  elaborateHelp =<< loadCollection helpCollectionConfig helpFolder
