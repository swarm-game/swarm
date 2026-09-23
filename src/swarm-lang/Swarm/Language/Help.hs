{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Loading + displaying user manual / help system.
module Swarm.Language.Help (
  -- * Help pages
  HelpPage (..),
  helpMetadata,
  helpDoc,

  -- * Loading help collection
  loadHelp,
  parseMetadata,
) where

import Commonmark.Types (ListSpacing (..), ListType (..))
import Control.Lens (at, ix, makeLenses, non, over, (^?))
import Data.Bifunctor (first, second)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Swarm.Effect.Warn.Local
import Swarm.Failure (Asset (..), AssetData (Help), Entry (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Language.Syntax (Phase (Raw), Raw, Syntax)
import Swarm.ResourceLoading (Collection, atPath, getDataDirThrow)
import Swarm.ResourceLoading.Collection (
  CollectionConfig (..),
  loadCollection,
 )
import Swarm.Text.Markdown (Document, fromTextE)
import Swarm.Text.Markdown.Document (Document (..), Node (..), Paragraph (..), Target (..), mapDocument, pureP, traverseDocument, traverseParagraph, txt)
import Swarm.Util (Encoding (UTF8), readFileMayT)
import Swarm.Util.Lens ((<+=))
import System.FilePath (takeExtension)

-- | A single page in the help collection. Contains a parsed document
--   along with arbitrary metadata.
data HelpPage = HelpPage
  { _helpMetadata :: Map Text (Document (Syntax Raw))
  , _helpDoc :: Document (Syntax Raw)
  }

makeLenses ''HelpPage

-- | Configuration for generic collection loader to load help pages
helpCollectionConfig :: CollectionConfig HelpPage
helpCollectionConfig =
  CollectionConfig
    { shouldLoad = \_ path -> pure (takeExtension path == ".md") -- only load .md files
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
        Right doc -> pure (Right (warns, HelpPage metadata doc))

-- | Parse metadata fields from a list of lines that start with %
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

-- | Elaborate the help collection by (1) rendering every table of
--   contents, and (2) inserting counters to disambiguate duplicate links
elaborateHelp :: Collection HelpPage -> Collection HelpPage
elaborateHelp help = fmap (over helpDoc (disambiguateLinks . renderTOCs help)) help

-- | Load the collection of help pages from the standard location (data/help).
loadHelp ::
  ( Warn SystemFailure :> es
  , Error SystemFailure :> es
  , IOE :> es
  ) =>
  Eff es (Collection HelpPage)
loadHelp = do
  helpFolder <- getDataDirThrow Help "help"
  elaborateHelp <$> loadCollection helpCollectionConfig helpFolder
