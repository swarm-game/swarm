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
) where

import Control.Algebra (Has)
import Control.Carrier.Lift (Lift)
import Control.Effect.Accum (Accum)
import Control.Effect.Throw (Throw)
import Control.Lens (makeLenses)
import Data.Bifunctor (first, second)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Failure (Asset (..), AssetData (Help), Entry (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Language.Syntax (Phase (Raw), Syntax)
import Swarm.Language.Text.Markdown (Document, fromTextE)
import Swarm.ResourceLoading (getDataDirThrow)
import Swarm.ResourceLoading.Collection (
  Collection,
  CollectionConfig (..),
  loadCollection,
 )
import Swarm.Util (Encoding (UTF8), readFileMayT)

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
    { shouldLoad = \_ _ -> pure True -- XXX fix me, what should be ignored?
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
    | otherwise = (field,) <$> first (CustomFailure . T.pack) (fromTextE (stripPunct content))

-- | Load the collection of help pages from the standard location (data/help).
loadHelp ::
  ( Has (Accum (Seq SystemFailure)) sig m
  , Has (Throw SystemFailure) sig m
  , Has (Lift IO) sig m
  ) =>
  m (Collection HelpPage)
loadHelp = do
  helpFolder <- getDataDirThrow Help "help"
  loadCollection helpCollectionConfig helpFolder
