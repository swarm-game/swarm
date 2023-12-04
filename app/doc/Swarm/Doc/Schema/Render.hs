{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Render a markdown document fragment
-- from the Scenario JSON schema files.
module Swarm.Doc.Schema.Render where

import Control.Arrow (left, (&&&))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (except)
import Data.Aeson
import Data.List (intersperse)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Scientific (FPFormat (..), Scientific, formatScientific)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Swarm.Doc.Schema.Arrangement
import Swarm.Doc.Schema.Parse
import Swarm.Doc.Schema.Refined
import Swarm.Doc.Schema.SchemaType
import Swarm.Doc.Util
import Swarm.Util (applyWhen, brackets, quote, showT)
import System.Directory (listDirectory)
import System.FilePath (splitExtension, (<.>), (</>))
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Walk (query)

scenariosDir :: FilePath
scenariosDir = "data/scenarios"

docFragmentsDir :: FilePath
docFragmentsDir = scenariosDir </> "doc-fragments"

schemasDir :: FilePath
schemasDir = "data/schema"

schemaExtension :: String
schemaExtension = ".json"

propertyColumnHeadings :: [T.Text]
propertyColumnHeadings =
  [ "Key"
  , "Default?"
  , "Type"
  , "Description"
  ]

listColumnHeadings :: [T.Text]
listColumnHeadings =
  [ "Index"
  , "Type"
  , "Description"
  ]

makeTitleMap :: [SchemaData] -> Map SchemaIdReference T.Text
makeTitleMap = M.fromList . map (fromFilePath . schemaPath &&& title . schemaContent)

makePandocTable :: Map SchemaIdReference T.Text -> SchemaData -> Pandoc
makePandocTable titleMap (SchemaData _ (ToplevelSchema theTitle theDescription _schema theMembers _) parsedFooters) =
  setTitle (text "JSON Schema for Scenarios") $
    doc (header 3 (text theTitle))
      <> fromMaybe mempty theDescription
      <> maybe mempty mkTable theMembers
      <> mconcat parsedFooters
 where
  renderItems someStuff = case someStuff of
    ItemType x -> plain $ text "List of " <> listToText titleMap (schemaType x)
    ItemList xs ->
      makePropsTable False listColumnHeadings titleMap
        . M.fromList
        $ zip (map tshow [0 :: Int ..]) xs

  mkTable x = doc $ case x of
    ObjectProperties props -> makePropsTable True propertyColumnHeadings titleMap props
    ListMembers someStuff -> renderItems someStuff

genPropsRow :: Bool -> Map SchemaIdReference T.Text -> (T.Text, SwarmSchema) -> [Blocks]
genPropsRow includeDefaultColumn titleMap (k, x) =
  firstColumn : applyWhen includeDefaultColumn (defaultColumn :) tailColumns
 where
  firstColumn = plain $ code k
  defaultColumn = maybe mempty (plain . code . renderValue) $ defaultValue x
  tailColumns =
    [ plain . listToText titleMap $ schemaType x
    , fromList $ maybe [] (query id) $ objectDescription x
    ]

makePropsTable ::
  Bool ->
  [T.Text] ->
  Map SchemaIdReference T.Text ->
  Map T.Text SwarmSchema ->
  Blocks
makePropsTable includeDefaultColumn headingsList titleMap =
  simpleTable headerRow . map (genPropsRow includeDefaultColumn titleMap) . M.toList
 where
  headerRow = map (plain . text) headingsList

type FileStemAndExtension = (FilePath, String)

recombineExtension :: FileStemAndExtension -> FilePath
recombineExtension (filenameStem, fileExtension) =
  filenameStem <.> fileExtension

genMarkdown :: [SchemaData] -> Either T.Text T.Text
genMarkdown schemaThings =
  left renderError $
    runPure $
      writeMarkdown (def {writerExtensions = extensionsFromList [Ext_pipe_tables]}) pd
 where
  titleMap = makeTitleMap schemaThings
  pd =
    mconcat $
      map (makePandocTable titleMap) $
        sortAndPruneSchemas (fromFilePath "scenario") schemaThings

parseSchemaFile :: FileStemAndExtension -> IO (Either T.Text ToplevelSchema)
parseSchemaFile stemAndExtension =
  left (prependPath . T.pack) <$> eitherDecodeFileStrict fullPath
 where
  prependPath = ((T.unwords ["in", quote (T.pack filename)] <> ": ") <>)
  filename = recombineExtension stemAndExtension
  fullPath = schemasDir </> filename

loadFooterContent :: (FilePath, ToplevelSchema) -> IO SchemaData
loadFooterContent (fp, schem) = do
  xs <- mapM (TIO.readFile . (scenariosDir </>)) $ footerPaths schem
  parsedFooters <- mapM getMarkdown xs
  return $
    SchemaData
      fp
      schem
      parsedFooters

genScenarioSchemaDocs :: IO ()
genScenarioSchemaDocs = do
  dirContents <- listDirectory schemasDir
  let inputFiles = filter ((== schemaExtension) . snd) $ map splitExtension dirContents
  xs <- mapM (sequenceA . (recombineExtension &&& parseSchemaFile)) inputFiles

  result <- runExceptT $ do
    schemaTuples <- except $ traverse sequenceA xs
    things <- liftIO $ mapM loadFooterContent schemaTuples
    myMarkdown <- except $ genMarkdown things
    docHeader <- liftIO $ TIO.readFile "data/scenarios/doc-fragments/header.md"
    liftIO . writeFile (docFragmentsDir </> "SCHEMA.md") . T.unpack $ docHeader <> myMarkdown

  case result of
    Left e -> print $ unwords ["Failed:", T.unpack e]
    Right _ -> return ()

renderValue :: Value -> T.Text
renderValue = \case
  Object obj -> showT obj
  Array arr -> brackets . T.intercalate ", " . map renderValue $ V.toList arr
  String t -> quote t
  Number num -> T.pack $ formatNumberCompact num
  Bool b -> showT b
  Null -> "null"

fragmentHref :: Map SchemaIdReference T.Text -> SchemaIdReference -> T.Text
fragmentHref titleMap r@(SchemaIdReference ref) =
  T.cons '#' . T.toLower . T.replace " " "-" $ x
 where
  x = M.findWithDefault ref r titleMap

listToText :: Map SchemaIdReference T.Text -> SchemaType -> Inlines
listToText titleMap = \case
  Simple xs -> renderAlternatives $ map code $ getList xs
  Alternatives xs -> renderAlternatives $ map (listToText titleMap) xs
  Reference r@(SchemaIdReference x) -> schemaLink r x
  ListOf x -> listToText titleMap x <> text " list"
 where
  renderAlternatives = mconcat . intersperse (text " or ")
  schemaLink r = link (fragmentHref titleMap r) "Link to object properties" . text

-- |
-- Strips trailing zeros and decimal point from a floating-point number
-- when possible.
--
-- Obtained from here: https://stackoverflow.com/a/35980995/105137
formatNumberCompact :: Scientific -> String
formatNumberCompact v
  | v == 0 = "0"
  | abs v < 1e-5 || abs v > 1e10 = formatScientific Exponent Nothing v
  | v - fromIntegral (floor v :: Integer) == 0 = formatScientific Fixed (Just 0) v
  | otherwise = formatScientific Generic Nothing v
