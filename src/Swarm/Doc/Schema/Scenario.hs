{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Render a markdown document fragment
-- from the Scenario JSON schema files.
module Swarm.Doc.Schema.Scenario where

import Control.Arrow (left, (&&&))
import Data.Aeson
import Data.Map.Strict qualified as M
import Swarm.Doc.Schema.Refined
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Swarm.Doc.Schema.Surface
import Swarm.Util (quote, showT)
import System.FilePath ((<.>), (</>), splitExtension, takeBaseName)
import Text.Pandoc
import Text.Pandoc.Builder
import System.Directory (listDirectory)

scenariosDir :: FilePath
scenariosDir = "data/scenarios"

schemasDir :: FilePath
schemasDir = "data/schema"

schemaExtension :: String
schemaExtension = ".json"

columnHeadings :: [T.Text]
columnHeadings =
  [ "Key"
  , "Default?"
  , "Type"
  , "Description"
  ]

makePandocTable :: (String, SwarmSchema) -> Pandoc
makePandocTable (fn, schm) =
  setTitle (text "JSON Schema for Scenarios") $
    doc $
      header 3 (text . T.toTitle . T.pack $ takeBaseName fn)
        <> maybe mempty (para . text) (description schm)
        <> myTable
 where
  genRow :: (T.Text, SwarmSchema) -> [Blocks]
  genRow (k, x) =
    [ plain $ code k
    , maybe mempty (plain . code . renderValue) $ defaultValue x
    , plain . listToText $ schemaType x
    , plain . text . fromMaybe "" $ description x
    ]

  headerRow = map (plain . text) columnHeadings
  myTable = simpleTable headerRow . map genRow . M.toList . fromMaybe mempty $ properties schm

type FileStemAndExtension = (FilePath, String)

recombineExtension :: FileStemAndExtension -> FilePath
recombineExtension (filenameStem, fileExtension) =
  filenameStem <.> fileExtension

genScenarioSchemaDocs :: IO ()
genScenarioSchemaDocs = do
  dirContents <- listDirectory schemasDir
  let inputFiles = filter ((== schemaExtension) . snd) $ map splitExtension dirContents
  xs <- mapM (sequenceA . (recombineExtension &&& parseSchemaFile)) inputFiles
  let eitherMarkdown = do
        schemas <- traverse sequenceA xs
        let pd = mconcat $ map makePandocTable schemas
        left renderError $ runPure (writeMarkdown (def {writerExtensions = extensionsFromList [Ext_pipe_tables]}) pd)

  case eitherMarkdown of
    Left e -> print $ unwords ["Failed:", T.unpack e]
    Right md -> writeFile (scenariosDir </> "README_NEW.md") $ T.unpack md
 where

  parseSchemaFile :: FileStemAndExtension -> IO (Either T.Text SwarmSchema)
  parseSchemaFile stemAndExtension =
    left (prependPath . T.pack) <$> eitherDecodeFileStrict fullPath
    where
      prependPath = ((T.unwords ["in", quote (T.pack filename)] <> ": ") <>)
      filename = recombineExtension stemAndExtension
      fullPath = schemasDir </> filename

renderValue :: Value -> T.Text
renderValue = \case
  Object obj -> showT obj
  Array arr -> showT arr
  String t -> quote t
  Number num -> showT num
  Bool b -> showT b
  Null -> "null"
