{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Test Markdown processing capabilities.
module TestMarkdown (testMarkdown) where

import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Text (Text)
import Swarm.Text.Markdown (fromTextM, toTextWidth)
import Swarm.Util (acquireAllWithExt, findAllWithExt)
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Witch (Utf8L, via)

-- | Generate test tree to check that the .md files in
--   @data/test/markdown/@ can be processed and laid out successfully.
--
--   Expects to see input files named foo.md, and golden outputs named
--   foo.nnn.txt, where nnn is the line width limit.  foo.0.txt is a
--   special case that uses no line width limit.
testMarkdown :: IO TestTree
testMarkdown = do
  testFiles <- acquireAllWithExt "data/test/markdown" "md"
  goldenFiles <- findAllWithExt "data/test/markdown" "txt"
  let goldenMap :: Map String [FilePath]
      goldenMap =
        M.fromListWith
          (++)
          [ (key, [name])
          | name <- goldenFiles
          , let key = dropExtension . dropExtension $ name
          ]
      checkMarkdown :: (FilePath, Text) -> TestTree
      checkMarkdown (fp, md) =
        testGroup fp (maybe [] (map (mkMarkdownTest md)) (goldenMap !? dropExtension fp))

      mkMarkdownTest :: Text -> FilePath -> TestTree
      mkMarkdownTest md goldenFile =
        let w = drop 1 . takeExtension . dropExtension $ goldenFile
         in goldenVsString w goldenFile $ do
              doc <- fromTextM md
              pure . via @Utf8L $ toTextWidth (Just (read w)) doc

  pure . testGroup "Check markdown processing" $ map checkMarkdown testFiles
