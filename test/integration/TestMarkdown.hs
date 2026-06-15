{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Test Markdown processing capabilities.
module TestMarkdown (testMarkdown) where

import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Language.Text.Markdown (chunksOf, fromTextM, streamToText, toStream)
import Swarm.Util (acquireAllWithExt)
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

-- | Generate test tree to check that the .md files in
--   @data/test/markdown/@ can be processed and laid out successfully.
--
--   Expects to see input files named foo.md, and golden outputs named
--   foo.nnn.txt, where nnn is the line width limit.  foo.0.txt is a
--   special case that uses no line width limit.
testMarkdown :: IO TestTree
testMarkdown = do
  testFiles <- acquireAllWithExt "data/test/markdown" "md"
  outputFiles <- acquireAllWithExt "data/test/markdown" "txt"
  let outputMap :: Map String [(Int, Text)]
      outputMap =
        M.fromListWith
          (++)
          [ (key, [(width, content)])
          | (name, content) <- outputFiles
          , let width = read . drop 1 . takeExtension . dropExtension $ name
          , let key = dropExtension . dropExtension $ name
          ]

      checkMarkdown :: (FilePath, Text) -> TestTree
      checkMarkdown (fp, md) =
        testGroup fp (maybe [] (map (mkMarkdownTest md)) (outputMap !? dropExtension fp))

      mkMarkdownTest :: Text -> (Int, Text) -> TestTree
      mkMarkdownTest md (w, golden) =
        testCase (show w) $ do
          doc <- fromTextM md
          assertEqual "Laid-out markdown does not match" golden (T.unlines . map streamToText . chunksOf w $ toStream doc)

  pure . testGroup "Check markdown processing" $ map checkMarkdown testFiles
