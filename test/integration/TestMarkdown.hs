{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Test Markdown processing capabilities.
module TestMarkdown (testMarkdown) where

import Data.Char (isSpace)
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Text.Markdown (fromTextM, toTextWidth)
import Swarm.Util (acquireAllWithExt)
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

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
          assertEqualUpToTrailingWS "Laid-out markdown does not match" golden (toTextWidth (Just w) doc)

  pure . testGroup "Check markdown processing" $ map checkMarkdown testFiles

assertEqualUpToTrailingWS :: String -> Text -> Text -> Assertion
assertEqualUpToTrailingWS msg x y = assertEqual msg (trim x) (trim y)
 where
  trim = T.dropWhileEnd isSpace
