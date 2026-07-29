{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Test Markdown processing capabilities.
module TestMarkdown (testMarkdown) where

import Data.Char (isSpace)
import Data.Function (on)
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Language.Syntax (Raw, Syntax)
import Swarm.Language.Syntax.Util (eraseSrcLoc)
import Swarm.Text.Markdown (fromTextM, toTextWidth)
import Swarm.Text.Markdown.Document (Document, Node (..), mapD, mapP)
import Swarm.Text.Markdown.Pretty (docToMark)
import Swarm.Util (acquireAllWithExt)
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

-- | Generate test tree to check that the .md files in
--   @data/test/markdown/@ can be processed and laid out successfully,
--   and also round-tripped back to Markdown.
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
        testGroup fp (mkRoundTripTest md : maybe [] (map (mkMarkdownTest md)) (outputMap !? dropExtension fp))

      -- It would be much too strong to require the pretty-printed
      -- Markdown to be identical to the original.  In fact, even
      -- requiring the pretty-printed Markdown to parse to an
      -- identical AST as the original is slightly too strong, since
      -- the Commonmark parser produces LeafText nodes that preserve
      -- the exact number of spaces in the original source.  However,
      -- simply requiring parsing then pretty-printing to be
      -- idempotent would be much too weak, since it would be
      -- satisfied e.g. by a pretty-printer that always produced the
      -- letter Q and nothing else.
      --
      -- Instead, we require that parsing produces an AST which is
      -- equivalent to the AST produced by parsing, pretty-printing,
      -- then parsing again, but only up to normalization of LeafText
      -- nodes containing spaces.

      mkRoundTripTest :: Text -> TestTree
      mkRoundTripTest md = testCase "round-trip" $ do
        doc <- fromTextM md
        let md' = docToMark doc
        doc' <- fromTextM md'
        (assertEqual "Round-tripped markdown does not parse equivalently (up to whitespace + pretty-printing)" `on` normalizeMarkdown) doc doc'

      normalizeMarkdown :: Document (Syntax Raw) -> Document (Syntax Raw)
      normalizeMarkdown = (mapD . mapP) normalizeNode

      normalizeNode :: Node (Syntax Raw) -> Node (Syntax Raw)
      normalizeNode = \case
        LeafText as t -> LeafText as (normalizeText t)
        LeafCode c -> LeafCode (eraseSrcLoc c)
        LeafCodeBlock a c -> LeafCodeBlock a (eraseSrcLoc c)
        n -> n

      normalizeText t
        | T.all isSpace t = " "
        | otherwise = t

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
