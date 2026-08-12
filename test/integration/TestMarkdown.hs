{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Test Markdown processing capabilities.
module TestMarkdown (testMarkdown) where

import Control.Applicative (asum)
import Data.Char (isSpace)
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.These (These (..))
import Data.Zip (alignWith)
import Swarm.Language.Syntax (Raw, Syntax)
import Swarm.Language.Syntax.Util (eraseSrcLoc)
import Swarm.Text.Markdown (fromTextM, toTextWidth)
import Swarm.Text.Markdown.Document (Document (..), Node (..), Paragraph (..), mapD, mapP)
import Swarm.Text.Markdown.Pretty (docToMark)
import Swarm.Util (acquireAllWithExt, showT)
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

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
      -- the exact number of spaces in the original source, and the
      -- Swarm parser produces an AST with embedded source location
      -- information.  However, simply requiring parsing then
      -- pretty-printing to be idempotent would be much too weak,
      -- since it would be satisfied e.g. by a pretty-printer that
      -- always produced the letter Q and nothing else.
      --
      -- Instead, we require that parsing produces an AST which is
      -- equivalent to the AST produced by parsing, pretty-printing,
      -- then parsing again, but only up to normalization of LeafText
      -- nodes containing spaces and removal of all SrcLoc information
      -- from embedded Swarm code.

      mkRoundTripTest :: Text -> TestTree
      mkRoundTripTest md = testCase "round-trip" $ do
        doc <- fromTextM md
        let md' = docToMark doc
        doc' <- fromTextM md'
        let ndoc = normalizeMarkdown doc
            ndoc' = normalizeMarkdown doc'
        case diff ndoc ndoc' of
          Nothing -> pure ()
          Just (d1, d2) -> do
            let msg = "Round-tripped markdown does not parse equivalently (up to whitespace + pretty-printing)"
            assertFailure $ msg <> "\n" <> "expected:  " <> T.unpack d1 <> "\nbut got:  " <> T.unpack d2

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

------------------------------------------------------------
-- Basic low-effort diffing of Markdown documents, to help
-- with debugging for failed tests

class Show a => Diff a where
  diff :: a -> a -> Maybe (Text, Text)

instance Diff a => Diff [a] where
  diff xs ys = asum (alignWith diffExt xs ys)
   where
    diffExt = \case
      This x -> Just (showT x, "<EMPTY>")
      That y -> Just ("<EMPTY>", showT y)
      These x y -> diff x y

instance (Eq c, Diff c) => Diff (Document c) where
  diff (Document ps1) (Document ps2) = diff ps1 ps2

instance (Eq c, Diff c) => Diff (Paragraph c) where
  diff (SimpleParagraph ns1) (SimpleParagraph ns2) = diff ns1 ns2
  diff l1@(ListParagraph t1 s1 is1) l2@(ListParagraph t2 s2 is2)
    | t1 == t2 && s1 == s2 = diff is1 is2
    | otherwise = Just (showT l1, showT l2)
  diff p1 p2 = Just (showT p1, showT p2)

diffEq :: (Show a, Eq a) => a -> a -> Maybe (Text, Text)
diffEq x y
  | x == y = Nothing
  | otherwise = Just (showT x, showT y)

instance (Eq c, Diff c) => Diff (Node c) where
  diff (LeafCode c1) (LeafCode c2) = diff c1 c2
  diff (LeafCodeBlock s1 c1) (LeafCodeBlock s2 c2)
    | s1 == s2 = diff c1 c2
  diff (LeafLink t1 i1 c1) (LeafLink t2 i2 c2)
    | t1 == t2 && i1 == i2 = diff c1 c2
  diff n1 n2 = diffEq n1 n2

instance Diff (Syntax Raw) where
  diff = diffEq
