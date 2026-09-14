{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Parsing of Markdown metadata header
module TestMarkdownMetadata where

import Data.Foldable (forM_)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Language.Help (parseMetadata)
import Swarm.Pretty (prettyText)
import Swarm.Text.Markdown.Pretty (docToMark)
import Test.Tasty
import Test.Tasty.HUnit

testMarkdownMetadata :: TestTree
testMarkdownMetadata =
  testGroup
    "Markdown metadata headers"
    [ testCase "title" $
        testHeaderParse ["% title: Foo bar"] [("title", "Foo bar")]
    , testCase "title + extra space" $
        testHeaderParse ["%    title: Foo bar"] [("title", "Foo bar")]
    , testCase "title + author" $
        testHeaderParse ["% title: Foo bar", "% author: X"] [("title", "Foo bar"), ("author", "X")]
    , testCase "multiple fields" $
        testHeaderParse
          (T.lines "% title: Foo bar\n% author: X\n% version: 17\n% date: June 4, 1883")
          [ ("title", "Foo bar")
          , ("date", "June 4, 1883")
          , ("author", "X")
          , ("version", "17")
          ]
    , testCase "title with nontrivial Markdown" $
        testHeaderParse
          ["% title: *This*   is a [title](url) with markup"]
          [("title", "*This* is a [title](url) with markup")]
    , testCase "missing colon" $
        testHeaderParse ["% title Foo bar"] []
    ]

testHeaderParse :: [Text] -> [(Text, Text)] -> Assertion
testHeaderParse hdrs fields = do
  let (warns, fieldMap) = parseMetadata hdrs
  case fields of
    [] ->
      assertBool
        (T.unpack . T.unlines $ "Markdown header parsing should have generated warnings but did not:" : hdrs)
        (not (null warns))
    _ ->
      assertBool
        (T.unpack . T.unlines $ "Warnings when parsing markdown headers:" : map prettyText warns)
        (null warns)

  forM_ fields $ \(field, expected) ->
    case M.lookup field fieldMap of
      Nothing -> assertFailure $ "Field not found: " <> T.unpack field
      Just actual -> assertEqual "foo" expected (docToMark actual)
