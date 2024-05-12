{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm parser tests.
module TestParse where

import Data.Foldable qualified as F
import Data.Text (Text)
import Swarm.Language.Parse
import Swarm.Language.Syntax
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Text.Megaparsec (errorBundlePretty)
import Witch (into)

testParse :: TestTree
testParse =
  testGroup
    "Parser - comments"
    [ testCase "none" $
        expectParsedComments
          "1 + 2"
          []
    , testCase "suffix" $
        expectParsedComments
          "1 + 2 // add"
          [Comment (SrcLoc 6 12) LineComment SuffixComment " add"]
    , testCase "standalone" $
        expectParsedComments
          "// add\n1 + 2"
          [Comment (SrcLoc 0 6) LineComment StandaloneComment " add"]
    , testCase "block suffix" $
        expectParsedComments
          "1 + 2 /* add */"
          [Comment (SrcLoc 6 15) BlockComment SuffixComment " add "]
    , testCase "block standalone" $
        expectParsedComments
          "/* add */\n1 + 2"
          [Comment (SrcLoc 0 9) BlockComment StandaloneComment " add "]
    , testCase "block prefix" $
        expectParsedComments
          "/* add */ 1 + 2"
          [Comment (SrcLoc 0 9) BlockComment StandaloneComment " add "]
    , testCase "block infix" $
        expectParsedComments
          "1 + /*add*/ 2"
          [Comment (SrcLoc 4 11) BlockComment SuffixComment "add"]
    , testCase "multiline block" $
        expectParsedComments
          "/* add \n  some numbers */\n  1 + 2"
          [Comment (SrcLoc 0 25) BlockComment StandaloneComment " add \n  some numbers "]
    , testCase "multiple lines" $
        expectParsedComments
          "// add\n// some numbers\n  1 + 2"
          [ Comment (SrcLoc 0 6) LineComment StandaloneComment " add"
          , Comment (SrcLoc 7 22) LineComment StandaloneComment " some numbers"
          ]
    ]

expectParsedComments :: Text -> [Comment] -> Assertion
expectParsedComments input ex = case readTerm' input of
  Left err -> assertFailure (into @String $ errorBundlePretty err)
  Right (_, res) -> assertEqual "Expected parsed comments" ex (F.toList res)
