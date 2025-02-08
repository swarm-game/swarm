{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- LSP unit tests
module TestLSP (testLSP) where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Swarm.Language.LSP.VarUsage qualified as VU
import Swarm.Language.Parser (readTerm)
import Swarm.Language.Syntax qualified as S
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

baseTestPath :: FilePath
baseTestPath = "data/test/language-snippets/warnings/unused-vars"

data UnusedVar = UnusedVar S.Var VU.BindingType
  deriving (Eq, Show)

simplifyWarning :: VU.VarUsage -> UnusedVar
simplifyWarning (VU.VarUsage (S.LV _ v) scope) = UnusedVar v scope

testLSP :: TestTree
testLSP =
  testGroup
    "Unused variable warnings"
    [ testCase "outer lambda"
        . checkFile "multiple-lambda-first-unused.sw"
        . pure
        $ UnusedVar "x" VU.Lambda
    , testCase "inner lambda"
        . checkFile "multiple-lambda-second-unused.sw"
        . pure
        $ UnusedVar "y" VU.Lambda
    , testCase "shadowed variable name"
        . checkFile "shadowed-variable-lambda-unused.sw"
        . pure
        $ UnusedVar "x" VU.Lambda
    , testCase "outer let"
        . checkFile "multiple-let-first-unused.sw"
        . pure
        $ UnusedVar "x" VU.Let
    , testCase "outer let"
        . checkFile "multiple-let-second-unused.sw"
        . pure
        $ UnusedVar "y" VU.Let
    , testCase "multiple unused let" $
        checkFile
          "multiple-let-all-unused.sw"
          [UnusedVar "x" VU.Let, UnusedVar "y" VU.Let]
    , testCase "shadowed let without usage" $
        checkFile
          "shadowed-variable-let-unused.sw"
          [UnusedVar "x" VU.Let]
    , testCase "shadowed let with intermediate usage" $
        checkFile
          "shadowed-variable-let-intermediate-use.sw"
          []
    , testCase "recursive let" $
        checkFile
          "recursive-let.sw"
          [UnusedVar "fac" VU.Let]
    , testCase "single unused bind" $
        checkFile
          "single-bind-unused.sw"
          [UnusedVar "x" VU.Bind]
    , testCase "single used bind" $
        checkFile
          "single-bind-used.sw"
          []
    , testCase "lambda with var used inside annotation" $
        checkFile
          "lambda-with-annot.sw"
          []
    , testCase "record with used var" $
        checkFile
          "lambda-with-record-used.sw"
          []
    , testCase "record with used var abbrev" $
        checkFile
          "lambda-with-record-used-abbrev.sw"
          []
    , testCase "record with unused var" $
        checkFile
          "lambda-with-record-unused.sw"
          [UnusedVar "y" VU.Lambda]
    ]
 where
  checkFile :: FilePath -> [UnusedVar] -> IO ()
  checkFile filename expectedWarnings = do
    content <- TIO.readFile fullPath
    let actualWarnings = getWarnings content
    assertEqual "failed" expectedWarnings actualWarnings
   where
    fullPath = baseTestPath </> filename

  getWarnings :: Text -> [UnusedVar]
  getWarnings content =
    case readTerm content of
      Right (Just term) -> map simplifyWarning problems
       where
        VU.Usage _ problems = VU.getUsage mempty term
      _ -> []
