{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- LSP unit tests
module TestLSP (testLSP) where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Swarm.Language.LSP.Hover (narrowToPosition)
import Swarm.Language.LSP.VarUsage qualified as VU
import Swarm.Language.Parser (readTerm)
import Swarm.Language.Parser.QQ
import Swarm.Language.Syntax (Syntax' (Syntax'))
import Swarm.Language.Syntax qualified as S
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad (unless)
import Swarm.Pretty (prettyString)

baseTestPath :: FilePath
baseTestPath = "data/test/language-snippets/warnings/unused-vars"

data UnusedVar = UnusedVar S.Var VU.BindingType
  deriving (Eq, Show)

simplifyWarning :: VU.VarUsage -> UnusedVar
simplifyWarning (VU.VarUsage (S.LV _ v) scope) = UnusedVar v scope

testLSP :: TestTree
testLSP =
  testGroup
    "Test LSP"
    [ testGroup
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
    , testGroup
        "narrowToPosition"
        [ testCase "narrow to TVar" $
            assertEqualTerms
              (S.Syntax' S.NoLoc (S.TVar "m2") (S.Comments mempty mempty) ())
              ( narrowToPosition
                  [astQ|def m2 = move; move end
                        def m4 = m2; m2 end
                        def m8 = m4; m4 end
                        def m16 = m8; m8 end|]
                  58
              )
        , testCase "narrow to forever" $
            assertEqualTerms
              (S.Syntax' S.NoLoc (S.TVar "forever") (S.Comments mempty mempty) ())
              ( narrowToPosition
                  [astQ|// A "cat" that wanders around randomly.  Shows off use of the
                        // 'random' command.
                        let forever : Cmd Unit -> Cmd Unit = \c. c ; forever c in
                        let repeat : Int -> Cmd Unit -> Cmd Unit =
                          \n. \c. if (n == 0) {} {c ; repeat (n-1) c} in
                        let randdir : Cmd Dir =
                          d <- random 4;
                          pure (
                            if (d == 0) {north}
                            {if (d == 1) {east}
                            {if (d == 2) {south} {west}}})
                          in

                        forever (
                          n <- random 20;
                          wait (10 + n);
                          d <- randdir;
                          turn d;
                          dist <- random 10;
                          repeat dist move;
                          r <- random 5;
                          if (r == 0) { say "meow" } {})|]
                  140
              )
        , testCase "narrow to maybe" $
            assertEqualTerms
              (S.Syntax' S.NoLoc (S.TVar "Maybe") (S.Comments mempty mempty) ())
              ( narrowToPosition
                  [astQ|tydef Maybe a = Unit + a end

                        def just : a -> Maybe a = inr end

                        def nothing : Maybe a = inl () end

                        def positive : Int -> Maybe Int = \x.
                          if (x > 0) {just x} {nothing}
                        end|]
                  8
              )
        ]
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

  -- When comparing Syntax elements we aren't interested in the SrcLoc or the comments so just compare the terms.
  -- This is because depending on the operating system the loc could be different due to line endings.
  assertEqualTerms (Syntax' _ expected _ _) (Syntax' _ actual _ _) =
    unless (actual == expected) $
      assertFailure
        ( "expected: "
            ++ prettyString expected
            ++ "\n but got: "
            ++ prettyString actual
        )
