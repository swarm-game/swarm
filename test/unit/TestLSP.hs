{-# LANGUAGE OverloadedStrings #-}

-- | LSP unit tests
module TestLSP (testLSP) where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Swarm.Language.LSP.VarUsage qualified as VU
import Swarm.Language.Parse (readTerm')
import Swarm.Language.Syntax qualified as S
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

baseTestPath :: FilePath
baseTestPath = "editors/vscode/test/warnings/unused-vars"

data UnusedVar = UnusedVar S.Var VU.BindingType
  deriving (Eq, Show)

simplifyWarning :: VU.VarUsage -> UnusedVar
simplifyWarning (VU.VarUsage (S.LV _ v) scope) = UnusedVar v scope

testLSP :: TestTree
testLSP =
  testGroup
    "Unused variable warnings"
    [ testCase "outer lambda" $
        checkFile "multiple-lambda-first-unused.sw" $
          pure $
            UnusedVar "x" VU.Lambda
    , testCase "inner lambda" $
        checkFile "multiple-lambda-second-unused.sw" $
          pure $
            UnusedVar "y" VU.Lambda
    , testCase "outer let" $
        checkFile "multiple-let-first-unused.sw" $
          pure $
            UnusedVar "x" VU.Let
    , testCase "outer let" $
        checkFile "multiple-let-second-unused.sw" $
          pure $
            UnusedVar "y" VU.Let
    , testCase "multiple unused let" $
        checkFile
          "multiple-let-all-unused.sw"
          [UnusedVar "x" VU.Let, UnusedVar "y" VU.Let]
    , testCase "single unused bind" $
        checkFile
          "single-bind-unused.sw"
          [UnusedVar "x" VU.Bind]
    , testCase "single used bind" $
        checkFile
          "single-bind-used.sw"
          []
    ]
 where
  checkFile :: FilePath -> [UnusedVar] -> IO ()
  checkFile filename expectedWarnings = do
    content <- TIO.readFile fullPath
    let actualWarnings = getWarnings content
    assertEqual "failed" actualWarnings expectedWarnings
   where
    fullPath = baseTestPath </> filename

  getWarnings :: Text -> [UnusedVar]
  getWarnings content =
    case readTerm' content of
      Right (Just term) -> map simplifyWarning problems
       where
        VU.Usage _ problems = VU.getUsage mempty term
      _ -> []
