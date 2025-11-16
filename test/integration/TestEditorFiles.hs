{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Ensure provided editor configuration files are up to date.
module TestEditorFiles (testEditorFiles) where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Doc.Keyword (EditorType (..))
import Swarm.Doc.Keyword qualified as Keyword
import Swarm.Util (Encoding (..), applyWhen, readFileMayT)
import Test.Tasty
import Test.Tasty.HUnit

-- | Test that editor files are up-to-date.
testEditorFiles :: TestTree
testEditorFiles =
  testGroup
    "editors"
    [ testGroup
        "VS Code"
        [ testTextInVSCode "operators" Keyword.operatorNames
        , testTextInVSCode "builtin" Keyword.builtinFunctionList
        , testTextInVSCode "commands" Keyword.keywordsCommands
        , testTextInVSCode "directions" Keyword.keywordsDirections
        ]
    , testGroup
        "Emacs"
        [ testTextInEmacs "operators" Keyword.operatorNames
        , testTextInEmacs "builtin" Keyword.builtinFunctionList
        , testTextInEmacs "commands" Keyword.keywordsCommands
        , testTextInEmacs "directions" Keyword.keywordsDirections
        ]
    , testGroup
        "Vim"
        [ testTextInVim "operators" Keyword.operatorNames
        , testTextInVim "builtin" Keyword.builtinFunctionList
        , testTextInVim "commands" Keyword.keywordsCommands
        , testTextInVim "directions" Keyword.keywordsDirections
        ]
    ]
 where
  testTextInVSCode name tf = testTextInFile False name (tf VSCode) "editors/vscode/syntaxes/swarm.tmLanguage.yaml"
  testTextInEmacs name tf = testTextInFile True name (tf Emacs) "editors/emacs/swarm-mode.el"
  testTextInVim name tf = testTextInFile False name (tf Vim) "editors/vim/swarm.vim"
  testTextInFile :: Bool -> String -> Text -> FilePath -> TestTree
  testTextInFile whitespace name t fp = testCase name $ do
    let removeLW' = T.unlines . map (T.dropWhile isSpace) . T.lines
        removeLW = applyWhen whitespace removeLW'
    f <- maybe (assertFailure "Can't read file!") pure =<< readFileMayT UTF8 fp
    assertBool
      ( "EDITOR FILE IS NOT UP TO DATE!\n"
          <> "I could not find the text:\n"
          <> T.unpack t
          <> "\nin file "
          <> fp
      )
      (removeLW t `T.isInfixOf` removeLW f)
