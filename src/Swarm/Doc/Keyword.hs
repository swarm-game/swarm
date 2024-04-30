{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: keywords for doc gen and testing
--
-- Collect keywords for documentation generation and testing.
module Swarm.Doc.Keyword (
  EditorType (..),

  -- ** Formatted keyword lists
  keywordsCommands,
  keywordsDirections,
  operatorNames,
  builtinFunctionList,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Doc.Util
import Swarm.Language.Syntax qualified as Syntax
import Swarm.Util (quote)

-- | An enumeration of the editors supported by Swarm (currently,
--   Emacs and VS Code).
data EditorType = Emacs | VSCode | Vim
  deriving (Eq, Show, Enum, Bounded)

builtinFunctionList :: EditorType -> Text
builtinFunctionList e = editorList e $ map constSyntax builtinFunctions

editorList :: EditorType -> [Text] -> Text
editorList = \case
  Emacs -> T.unlines . map (("  " <>) . quote)
  VSCode -> T.intercalate "|"
  Vim -> T.intercalate " "

-- | Get formatted list of basic functions/commands.
keywordsCommands :: EditorType -> Text
keywordsCommands e = editorList e $ map constSyntax commands

-- | Get formatted list of directions.
keywordsDirections :: EditorType -> Text
keywordsDirections e = editorList e $ map Syntax.directionSyntax Syntax.allDirs

-- | A list of the names of all the operators in the language.
operatorNames :: Text
operatorNames = T.intercalate "|" $ map (escape . constSyntax) operators
 where
  special :: String
  special = "*+$[]|^"
  slashNotComment = \case
    '/' -> "/(?![/|*])"
    c -> T.singleton c
  escape = T.concatMap (\c -> if c `elem` special then T.snoc "\\\\" c else slashNotComment c)
