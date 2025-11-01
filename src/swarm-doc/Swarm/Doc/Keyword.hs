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

import Data.List (nub, sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Swarm.Doc.Util
import Swarm.Language.Syntax.Direction
import Swarm.Util (quote)

-- | An enumeration of the editors supported by Swarm (currently,
--   Emacs, VS Code and Vim).
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
keywordsDirections e = editorList e $ map directionSyntax allDirs

-- | A list of the names of all the operators in the language.
-- These are reflective of how the different editors treat operators,
-- keywords, symbols etc differently.
-- In order to get the list of operators supported by Swarm language
-- irrespective of an editor, @map constSyntax operators@ should suffice.
operatorNames :: EditorType -> Text
operatorNames e = case e of
  Emacs -> editorList e $ map constSyntax operators <> extraOperators
  -- Vim needs a list of unique characters that can be matched over using a regex
  Vim -> T.pack . nub . T.unpack . T.concat $ map constSyntax operators <> extraOperators
  VSCode -> editorList e . map escape . sortOn (Down . T.length) $ map constSyntax operators <> extraOperators
 where
  slashNotComment = \case
    '/' -> "/(?![/|*])"
    c -> T.singleton c

  special :: String
  special = "*+$[]|^"

  -- Extra operators appearing in different places. Eg: Type signatures.
  extraOperators :: [Text]
  extraOperators = [":"]

  escape = T.concatMap (\c -> if c `elem` special then T.snoc "\\" c else slashNotComment c)
