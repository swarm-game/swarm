{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Test the behavior of 'swarm format' on selected input files.
module TestFormat (testFormatting) where

import Data.Text qualified as T
import Swarm.Language.Format
import Swarm.Language.Parser.Core (LanguageVersion (SwarmLangLatest))
import Swarm.Util (findAllWithExt, readFileMayT)
import Test.Tasty
import Test.Tasty.HUnit
import Witch (into)

-- | Generate test tree to check that 'swarm format' is the identity
--   on .sw files in @data/test/language-snippets/format@.
testFormatting :: IO TestTree
testFormatting = do
  testFilePaths <- findAllWithExt "data/test/language-snippets/format" "sw"
  pure . testGroup "Check code formatting" $ map checkFormat testFilePaths
 where
  defaultFormatCfg = FormatConfig Stdin Stdout Nothing SwarmLangLatest

  checkFormat :: FilePath -> TestTree
  checkFormat f = testCase f $ do
    mcontent <- readFileMayT f
    case mcontent of
      Nothing -> assertFailure $ "Failed to load " <> f
      Just content -> case formatSwarm defaultFormatCfg content of
        Left err -> assertFailure (into @String err)
        -- Make sure they are the same, but ignore leading + trailing whitespace
        Right formatted ->
          assertEqual "Formatted code does not match." (T.strip content) (T.strip formatted)
