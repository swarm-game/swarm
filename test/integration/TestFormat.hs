{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Test the behavior of 'swarm format' on selected input files.
module TestFormat (testFormatting) where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Swarm.Language.Format
import Swarm.Language.Parser.Core (LanguageVersion (SwarmLangLatest))
import Swarm.Util (Encoding (..), findAllWithExt, readFileMayT)
import Swarm.Util.InputSource (InputSource (Stdin))
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

  -- Remove a final trailing newline character, if any
  trimFinalEOL :: T.Text -> T.Text
  trimFinalEOL t = fromMaybe t (T.stripSuffix "\n" t)

  checkFormat :: FilePath -> TestTree
  checkFormat f = testCase f $ do
    mcontent <- readFileMayT UTF8 f
    case mcontent of
      Nothing -> assertFailure $ "Failed to load " <> f
      Just content -> case formatSwarm defaultFormatCfg content of
        Left err -> assertFailure (into @String err)
        -- Make sure they are the same, up to a trailing newline
        Right formatted ->
          assertEqual "Formatted code does not match." (trimFinalEOL content) (trimFinalEOL formatted)
