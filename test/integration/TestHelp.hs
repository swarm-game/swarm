{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Test help system.
module TestHelp (helpTests) where

import Swarm.Pretty (PrettyPrec)
import Control.Lens (imapM, (^.), (^?))
import Control.Monad (void)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Text qualified as T
import Swarm.Language.Help (HelpPage, helpDoc, helpMetadata)
import Swarm.ResourceLoading (Collection, atPath)
import Swarm.Text.Markdown.Document (Document (..), Node (..), traverseDocument, traverseParagraph, Target (..), pureP)
import Swarm.Text.Markdown.Pretty (docToMark)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

-- | Generate test tree for checking various properties of the help system data.
helpTests :: Collection HelpPage -> TestTree
helpTests help =
  testGroup
    "Help"
    [ helpLinks help
    , helpTitles help
    ]

-- | Check that there are no broken/dangling internal links.
helpLinks :: Collection HelpPage -> TestTree
helpLinks help =
  testCase "No broken/dangling help links" $ void $ imapM checkPageLinks help
 where
  checkPageLinks :: FilePath -> HelpPage -> IO ()
  checkPageLinks path pg = void $ (traverseDocument . traverseParagraph) checkNode (pg ^. helpDoc)
   where
    checkNode :: PrettyPrec c => Node c -> IO (Node c)
    checkNode = \case
      l@(LeafLink dest _ _) -> checkLink l dest >> pure l
      n -> pure n

    checkLink :: PrettyPrec c => Node c -> Target -> IO ()
    checkLink link = \case
      URL {} -> pure ()
      Internal dest ->
        assertBool
          ("Broken internal link in " <> path <> ": " <> T.unpack (docToMark (Document [pureP link])))
          (isJust (help ^? atPath (T.unpack dest)))

-- | Check that every help page has a title.
helpTitles :: Collection HelpPage -> TestTree
helpTitles help =
  testCase "All help pages have titles" $ void $ imapM checkTitle help
 where
  checkTitle :: FilePath -> HelpPage -> IO ()
  checkTitle path pg =
    assertBool
      ("Help page " <> path <> " has no title")
      (M.member "title" (pg ^. helpMetadata))
