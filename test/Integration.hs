{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Swarm unit tests
module Main where

import Data.Either
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Witch

import Swarm.Language.Pretty
import Swarm.Language.Syntax hiding (mkOp)
import Swarm.Language.Pipeline (processTerm)
import System.Directory (doesFileExist, listDirectory)

main :: IO ()
main = defaultMain (withResource acquire exampleTests)

exampleTests :: IO [(FilePath, String)] -> TestTree
exampleTests = testGroup "Examples test" (mapM exampleTest)

exampleTest (path, fileContents) =
  testCase ("processTerm for contents of " ++ show path)
    $ assertBool "should return Right value" (isRight . processTerm $ into @Text fileContents)

acquire :: IO [(FilePath, String)]
acquire = do
  paths <- listDirectory "example"
  filePaths <- filterM doesFileExist paths
  mapM (\path -> (path, readFile path)) filePaths
