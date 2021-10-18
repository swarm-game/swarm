{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Swarm unit tests
module Main where

import Control.Monad
import Data.Either
import Data.Functor
import Data.Text (Text)
import Swarm.Language.Pipeline (processTerm)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath.Posix
import Test.Tasty
import Test.Tasty.HUnit
import Witch

main :: IO ()
main = do
  paths <- acquire
  defaultMain . exampleTests $ paths

exampleTests :: [(FilePath, String)] -> TestTree
exampleTests inputs = testGroup "Test example" (map exampleTest inputs)

exampleTest :: (FilePath, String) -> TestTree
exampleTest (path, fileContent) = do
  testCase ("processTerm for contents of " ++ show path) $
    assertBool "should return Right value" (isRight . processTerm $ into @Text fileContent)

acquire :: IO [(FilePath, String)]
acquire = do
  paths <- listDirectory exampleDirectory <&> map (exampleDirectory </>)
  filePaths <- filterM (\path -> doesFileExist path <&> (&&) (swExtension path)) paths
  mapM (\path -> (,) path <$> readFile path) filePaths
 where
  exampleDirectory = "example"
  swExtension path = takeExtension path == ".sw"
