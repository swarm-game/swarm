{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Swarm integration tests
module Main where

import Control.Monad
import Data.Functor
import Data.Text (Text)
import Data.Yaml (ParseException, prettyPrintParseException)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath.Posix
import Test.Tasty
import Test.Tasty.HUnit
import Witch

import Swarm.Game.Challenge
import Swarm.Game.Entity
import Swarm.Language.Pipeline (processTerm)
import Swarm.Util.Yaml (decodeFileEitherE)

main :: IO ()
main = do
  examplePaths <- acquire "example" "sw"
  challengePaths <- acquire "data/challenges" "yaml"

  entities <- loadEntities
  case entities of
    Left t -> fail $ "Couldn't load entities: " <> into @String t
    Right em -> do
      defaultMain $
        testGroup
          "Tests"
          [ exampleTests examplePaths
          , challengeTests em challengePaths
          ]

exampleTests :: [(FilePath, String)] -> TestTree
exampleTests inputs = testGroup "Test example" (map exampleTest inputs)

exampleTest :: (FilePath, String) -> TestTree
exampleTest (path, fileContent) =
  testCase ("processTerm for contents of " ++ show path) $ do
    either (assertFailure . into @String) (const . return $ ()) value
 where
  value = processTerm $ into @Text fileContent

challengeTests :: EntityMap -> [(FilePath, String)] -> TestTree
challengeTests em inputs = testGroup "Test challenges" (map (challengeTest em) inputs)

challengeTest :: EntityMap -> (FilePath, String) -> TestTree
challengeTest em (path, _) =
  testCase ("parse challenge " ++ show path) $ do
    res <- decodeFileEitherE em path :: IO (Either ParseException Challenge)
    case res of
      Left err -> assertFailure (prettyPrintParseException err)
      Right _ -> return ()

acquire :: FilePath -> String -> IO [(FilePath, String)]
acquire dir ext = do
  paths <- listDirectory dir <&> map (dir </>)
  filePaths <- filterM (\path -> doesFileExist path <&> (&&) (hasExt path)) paths
  mapM (\path -> (,) path <$> readFile path) filePaths
 where
  hasExt path = takeExtension path == ("." ++ ext)
