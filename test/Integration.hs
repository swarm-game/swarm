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

import Swarm.Game.Entity
import Swarm.Game.Scenario
import Swarm.Language.Pipeline (processTerm)
import Swarm.Util.Yaml (decodeFileEitherE)

main :: IO ()
main = do
  examplePaths <- acquire "example" "sw"
  scenarioPaths <- acquire "data/scenarios" "yaml"

  entities <- loadEntities
  case entities of
    Left t -> fail $ "Couldn't load entities: " <> into @String t
    Right em -> do
      defaultMain $
        testGroup
          "Tests"
          [ exampleTests examplePaths
          , scenarioTests em scenarioPaths
          ]

exampleTests :: [(FilePath, String)] -> TestTree
exampleTests inputs = testGroup "Test example" (map exampleTest inputs)

exampleTest :: (FilePath, String) -> TestTree
exampleTest (path, fileContent) =
  testCase ("processTerm for contents of " ++ show path) $ do
    either (assertFailure . into @String) (const . return $ ()) value
 where
  value = processTerm $ into @Text fileContent

scenarioTests :: EntityMap -> [(FilePath, String)] -> TestTree
scenarioTests em inputs = testGroup "Test scenarios" (map (scenarioTest em) inputs)

scenarioTest :: EntityMap -> (FilePath, String) -> TestTree
scenarioTest em (path, _) =
  testCase ("parse scenario " ++ show path) $ do
    res <- decodeFileEitherE em path :: IO (Either ParseException Scenario)
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
