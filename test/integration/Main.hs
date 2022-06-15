{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Swarm integration tests
module Main where

import Control.Lens (use, (<&>))
import Control.Monad (filterM, void)
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Data.Yaml (ParseException, prettyPrintParseException)
import Swarm.Game.Entity (EntityMap, loadEntities)
import Swarm.Game.Scenario (Scenario)
import Swarm.Game.State (GameState, WinCondition (Won), initGameState, winCondition)
import Swarm.Game.Step (gameTick)
import Swarm.Language.Pipeline (processTerm)
import Swarm.Util.Yaml (decodeFileEitherE)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath.Posix (takeExtension, (</>))
import System.Timeout (timeout)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Witch (into)

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
          , testScenarioSolution em
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
  testCase ("parse scenario " ++ show path) (void $ getScenario em path)

getScenario :: EntityMap -> FilePath -> IO Scenario
getScenario em p = do
  res <- decodeFileEitherE em p :: IO (Either ParseException Scenario)
  case res of
    Left err -> assertFailure (prettyPrintParseException err)
    Right s -> return s

acquire :: FilePath -> String -> IO [(FilePath, String)]
acquire dir ext = do
  paths <- listDirectory dir <&> map (dir </>)
  filePaths <- filterM (\path -> doesFileExist path <&> (&&) (hasExt path)) paths
  mapM (\path -> (,) path <$> readFile path) filePaths
 where
  hasExt path = takeExtension path == ("." ++ ext)

testScenarioSolution :: EntityMap -> TestTree
testScenarioSolution _em =
  testGroup
    "Test scenario solutions"
    [ testSolution "chess" "data/scenarios/03Challenges/01-chess_horse.yaml" "example/chess-solution.sw"
    ]
 where
  testSolution :: TestName -> FilePath -> FilePath -> TestTree
  testSolution n p r = testCase n $ do
    Right gs <- runExceptT $ initGameState Nothing (Just p) (Just r)
    m <- timeout (60 * sec) (snd <$> runStateT playUntilWin gs)
    case m of
      Nothing -> assertFailure "Timed out"
      Just _g -> pure ()

  sec :: Int
  sec = 10 ^ (6 :: Int)

  playUntilWin :: StateT GameState IO ()
  playUntilWin = do
    w <- use winCondition
    case w of
      Won _ -> return ()
      _ -> gameTick >> playUntilWin