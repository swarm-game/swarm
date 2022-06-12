{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Swarm integration tests
module Main where

import Control.Monad
import Data.Functor
import qualified Data.IntMap as IM
import Data.Text (Text)
import Data.Yaml (ParseException, prettyPrintParseException)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath.Posix
import Test.Tasty
import Test.Tasty.HUnit
import Witch
import Control.Monad.State
import Swarm.Game.Entity
import Swarm.Game.Scenario
import Swarm.Language.Pipeline (processTerm)
import Swarm.Language.Context as Ctx
import Swarm.Util.Yaml (decodeFileEitherE)
import Swarm.Game.State
import Control.Monad.Trans.Except (runExceptT)
import Swarm.Game.CESK
import Swarm.Game.Value
import Swarm.Game.Robot
import Swarm.Game.Step
import Swarm.Game.Exception
import Control.Lens hiding (from)

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
testScenarioSolution em =
  testGroup
    "Test scenario solutions"
    [ testSolution "chess" "data/scenarios/03Challenges/01-chess_horse.yaml" "example/chess-solution.sw"
    ]
 where
  testSolution :: TestName -> FilePath -> FilePath -> TestTree
  testSolution n p r = testCase n $ do
    Right gs <- runExceptT $ initGameState Nothing (Just p) (Just r)
    v <- evalScenario gs
    case v of
      Left err -> assertFailure ("Evaluation failed: " ++ from @Text @String err)
      Right (VBool True) -> return ()
      Right v -> assertFailure ("Not what I hoped for: " <> show v)

  evalScenario :: GameState -> IO (Either Text Value)
  evalScenario g = evaluateCESK (initMachine w Ctx.empty emptyStore) g
   where
    w = case g ^. winCondition of
      WinCondition win -> win
      _ -> error "no win condition"

  evaluateCESK :: CESK -> GameState -> IO (Either Text Value)
  evaluateCESK cesk g = flip evalStateT g . flip evalStateT r . runCESK $ cesk
   where
    r :: Robot
    r = (g ^. robotMap) IM.! 0

  runCESK :: CESK -> StateT Robot (StateT GameState IO) (Either Text Value)
  runCESK (Up exn _ []) = return (Left (formatExn em exn))
  runCESK cesk = case finalValue cesk of
    Just (v, _) -> return (Right v)
    Nothing -> stepCESK cesk >>= runCESK