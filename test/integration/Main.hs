{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Swarm integration tests
module Main where

import Control.Lens (Ixed (ix), use, (&), (.~), (<&>), (^.))
import Control.Monad (filterM, void)
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Data.Yaml (ParseException, prettyPrintParseException)
import Swarm.Game.CESK (emptyStore, initMachine)
import Swarm.Game.Entity (EntityMap, loadEntities)
import Swarm.Game.Robot (machine)
import Swarm.Game.Scenario (Scenario)
import Swarm.Game.State (GameState, WinCondition (Won), initGameState, robotMap, winCondition, winSolution)
import Swarm.Game.Step (gameTick)
import qualified Swarm.Language.Context as Ctx
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

data Time
  = -- | One second should be anough to run most programs.
    Default
  | -- | You can specify more seconds if you need to.
    Sec Int
  | -- | If you absolutely have to, you can ignore timeout.
    None

time :: Time -> Int
time = \case
  Default -> 1 * sec
  Sec s -> s * sec
  None -> -1
 where
  sec :: Int
  sec = 10 ^ (6 :: Int)

testScenarioSolution :: EntityMap -> TestTree
testScenarioSolution _em =
  testGroup
    "Test scenario solutions"
    [ testGroup
        "Tutorial"
        [ testSolution "move" Default "data/scenarios/02Tutorial/00-move.yaml"
        , testSolution "turn" Default "data/scenarios/02Tutorial/01-turn.yaml"
        ]
    , testGroup
        "Challenges"
        [ testSolution "chess" Default "data/scenarios/03Challenges/01-chess_horse.yaml"
        , testSolution "test (grab)" Default "data/scenarios/03Challenges/00-test.yaml"
        ]
    ]
 where
  testSolution :: TestName -> Time -> FilePath -> TestTree
  testSolution n s p = testCase n $ do
    Right gs <- runExceptT $ initGameState Nothing (Just p) Nothing
    case gs ^. winSolution of
      Nothing -> assertFailure "No solution to test!"
      Just sol -> do
        let gs' = gs & robotMap . ix 0 . machine .~ initMachine sol Ctx.empty emptyStore
        m <- timeout (time s) (snd <$> runStateT playUntilWin gs')
        case m of
          Nothing -> assertFailure "Timed out"
          Just _g -> pure ()

  playUntilWin :: StateT GameState IO ()
  playUntilWin = do
    w <- use winCondition
    case w of
      Won _ -> return ()
      _ -> gameTick >> playUntilWin
