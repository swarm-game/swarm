{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Swarm integration tests
module Main where

import Control.Lens (Ixed (ix), use, view, (&), (.~), (<&>), (^.))
import Control.Monad (filterM, forM_, void)
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Trans.Except (runExceptT)
import Data.Foldable (Foldable (toList), find)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (ParseException, prettyPrintParseException)
import Swarm.Game.CESK (emptyStore, initMachine)
import Swarm.Game.Entity (EntityMap, loadEntities)
import Swarm.Game.Robot (leText, machine, robotLog)
import Swarm.Game.Scenario (Scenario)
import Swarm.Game.State (GameState, WinCondition (Won), initGameStateForScenario, robotMap, winCondition, winSolution)
import Swarm.Game.Step (gameTick)
import qualified Swarm.Language.Context as Ctx
import Swarm.Language.Pipeline (processTerm)
import Swarm.Util.Yaml (decodeFileEitherE)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath.Posix (takeExtension, (</>))
import System.Timeout (timeout)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause, ignoreTestBecause)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)
import Witch (into)

main :: IO ()
main = do
  examplePaths <- acquire "example" "sw"
  scenarioPaths <- acquire "data/scenarios" "yaml"
  scenarioPrograms <- acquire "data/scenarios" "sw"

  entities <- loadEntities
  case entities of
    Left t -> fail $ "Couldn't load entities: " <> into @String t
    Right em -> do
      defaultMain $
        testGroup
          "Tests"
          [ exampleTests examplePaths
          , exampleTests scenarioPrograms
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
  children <- mapM (\path -> (,) path <$> readFile path) filePaths
  -- recurse
  sub <- filterM doesDirectoryExist paths
  transChildren <- concat <$> mapM (`acquire` ext) sub
  return $ children <> transChildren
 where
  hasExt path = takeExtension path == ("." ++ ext)

data Time
  = -- | One second should be enough to run most programs.
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
        [ testSolution Default "02Tutorials/00-move"
        , testSolution Default "02Tutorials/01-turn"
        , testSolution Default "02Tutorials/02-types"
        , testSolution Default "02Tutorials/03-craft"
        , testSolution Default "02Tutorials/04-grab"
        , testSolution Default "02Tutorials/05-place"
        , testSolution Default "02Tutorials/06-bind"
        , testSolution Default "02Tutorials/07-install"
        , testSolution Default "02Tutorials/08-build"
        , ignoreTestBecause "The solution does not seem to work in CI" $
            testSolution' Default "02Tutorials/09-crash" $ \g -> do
              let rs = toList $ g ^. robotMap
              let hints = any (T.isInfixOf "you will win" . view leText) . toList . view robotLog
              let win = isJust $ find hints rs
              assertBool "Could not find a robot with winning instructions!" win
        , testSolution Default "02Tutorials/10-scan"
        ]
    , testGroup
        "Challenges"
        [ testSolution Default "03Challenges/01-chess_horse"
        , testSolution Default "03Challenges/00-test"
        , testSolution Default "03Challenges/03-teleport"
        ]
    , testGroup
        "Regression tests"
        [ expectFailBecause "Awaiting fix (#394)" $
            testSolution Default "04Testing/394-build-drill"
        , testSolution Default "04Testing/428-drowning-destroy"
        ]
    ]
 where
  testSolution :: Time -> FilePath -> TestTree
  testSolution s p = testSolution' s p (const $ pure ())

  testSolution' :: Time -> FilePath -> (GameState -> Assertion) -> TestTree
  testSolution' s p verify = testCase p $ do
    Right gs <- runExceptT $ initGameStateForScenario p Nothing Nothing
    case gs ^. winSolution of
      Nothing -> assertFailure "No solution to test!"
      Just sol -> do
        let gs' = gs & robotMap . ix 0 . machine .~ initMachine sol Ctx.empty emptyStore
        m <- timeout (time s) (snd <$> runStateT playUntilWin gs')
        case m of
          Nothing -> assertFailure "Timed out - this likely means that the solution did not work."
          Just g -> do
            noFatalErrors g
            verify g

  playUntilWin :: StateT GameState IO ()
  playUntilWin = do
    w <- use winCondition
    case w of
      Won _ -> return ()
      _ -> gameTick >> playUntilWin

noFatalErrors :: GameState -> Assertion
noFatalErrors g = do
  let rm = g ^. robotMap
  forM_
    rm
    ( \r -> do
        let f = find isFatal (view leText <$> r ^. robotLog)
        -- -----------------------------------------------
        -- When debugging, try logging all robot messages:
        -- forM_ (r ^. robotLog) (putStrLn . T.unpack . view leText)
        -- -----------------------------------------------
        maybe (return ()) (assertFailure . T.unpack) f
    )
 where
  isFatal = ("Fatal error:" `T.isInfixOf`)
