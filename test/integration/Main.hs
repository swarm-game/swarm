{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm integration tests
module Main where

import Control.Carrier.Lift (runM)
import Control.Carrier.Throw.Either (runThrow)
import Control.Lens (Ixed (ix), at, to, view, (&), (.~), (^.), (^..), (^?), (^?!))
import Control.Monad (forM_, unless, when)
import Control.Monad.State (execStateT)
import Data.Char (isSpace)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (Foldable (toList), find)
import Data.Function (on)
import Data.IntSet qualified as IS
import Data.List (isPrefixOf, isSuffixOf, partition)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml (ParseException, decodeFileEither, prettyPrintParseException)
import Swarm.Doc.Keyword (EditorType (..))
import Swarm.Doc.Keyword qualified as Keyword
import Swarm.Failure (SystemFailure)
import Swarm.Game.Achievement.Definitions (GameplayAchievement (..))
import Swarm.Game.CESK (initMachine)
import Swarm.Game.Entity (lookupByName)
import Swarm.Game.Robot (equippedDevices, robotName, systemRobot)
import Swarm.Game.Robot.Activity (commandsHistogram, lifetimeStepCount, tangibleCommandCount)
import Swarm.Game.Robot.Concrete (activityCounts, machine, robotLog, waitingUntil)
import Swarm.Game.Scenario (Scenario, ScenarioInputs (..), gsiScenarioInputs)
import Swarm.Game.Scenario.Scoring.GenericMetrics (Metric (..), Progress (..))
import Swarm.Game.ScenarioInfo (ScenarioInfo, ScenarioStatus (..), scenarioStatus)
import Swarm.Game.State (
  GameState,
  baseRobot,
  discovery,
  messageInfo,
  pathCaching,
  robotInfo,
  temporal,
  winSolution,
 )
import Swarm.Game.State.Robot (
  activeRobots,
  robotMap,
  waitingRobots,
 )
import Swarm.Game.State.Runtime (
  RuntimeState,
  eventLog,
  metrics,
  stdGameConfigInputs,
 )
import Swarm.Game.State.Substate (
  gameAchievements,
  initState,
  messageQueue,
  notificationsContent,
  ticks,
 )
import Swarm.Game.Step.Path.Type
import Swarm.Game.Step.Validate (badErrorsInLogs, playUntilWin)
import Swarm.Game.Tick (getTickNumber)
import Swarm.Language.Pipeline (processTerm)
import Swarm.Log
import Swarm.Pretty (prettyString)
import Swarm.TUI.Model (
  debugOptions,
  defaultAppOpts,
  gameState,
  playState,
  runtimeState,
  scenarioState,
  userScenario,
 )
import Swarm.TUI.Model.DebugOption (DebugOption (LoadTestingScenarios))
import Swarm.TUI.Model.StateUpdate (PersistentState (..), constructAppState, initPersistentState)
import Swarm.Util (Encoding (..), allPairs, applyWhen, findAllWithExt, readFileMayT)
import Swarm.Util.Effect ((???))
import Swarm.Util.RingBuffer qualified as RB
import Swarm.Util.Yaml (decodeFileEitherE)
import System.FilePath (normalise, splitDirectories, (<.>), (</>))
import System.Metrics qualified as Metrics (Store, newStore)
import System.Timeout (timeout)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestFormat
import TestRecipeCoverage
import Witch (into)

isUnparseableTest :: FilePath -> Bool
isUnparseableTest fp = "_Validation" `elem` splitDirectories fp

main :: IO ()
main = do
  examplePaths <- findAllWithExt "example" "sw"
  scenarioPaths <- findAllWithExt "data/scenarios" "yaml"
  let (unparseableScenarios, parseableScenarios) = partition isUnparseableTest scenarioPaths
  scenarioPrograms <- findAllWithExt "data/scenarios" "sw"
  PersistentState rs ui key progState <- do
    let testingOptions = defaultAppOpts {debugOptions = S.singleton LoadTestingScenarios}
    out <- runM . runThrow @SystemFailure $ initPersistentState testingOptions
    either (assertFailure . prettyString) return out
  let scenarioInputs = gsiScenarioInputs $ initState $ rs ^. stdGameConfigInputs
      rs' = rs & eventLog .~ mempty
  recipeTests <- testRecipeCoverage
  formatTests <- testFormatting
  defaultMain $
    testGroup
      "Tests"
      [ testNoLoadingErrors rs
      , exampleTests examplePaths
      , exampleTests scenarioPrograms
      , scenarioParseTests scenarioInputs parseableScenarios
      , scenarioParseInvalidTests scenarioInputs unparseableScenarios
      , formatTests
      , noScenarioOverlap
      , testScenarioSolutions scenarioPaths $ PersistentState rs' ui key progState
      , testEditorFiles
      , recipeTests
      , saveFileTests
      ]

testNoLoadingErrors :: RuntimeState -> TestTree
testNoLoadingErrors r =
  testCase "Test runtime log does not contain errors" (checkNoRuntimeErrors r)

checkNoRuntimeErrors :: RuntimeState -> IO ()
checkNoRuntimeErrors r =
  forM_ (r ^. eventLog . notificationsContent) $ \e ->
    when (isError e) $
      assertFailure $
        show (e ^. leSeverity) <> " was produced during loading: " <> T.unpack (e ^. leText)

isError :: LogEntry -> Bool
isError = (>= Warning) . view leSeverity

exampleTests :: [FilePath] -> TestTree
exampleTests = testGroup "Process .sw files" . map exampleTest

exampleTest :: FilePath -> TestTree
exampleTest path =
  testCase ("processTerm for contents of " ++ show path) $ do
    content <- readFileMayT UTF8 path ??? assertFailure "Can't read file!"
    let value = processTerm content
    either (assertFailure . into @String) (const $ return ()) value

scenarioParseTests :: ScenarioInputs -> [FilePath] -> TestTree
scenarioParseTests scenarioInputs inputs =
  testGroup
    "Test scenarios parse"
    (map (scenarioParseTest Parsed scenarioInputs) inputs)

scenarioParseInvalidTests :: ScenarioInputs -> [FilePath] -> TestTree
scenarioParseInvalidTests scenarioInputs inputs =
  testGroup
    "Test invalid scenarios fail to parse"
    (map (scenarioParseTest Failed scenarioInputs) inputs)

data ParseResult = Parsed | Failed

scenarioParseTest :: ParseResult -> ScenarioInputs -> FilePath -> TestTree
scenarioParseTest expRes scenarioInputs path =
  testCase ("parse scenario " ++ show path) (getScenario expRes scenarioInputs path)

getScenario :: ParseResult -> ScenarioInputs -> FilePath -> IO ()
getScenario expRes scenarioInputs p = do
  res <- decodeFileEitherE scenarioInputs p :: IO (Either ParseException Scenario)
  case expRes of
    Parsed -> case res of
      Left err -> assertFailure $ prettyPrintParseException err
      Right _s -> return ()
    Failed -> forM_ res $ const $ assertFailure "Unexpectedly parsed invalid scenario!"

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

data ShouldCheckBadErrors = CheckForBadErrors | AllowBadErrors deriving (Eq, Show)

-- | Different types of scenario integration tests.
data TestType where
  -- | Default test with 1s timeout and standard checks.
  DefaultTest :: ScenarioType -> TestType
  -- | Custom timeout with standard checks.
  Timed :: ScenarioType -> Time -> TestType
  -- | Custom timeout and customized checks.
  Special :: ScenarioType -> Time -> ShouldCheckBadErrors -> (GameState -> Assertion) -> TestType

-- | Tutorial scenarios are tested differently than other scenarios.
data ScenarioType
  = Tutorial
  | NonTutorial

-- | Configuration record specifying how to test a particular scenario.
data ScenarioTestConfig
  = ScenarioTestConfig
  { allowedTime :: Time
  , scenarioPath :: FilePath
  , checkBadErrors :: ShouldCheckBadErrors
  , checks :: GameState -> Assertion
  }

-- | Compare 'ScenarioTestConfig' values by scenario path.
instance Eq ScenarioTestConfig where
  (==) = (==) `on` scenarioPath

-- | Compare 'ScenarioTestConfig' values by scenario path.
instance Ord ScenarioTestConfig where
  compare = compare `on` scenarioPath

scenarioTest :: FilePath -> TestType -> ScenarioTestSet
scenarioTest file testType = singletonTest $ case testType of
  DefaultTest stype -> ScenarioTestConfig Default file' CheckForBadErrors (gameStateCheck stype)
  Timed stype timed -> ScenarioTestConfig timed file' CheckForBadErrors (gameStateCheck stype)
  Special stype timed shouldCheckBadErrors verify ->
    ScenarioTestConfig timed file' shouldCheckBadErrors $ \g ->
      gameStateCheck stype g >> verify g
 where
  file' = normalizePath file

-- | A set of Scenario test configurations. The semigroup and monoid instances are left-biased.
newtype ScenarioTestSet = ScenarioTestSet {getScenarioTestSet :: S.Set ScenarioTestConfig}
  deriving newtype (Semigroup, Monoid)

singletonTest :: ScenarioTestConfig -> ScenarioTestSet
singletonTest = ScenarioTestSet . S.singleton

isTutorial :: FilePath -> ScenarioType
isTutorial f
  | "Tutorials" `elem` splitDirectories f = Tutorial
  | otherwise = NonTutorial

gameStateCheck :: ScenarioType -> (GameState -> Assertion)
gameStateCheck = \case
  Tutorial -> tutorialHasLog
  NonTutorial -> const $ pure ()

mkTests :: PersistentState -> ScenarioTestSet -> TestTree
mkTests ps (ScenarioTestSet list) = testGroup "Test scenario solutions" . map (testSolution ps) . S.toList $ list

testSolution :: PersistentState -> ScenarioTestConfig -> TestTree
testSolution ps (ScenarioTestConfig s p shouldCheckBadErrors verify) = maybeExpectFail . testCase p $ do
  cleanStore <- Metrics.newStore
  out <-
    runM . runThrow @SystemFailure $
      constructAppState
        (resetMetrics cleanStore ps)
        (defaultAppOpts {userScenario = Just p})
        Nothing
  case out of
    Left err -> assertFailure $ prettyString err
    Right appState -> case appState ^. playState . scenarioState . gameState . winSolution of
      Nothing -> assertFailure "No solution to test!"
      Just sol -> do
        when (shouldCheckBadErrors == CheckForBadErrors) (checkNoRuntimeErrors $ appState ^. runtimeState)
        let gs' =
              (appState ^. playState . scenarioState . gameState)
                & baseRobot . machine .~ initMachine sol
        m <- timeout (time s) (execStateT playUntilWin gs')
        case m of
          Nothing -> assertFailure "Timed out - this likely means that the solution did not work."
          Just g -> do
            -- When debugging, try logging all robot messages.
            -- printAllLogs
            when (shouldCheckBadErrors == CheckForBadErrors) $ case noBadErrors g of
              Left x -> assertFailure $ T.unpack x
              _ -> return ()
            verify g
 where
  maybeExpectFail = maybe id expectFailBecause $ M.lookup p expectFailScenarios

tutorialHasLog :: GameState -> Assertion
tutorialHasLog gs =
  let baseDevs = gs ^?! baseRobot . equippedDevices
   in assertBool "Base should have a logger installed!" (not . null $ lookupByName "logger" baseDevs)

normalizePath :: FilePath -> FilePath
normalizePath = normalizePrefix . normalizeSuffix . normalise
 where
  normalizePrefix fp
    | ["data", "scenarios"] `isPrefixOf` splitDirectories fp = fp
    | otherwise = "data" </> "scenarios" </> fp
  normalizeSuffix fp
    | ".yaml" `isSuffixOf` fp = fp
    | otherwise = fp <.> "yaml"

-- | Scenarios with no solution, which should not be tested.  These
--   are listed explicitly so that it's not possible to simply forget
--   to add a solution for a new scenario; one must explicitly choose
--   to add it to this list if no solution is intended.
noSolutionScenarios :: S.Set FilePath
noSolutionScenarios =
  S.fromList . map normalizePath $
    [ "blank"
    , "classic"
    , "creative"
    , "World Examples/clearing"
    , "World Examples/rorschach"
    , "World Examples/stretch"
    , "World Examples/translate"
    , "Vignettes/roadway"
    , "Fun/GoL"
    , "Fun/logo-burst"
    , "Fun/spider"
    , "Speedruns/curry"
    , "Speedruns/forester"
    , "Speedruns/mithril"
    , "Testing/1138-structures/flip-and-rotate"
    , "Testing/1138-structures/nested-structure"
    , "Testing/1138-structures/sibling-precedence"
    , "Testing/1262-display-device-commands"
    , "Testing/1356-portals/automatic-waypoint-patrol"
    , "Testing/144-subworlds/spatial-consistency-enforcement"
    , "Testing/1634-message-colors"
    , "Testing/1780-structure-merge-expansion/nonoverlapping-structure-merge"
    , "Testing/1780-structure-merge-expansion/root-map-expansion"
    , "Testing/1780-structure-merge-expansion/structure-composition"
    , "Testing/2193-text-entities"
    , "Testing/1356-portals/portals-and-waypoints"
    ]

-- | Scenarios that are currently expected to fail for some reason.
--
--   Note, scenarios that are constructed in such a way that the
--   objective is not completed (i.e. testing that a certain
--   restriction works, or that a certain solution is not allowed)
--   should use a custom test.  This group is only for scenarios that
--   are currently failing but ideally should not, perhaps pending a
--   fix.
expectFailScenarios :: M.Map FilePath String
expectFailScenarios =
  M.fromList
    [ "Testing/231-requirements/231-command-transformer-reqs" ==> "Awaiting fix for #231"
    , "Challenges/Logic/wolf-goat-cabbage" ==> "Awaiting fix for #2602"
    ]
 where
  f ==> e = (normalizePath f, e)

-- | Scenarios that should be tested using default checks, but need a
--   bit more than the standard 1 second to complete.
customTimeoutScenarios :: ScenarioTestSet
customTimeoutScenarios =
  mconcat
    [ "Tutorials/move" ==> 10
    , "Tutorials/stock" ==> 3
    , "Tutorials/farming" ==> 5
    , "Fun/snake" ==> 20
    , "Fun/horton" ==> 10
    , "Challenges/Algorithmic/2048" ==> 10
    , "Challenges/Algorithmic/word-search" ==> 6
    , "Challenges/Story/bridge-building" ==> 10
    , "Challenges/Story/ice-cream" ==> 5
    , "Challenges/Algorithmic/combo-lock" ==> 10
    , "Challenges/Mechanics/wave" ==> 15
    , "Challenges/Story/arbitrage" ==> 3
    , "Challenges/Mechanics/bucket-brigade" ==> 5
    , "Challenges/Story/gopher" ==> 20
    , "Challenges/Story/hackman" ==> 5
    , "Challenges/Story/blender" ==> 5
    , "Challenges/Story/dna" ==> 20
    , "Challenges/Logic/hanoi" ==> 10
    , "Challenges/Logic/lights-out" ==> 3
    , "Challenges/Logic/Sliding Puzzles/3x3" ==> 40
    , "Challenges/Algorithmic/dimsum" ==> 10
    , "Challenges/Algorithmic/gallery" ==> 20
    , "Challenges/Mechanics/telephone" ==> 20
    , "Challenges/Story/flower-count" ==> 20
    , "Challenges/Mechanics/photocopier" ==> 30
    , "Challenges/Mazes/invisible_maze" ==> 2
    , "Challenges/Story/Ranching/beekeeping" ==> 60
    , "Challenges/Story/Ranching/powerset" ==> 20
    , "Challenges/Story/Ranching/fishing" ==> 10
    , "Challenges/Story/Ranching/gated-paddock" ==> 30
    , "Challenges/Story/Ranching/pied-piper" ==> 30
    , "Testing/479-atomic-race" ==> 2
    , "Testing/479-atomic" ==> 5
    , "Testing/490-harvest" ==> 5
    , "Testing/562-lodestone" ==> 2
    , "Testing/1024-sand" ==> 2
    , "Testing/1533-sow-command" ==> 10
    , "Testing/836-pathfinding/836-no-path-exists2" ==> 10
    , "Testing/836-pathfinding/836-automatic-waypoint-navigation" ==> 3
    ]
 where
  p ==> n = let p' = normalizePath p in scenarioTest p' (Timed (isTutorial p') (Sec n))

-- | Scenarios with custom testing code.
customTestScenarios :: ScenarioTestSet
customTestScenarios =
  mconcat
    [ scenarioTest "Tutorials/debug" $ Special Tutorial Default CheckForBadErrors $ \g -> do
        -- printAllLogs g
        let robots = toList $ g ^. robotInfo . robotMap
            hints = any (T.isInfixOf "you will win" . view leText) . toList . view robotLog
            win = isJust $ find hints robots
        assertBool "Could not find a robot with winning instructions!" win
    , scenarioTest "Testing/Achievements/RobotIntoWater" $ Special NonTutorial Default CheckForBadErrors $ \g ->
        assertBool
          "Did not get RobotIntoWater achievement!"
          (isJust $ g ^? discovery . gameAchievements . at RobotIntoWater)
    , scenarioTest "Testing/475-wait-one" $ Special NonTutorial Default CheckForBadErrors $ \g -> do
        let t = g ^. temporal . ticks
            r1Waits = g ^?! robotInfo . robotMap . ix 1 . to waitingUntil
            active = IS.member 1 $ g ^. robotInfo . activeRobots
            waiting = elem 1 . concat . toList $ g ^. robotInfo . waitingRobots
        assertBool "The game should only take two ticks" $ getTickNumber t == 2
        assertBool "Robot 1 should have waiting machine" $ isJust r1Waits
        assertBool "Robot 1 should be still active" active
        assertBool "Robot 1 should not be in waiting set" $ not waiting
    , scenarioTest "Testing/397-wrong-missing" $ Special NonTutorial Default CheckForBadErrors $ \g -> do
        let msgs =
              (g ^. messageInfo . messageQueue . to logToText)
                <> (g ^.. robotInfo . robotMap . traverse . robotLog . to logToText . traverse)

        assertBool "Should be some messages" (not (null msgs))
        assertBool "Error messages should not mention treads" $
          not (any ("treads" `T.isInfixOf`) msgs)
        assertBool "Error message should mention GPS receiver" $
          any ("GPS receiver" `T.isInfixOf`) msgs
    , scenarioTest "Testing/1569-pathfinding-cache/1569-cache-invalidation-modes" $ Special NonTutorial Default CheckForBadErrors $ \g -> do
        let cachingLog = g ^. pathCaching . pathCachingLog
            actualEntries = map (\(CacheLogEntry _ x) -> x) $ toList $ RB.getValues cachingLog
            expectedEntries =
              [ RetrievalAttempt (RecomputationRequired NotCached)
              , Invalidate UnwalkableOntoPath
              , RetrievalAttempt (RecomputationRequired NotCached)
              , RetrievalAttempt Success
              , Invalidate UnwalkableRemoved
              , RetrievalAttempt (RecomputationRequired NotCached)
              , Invalidate TargetEntityAddedOutsidePath
              , RetrievalAttempt (RecomputationRequired NotCached)
              , Preserve PathTruncated
              , RetrievalAttempt Success
              ]
        assertEqual "Incorrect sequence of invalidations!" expectedEntries actualEntries
    , scenarioTest "Testing/1569-pathfinding-cache/1569-cache-invalidation-distance-limit" $ Special NonTutorial Default CheckForBadErrors $ \g -> do
        let cachingLog = g ^. pathCaching . pathCachingLog
            actualEntries = map (\(CacheLogEntry _ x) -> x) $ toList $ RB.getValues cachingLog
            expectedEntries =
              [ RetrievalAttempt (RecomputationRequired NotCached)
              , RetrievalAttempt Success
              , RetrievalAttempt Success
              , RetrievalAttempt Success
              , RetrievalAttempt (RecomputationRequired PositionOutsidePath)
              , RetrievalAttempt Success
              , RetrievalAttempt (RecomputationRequired (DifferentArg (NewDistanceLimit LimitIncreased)))
              , RetrievalAttempt Success
              , RetrievalAttempt (RecomputationRequired (DifferentArg (NewDistanceLimit PathExceededLimit)))
              , RetrievalAttempt Success
              ]
        assertEqual "Incorrect sequence of invalidations!" expectedEntries actualEntries
    , scenarioTest "Testing/1430-built-robot-ownership" $ Special NonTutorial Default CheckForBadErrors $ \g -> do
        let r2 = g ^. robotInfo . robotMap . at 2
        let r3 = g ^. robotInfo . robotMap . at 3
        assertBool "The second built robot should be a system robot like it's parent." $
          maybe False (view systemRobot) r2
        assertBool "The third built robot should be a normal robot like base." $
          maybe False (not . view systemRobot) r3
    , scenarioTest "Testing/1341-command-count" $ Special NonTutorial Default CheckForBadErrors $ \g ->
        case g ^. robotInfo . robotMap . at 0 of
          Nothing -> assertFailure "No base bot!"
          Just base -> do
            let counters = base ^. activityCounts
            -- NOTE: The values of 7 and 10 for "tangible" and "total" command counts
            -- make sense from the test program and match the F2 screen upon winning the scenario.
            -- However, the F2 dialog actually shows 64 for the step count. This test was
            -- hardcoded to 62 just to make it pass.
            assertEqual "Incorrect tangible command count." 7 $ view tangibleCommandCount counters
            assertEqual "Incorrect command count." 10 $ sum . M.elems $ view commandsHistogram counters
            assertEqual "Incorrect step count." 62 $ view lifetimeStepCount counters
    , scenarioTest "Testing/2240-overridden-entity-capabilities" $ Special NonTutorial Default CheckForBadErrors $ \g -> do
        let msgs = g ^.. robotInfo . robotMap . traverse . robotLog . to logToText . traverse
        assertBool "Error message should mention tank treads but not treads" $
          not (any ("- treads" `T.isInfixOf`) msgs)
            && any ("- tank treads" `T.isInfixOf`) msgs
    , scenarioTest "Testing/1592-shared-template-robot-say-logs" $ Special NonTutorial Default CheckForBadErrors $ \g -> do
        let baseLogs = g ^.. baseRobot . robotLog . to logToText . traverse
        -- printAllLogs g
        assertEqual
          "There should be 6 logs from all of the robots saying things at once!"
          (length baseLogs)
          6 -- the final OK said by base happens after win, and is for debugging
    ]

-- Ensure that none of the explicitly declared custom scenario test
-- categories overlap.  Any given scenario may be in at most one of
-- the special categories.
noScenarioOverlap :: TestTree
noScenarioOverlap =
  testGroup "Ensure custom scenario test categories do not overlap" $
    map (uncurry pairTest) (allPairs categories)
 where
  pairTest (c1, s1) (c2, s2) =
    testCase ("[" ++ c1 ++ "] and [" ++ c2 ++ "] do not overlap") $
      assertEqual "Non-null intersection!" S.empty (s1 `S.intersection` s2)

  categories :: [(String, S.Set FilePath)]
  categories =
    [ ("No solutions", noSolutionScenarios)
    , ("Expected failures", M.keysSet expectFailScenarios)
    , ("Custom timeouts", S.map getFP (getScenarioTestSet customTimeoutScenarios))
    , ("Custom tests", S.map getFP (getScenarioTestSet customTestScenarios))
    ]

  getFP (ScenarioTestConfig _ fp _ _) = fp

testScenarioSolutions :: [FilePath] -> PersistentState -> TestTree
testScenarioSolutions scenarios ps = mkTests ps $ customTests <> defaultTests
 where
  customTests = customTestScenarios <> customTimeoutScenarios

  defaultTests = foldMap mkDefaultTest (filter shouldTest scenarios)
  mkDefaultTest s = scenarioTest s (DefaultTest $ isTutorial s)

  shouldTest s = s `S.notMember` noSolutionScenarios && not (isUnparseableTest s)

resetMetrics :: Metrics.Store -> PersistentState -> PersistentState
resetMetrics s (PersistentState r u k p) =
  let newMetrics = metrics .~ s
   in PersistentState (newMetrics r) u k p

noBadErrors :: GameState -> Either T.Text ()
noBadErrors g =
  unless (null bad) (Left . T.unlines . take 5 $ nubOrd bad)
 where
  bad = badErrorsInLogs g

printAllLogs :: GameState -> IO ()
printAllLogs g =
  forM_ (g ^. robotInfo . robotMap) $ \r -> do
    putStrLn $ "-- ROBOT: " ++ T.unpack (r ^. robotName)
    forM_ (r ^. robotLog) (putStrLn . T.unpack . view leText)

-- | Test that editor files are up-to-date.
testEditorFiles :: TestTree
testEditorFiles =
  testGroup
    "editors"
    [ testGroup
        "VS Code"
        [ testTextInVSCode "operators" Keyword.operatorNames
        , testTextInVSCode "builtin" Keyword.builtinFunctionList
        , testTextInVSCode "commands" Keyword.keywordsCommands
        , testTextInVSCode "directions" Keyword.keywordsDirections
        ]
    , testGroup
        "Emacs"
        [ testTextInEmacs "operators" Keyword.operatorNames
        , testTextInEmacs "builtin" Keyword.builtinFunctionList
        , testTextInEmacs "commands" Keyword.keywordsCommands
        , testTextInEmacs "directions" Keyword.keywordsDirections
        ]
    , testGroup
        "Vim"
        [ testTextInVim "operators" Keyword.operatorNames
        , testTextInVim "builtin" Keyword.builtinFunctionList
        , testTextInVim "commands" Keyword.keywordsCommands
        , testTextInVim "directions" Keyword.keywordsDirections
        ]
    ]
 where
  testTextInVSCode name tf = testTextInFile False name (tf VSCode) "editors/vscode/syntaxes/swarm.tmLanguage.yaml"
  testTextInEmacs name tf = testTextInFile True name (tf Emacs) "editors/emacs/swarm-mode.el"
  testTextInVim name tf = testTextInFile False name (tf Vim) "editors/vim/swarm.vim"
  testTextInFile :: Bool -> String -> Text -> FilePath -> TestTree
  testTextInFile whitespace name t fp = testCase name $ do
    let removeLW' = T.unlines . map (T.dropWhile isSpace) . T.lines
        removeLW = applyWhen whitespace removeLW'
    f <- maybe (assertFailure "Can't read file!") pure =<< readFileMayT UTF8 fp
    assertBool
      ( "EDITOR FILE IS NOT UP TO DATE!\n"
          <> "I could not find the text:\n"
          <> T.unpack t
          <> "\nin file "
          <> fp
      )
      (removeLW t `T.isInfixOf` removeLW f)

saveFileTests :: TestTree
saveFileTests =
  testGroup
    "Save files"
    [ checkLoaded "backstory" "0.6.0.0"
    , checkLoaded "backstory" "latest"
    ]
 where
  checkLoaded scenario version = testCase ("save from version " <> version) $ do
    ef <- decodeFileEither @ScenarioInfo $ "data/test/saves/" <> scenario <> "-" <> version <> ".yaml"
    case ef of
      Left e -> assertFailure $ prettyPrintParseException e
      Right si -> case si ^. scenarioStatus of
        Played _par (Metric Completed _) _best -> pure ()
        other -> assertFailure $ "scenario save file loaded wrong - contains: " <> show other
