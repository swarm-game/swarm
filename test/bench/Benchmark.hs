{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Control.Carrier.Accum.FixedStrict (runAccum)
import Control.Lens (view, (&), (.~))
import Control.Monad (replicateM_)
import Control.Monad.State (evalStateT, execStateT)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Text qualified as T
import Data.Tuple.Extra (dupe)
import Swarm.Effect (runTimeIO)
import Swarm.Game.CESK (initMachine)
import Swarm.Game.Display (defaultRobotDisplay)
import Swarm.Game.Failure (SystemFailure, simpleErrorHandle)
import Swarm.Game.Location
import Swarm.Game.Robot (TRobot, mkRobot)
import Swarm.Game.Robot.Walk (emptyExceptions)
import Swarm.Game.Scenario (loadStandaloneScenario)
import Swarm.Game.State (GameState, creativeMode, landscape, zoomRobots)
import Swarm.Game.State.Initialize (pureScenarioToGameState)
import Swarm.Game.State.Landscape (multiWorld)
import Swarm.Game.State.Robot (addTRobot)
import Swarm.Game.State.Runtime (RuntimeOptions (..), initRuntimeState, stdGameConfigInputs)
import Swarm.Game.Step (gameTick)
import Swarm.Game.Terrain (blankTerrainIndex)
import Swarm.Game.Universe (Cosmic (..), SubworldName (DefaultRootSubworld))
import Swarm.Game.World (WorldFun (..), newWorld)
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Syntax
import Swarm.Util (parens, showT)
import Swarm.Util.Erasable
import Test.Tasty.Bench (Benchmark, bcompare, bench, bgroup, defaultMain, whnfAppIO)

-- | The program of a robot that does nothing.
idleProgram :: TSyntax
idleProgram = [tmQ| {} |]

-- | The program of a robot which waits a random number of ticks, changes its
--   appearance, then waits another random number of ticks, places a tree, and
--   then self-destructs.
treeProgram :: TSyntax
treeProgram =
  [tmQ|
  {
    r <- random 100;
    wait (r + 300);
    appear "|" (inl ());
    r <- random 100;
    wait (r + 300);
    place "tree";
    selfdestruct
  }
  |]

-- | The program of a robot that moves forward forever.
moverProgram :: TSyntax
moverProgram =
  [tmQ|
    let forever : Cmd Unit -> Cmd Unit = \c. c; forever c
    in forever move
  |]

-- | The program of a robot that moves in circles forever.
circlerProgram :: TSyntax
circlerProgram =
  [tmQ|
    let forever : Cmd Unit -> Cmd Unit = \c. c; forever c
    in forever (
      move;
      turn right;
      move;
      turn right;
      move;
      turn right;
      move;
      turn right;
    )
  |]

-- | The program of a robot that moves back and forth.
--
-- Each robot in a line starts a tick later, forming a wave.
-- See data/scenarios/Challenges/wave.yaml
--
-- This is used to compare the performance degradation caused
-- by using definitions and chains of ifs. Ideally there should
-- not be cost if the code is inlined and simplified. TODO: #1557
waveProgram :: Bool -> TSyntax
waveProgram manualInline =
  let inlineDef = if manualInline then (1 :: Integer) else 0
   in [tmQ|
    def doN = \n. \f. if (n > 0) {f; doN (n - 1) f} {}; end;
    def crossPath =
        if ($int:inlineDef == 0) {
          doN 6 move;
        } {
          move; move; move; move; move; move;
        };
        turn back;
        wait 5;
        end;
    def go =
        crossPath;
        go;
        end;
    def start =
        pos <- whereami;
        wait $ fst pos;
        go;
        end;
    start;
  |]

-- | Initializes a robot with program prog at location loc facing north.
initRobot :: TSyntax -> Location -> TRobot
initRobot prog loc =
  mkRobot
    Nothing
    ""
    mempty
    (Just $ Cosmic DefaultRootSubworld loc)
    north
    defaultRobotDisplay
    (Just prog)
    []
    []
    False
    False
    emptyExceptions
    0

-- | Creates a GameState with numRobot copies of robot on a blank map, aligned
--   in a row starting at (0,0) and spreading east.
mkGameState :: TSyntax -> (Location -> TRobot) -> Int -> IO GameState
mkGameState prog robotMaker numRobots = do
  let robots = [robotMaker (Location (fromIntegral x) 0) | x <- [0 .. numRobots - 1]]

  -- NOTE: This replaces "classicGame0", which is still used by unit tests.
  gs <- simpleErrorHandle $ do
    (_ :: Seq SystemFailure, initRS) <-
      runAccum mempty . initRuntimeState $
        RuntimeOptions {startPaused = False, pauseOnObjectiveCompletion = False, loadTestScenarios = False}
    (scenario, _) <- loadStandaloneScenario "classic"
    return $ pureScenarioToGameState scenario 0 0 Nothing $ view stdGameConfigInputs initRS

  execStateT
    (zoomRobots $ mapM_ (addTRobot $ initMachine prog) robots)
    ( gs
        & creativeMode .~ True
        & landscape . multiWorld .~ M.singleton DefaultRootSubworld (newWorld (WF $ const (blankTerrainIndex, ENothing)))
    )

-- | Runs numGameTicks ticks of the game.
runGame :: Int -> GameState -> IO ()
runGame numGameTicks = evalStateT (replicateM_ numGameTicks $ runTimeIO gameTick)

main :: IO ()
main = do
  idlers <- mkGameStates largeRobotNumbers idleProgram
  trees <- mkGameStates robotNumbers treeProgram
  circlers <- mkGameStates robotNumbers circlerProgram
  movers <- mkGameStates robotNumbers moverProgram
  wavesInlined <- mkGameStates robotNumbers $ waveProgram True
  wavesWithDef <- mkGameStates robotNumbers $ waveProgram False
  -- In theory we should force the evaluation of these game states to normal
  -- form before running the benchmarks. In practice, the first of the many
  -- criterion runs for each of these benchmarks doesn't look like an outlier.
  defaultMain
    [ bgroup
        "run 1000 game ticks"
        [ bgroupTicks "idlers" 10000 idlers
        , bgroupTicks "trees" 1000 trees
        , bgroupTicks "circlers" 1000 circlers
        , bgroupTicks "movers" 1000 movers
        , bgroup
            "waves comparison"
            [ bgroup "wavesInlined" (toBenchmarks 1000 wavesInlined)
            , bgroup
                "wavesWithDef"
                ( zipWith (\i -> bcompare ("wavesInlined." <> show i)) robotNumbers $
                    toBenchmarks 1000 wavesWithDef
                )
            ]
        ]
    ]
 where
  bgroupTicks label ticks bots =
    bgroup newLabel $ toBenchmarks ticks bots
   where
    newLabel =
      unwords
        [ label
        , T.unpack $
            parens $
              T.unwords
                [ showT ticks
                , "ticks"
                ]
        ]

  robotNumbers = [10, 20 .. 40]
  largeRobotNumbers = take 4 $ iterate (* 2) 100

  mkGameStates :: [Int] -> TSyntax -> IO [(Int, GameState)]
  mkGameStates botCounts prog = mapM (traverse (mkGameState prog $ initRobot prog) . dupe) botCounts

  toBenchmarks :: Int -> [(Int, GameState)] -> [Benchmark]
  toBenchmarks tickCount gameStates =
    [ bench (show n) $ whnfAppIO (runGame tickCount) gs
    | (n, gs) <- gameStates
    ]
