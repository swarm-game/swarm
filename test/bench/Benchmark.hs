{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Control.Lens ((&), (.~), (^.))
import Control.Monad (replicateM_)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT, execStateT)
import Data.Map qualified as M
import Swarm.Game.CESK (emptyStore, initMachine)
import Swarm.Game.Display (defaultRobotDisplay)
import Swarm.Game.Location
import Swarm.Game.Robot (TRobot, mkRobot)
import Swarm.Game.State (GameState, addTRobot, creativeMode, landscape, multiWorld)
import Swarm.Game.Step (gameTick)
import Swarm.Game.Terrain (TerrainType (DirtT))
import Swarm.Game.Universe (Cosmic (..), SubworldName (DefaultRootSubworld))
import Swarm.Game.World (WorldFun (..), newWorld)
import Swarm.Language.Context qualified as Context
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Syntax
import Swarm.TUI.Model (gameState)
import Swarm.TUI.Model.StateUpdate (classicGame0)
import Swarm.Util.Erasable
import Test.Tasty.Bench (Benchmark, bcompare, bench, bgroup, defaultMain, whnfAppIO)

-- | The program of a robot that does nothing.
idleProgram :: ProcessedTerm
idleProgram = [tmQ| {} |]

-- | The program of a robot which waits a random number of ticks, changes its
--   appearance, then waits another random number of ticks, places a tree, and
--   then self-destructs.
treeProgram :: ProcessedTerm
treeProgram =
  [tmQ|
  {
    r <- random 100;
    wait (r + 300);
    appear "|";
    r <- random 100;
    wait (r + 300);
    place "tree";
    selfdestruct
  }
  |]

-- | The program of a robot that moves forward forever.
moverProgram :: ProcessedTerm
moverProgram =
  [tmQ|
    let forever : cmd unit -> cmd unit = \c. c; forever c
    in forever move
  |]

-- | The program of a robot that moves in circles forever.
circlerProgram :: ProcessedTerm
circlerProgram =
  [tmQ|
    let forever : cmd unit -> cmd unit = \c. c; forever c
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
waveProgram :: Bool -> ProcessedTerm
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
initRobot :: ProcessedTerm -> Location -> TRobot
initRobot prog loc = mkRobot () Nothing "" mempty (Just $ Cosmic DefaultRootSubworld loc) north defaultRobotDisplay (initMachine prog Context.empty emptyStore) [] [] False False mempty 0

-- | Creates a GameState with numRobot copies of robot on a blank map, aligned
--   in a row starting at (0,0) and spreading east.
mkGameState :: (Location -> TRobot) -> Int -> IO GameState
mkGameState robotMaker numRobots = do
  let robots = [robotMaker (Location (fromIntegral x) 0) | x <- [0 .. numRobots - 1]]
  Right initAppState <- runExceptT classicGame0
  execStateT
    (mapM addTRobot robots)
    ( (initAppState ^. gameState)
        & creativeMode .~ True
        & landscape . multiWorld .~ M.singleton DefaultRootSubworld (newWorld (WF $ const (fromEnum DirtT, ENothing)))
    )

-- | Runs numGameTicks ticks of the game.
runGame :: Int -> GameState -> IO ()
runGame numGameTicks = evalStateT (replicateM_ numGameTicks gameTick)

main :: IO ()
main = do
  idlers <- mkGameStates idleProgram
  trees <- mkGameStates treeProgram
  circlers <- mkGameStates circlerProgram
  movers <- mkGameStates moverProgram
  wavesInlined <- mkGameStates (waveProgram True)
  wavesWithDef <- mkGameStates (waveProgram False)
  -- In theory we should force the evaluation of these game states to normal
  -- form before running the benchmarks. In practice, the first of the many
  -- criterion runs for each of these benchmarks doesn't look like an outlier.
  defaultMain
    [ bgroup
        "run 1000 game ticks"
        [ bgroup "idlers" (toBenchmarks idlers)
        , bgroup "trees" (toBenchmarks trees)
        , bgroup "circlers" (toBenchmarks circlers)
        , bgroup "movers" (toBenchmarks movers)
        , bgroup "wavesInlined" (toBenchmarks wavesInlined)
        , bgroup
            "wavesWithDef"
            ( zipWith (\i -> bcompare ("wavesInlined." <> show i)) robotNumbers $
                toBenchmarks wavesWithDef
            )
        ]
    ]
 where
  robotNumbers = [10, 20 .. 40]

  mkGameStates :: ProcessedTerm -> IO [(Int, GameState)]
  mkGameStates prog = zip robotNumbers <$> mapM (mkGameState (initRobot prog)) robotNumbers

  toBenchmarks :: [(Int, GameState)] -> [Benchmark]
  toBenchmarks gameStates =
    [ bench (show n) $ whnfAppIO (runGame 1000) gs
    | (n, gs) <- gameStates
    ]
