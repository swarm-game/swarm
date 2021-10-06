{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (replicateM_)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT, execStateT)
import Criterion.Main (bench, bgroup, defaultMain, whnfIO)
import Linear.V2 (V2 (V2))
import Swarm.Game.CEK (initMachine)
import Swarm.Game.Robot (Robot, mkRobot)
import Swarm.Game.State (GameState, addRobot, initGameState, gameMode, GameMode (Creative))
import Swarm.Game.Step (gameTick)
import qualified Swarm.Language.Context as Context
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Syntax (north)
import Data.Int (Int64)
import Control.Lens ((&), (.~))

-- | The program of a robot which waits a random number of ticks, changes its
--   appearence, then waits another random number of ticks, places a tree, and
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

-- | Initialize a robot with program prog at location loc facing north.
initRobot :: ProcessedTerm -> V2 Int64 -> Robot
initRobot prog loc = mkRobot "" north loc (initMachine prog Context.empty) []

-- | Creates a GameState with numRobot copies of robot, aligned in a row
--   pointing east and starting at (0,0).
mkGameState :: Int -> (V2 Int64 -> Robot) -> IO GameState
mkGameState numRobots robotMaker = do
  let robots = [robotMaker (V2 (fromIntegral x) 0) | x <- [0..numRobots-1]]
  Right initState <- runExceptT (initGameState 0)
  execStateT (mapM addRobot robots) (initState & gameMode .~ Creative)

-- | Creates a GameState with numTrees trees.
mkTrees :: Int -> IO GameState
mkTrees numTrees = mkGameState numTrees (initRobot treeProgram)

-- | Runs numGameTicks ticks of the game.
runGame :: Int -> GameState -> IO ()
runGame numGameTicks = evalStateT (replicateM_ numGameTicks gameTick)

main :: IO ()
main = do
  trees10 <- mkTrees 10
  trees20 <- mkTrees 20
  trees30 <- mkTrees 30
  -- In theory we should force the evaluation of these game states to normal
  -- form before running the benchmarks. In practice, the first of the many
  -- criterion runs for each of these benchmarks doesn't look like an outlier.
  defaultMain
    [ bgroup
        "run 1000 game ticks"
        [ bench "10 trees" $ whnfIO (runGame 1000 trees10)
        , bench "20 trees" $ whnfIO (runGame 1000 trees20)
        , bench "30 trees" $ whnfIO (runGame 1000 trees30)
        ]
    ]
