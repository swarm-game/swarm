{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Lens ((&), (.~))
import Control.Monad (replicateM, replicateM_)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT, execStateT)
import Criterion.Main
import GHC.Int (Int64)
import Linear.V2 (V2 (V2))
import Swarm.Game.CEK (initMachine)
import Swarm.Game.Robot (Robot, mkRobot, systemRobot)
import Swarm.Game.State (GameState, addRobot, initGameState)
import Swarm.Game.Step (gameTick)
import qualified Swarm.Language.Context as Context
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Syntax (Term (TInt))
import System.Random (randomRIO)

treeProgram :: Integer -> Integer -> ProcessedTerm
treeProgram rn1 rn2 =
  [tmQ|
  let repeat : int -> cmd () -> cmd () = \n.\c.
    if (n == 0) {} {c ; repeat (n-1) c}
  in {
    repeat ($int:rn1 + 300) wait;
    appear "|";
    repeat ($int:rn2 + 300) wait;
    place "tree";
    selfdestruct
  }
  |]

-- | Creates a seed robot at location loc which waits rn1 ticks before changing
-- | its appearance, and then rn2 ticks before placing a tree.
mkTreeBot :: V2 Int64 -> Integer -> Integer -> Robot
mkTreeBot loc rn1 rn2 =
  mkRobot "tree" loc (V2 0 0) machine []
    & systemRobot .~ True
 where
  machine = initMachine (treeProgram rn1 rn2) Context.empty

-- | Creates a GameState with numTrees trees with random growing rates.
mkTrees :: Int -> IO GameState
mkTrees numTrees = do
  rn1s <- replicateM numTrees (randomRIO (0, 99))
  rn2s <- replicateM numTrees (randomRIO (0, 99))
  let robots = zipWith3 mkTreeBot [V2 x 0 | x <- [0 ..]] rn1s rn2s
  Right initState <- runExceptT (initGameState 0)
  execStateT (mapM_ addRobot robots) initState

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
