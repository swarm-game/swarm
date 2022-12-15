{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Lens ((&), (.~))
import Control.Monad (replicateM_)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT, execStateT)
import Criterion.Main (Benchmark, bench, bgroup, defaultConfig, defaultMainWith, whnfAppIO)
import Criterion.Types (Config (timeLimit))
import Data.Int (Int32)
import Linear.V2 (V2 (V2))
import Swarm.Game.CESK (emptyStore, initMachine)
import Swarm.Game.Display (defaultRobotDisplay)
import Swarm.Game.Robot (TRobot, mkRobot)
import Swarm.Game.State (GameState, addTRobot, classicGame0, creativeMode, world)
import Swarm.Game.Step (gameTick)
import Swarm.Game.Terrain (TerrainType (DirtT))
import Swarm.Game.World (WorldFun (..), newWorld)
import Swarm.Language.Context qualified as Context
import Swarm.Language.Pipeline (ProcessedTerm)
import Swarm.Language.Pipeline.QQ (tmQ)
import Swarm.Language.Syntax (north)
import Swarm.Util.Location

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

-- | Initializes a robot with program prog at location loc facing north.
initRobot :: ProcessedTerm -> Location -> TRobot
initRobot prog loc = mkRobot () Nothing "" [] (Just loc) north defaultRobotDisplay (initMachine prog Context.empty emptyStore) [] [] False False 0

-- | Creates a GameState with numRobot copies of robot on a blank map, aligned
--   in a row starting at (0,0) and spreading east.
mkGameState :: (Location -> TRobot) -> Int -> IO GameState
mkGameState robotMaker numRobots = do
  let robots = [robotMaker (Location (fromIntegral x) 0) | x <- [0 .. numRobots - 1]]
  Right initState <- runExceptT classicGame0
  execStateT
    (mapM addTRobot robots)
    ( initState
        & creativeMode .~ True
        & world .~ newWorld (WF $ const (fromEnum DirtT, Nothing))
    )

-- | Runs numGameTicks ticks of the game.
runGame :: Int -> GameState -> IO ()
runGame numGameTicks = evalStateT (replicateM_ numGameTicks gameTick)

main :: IO ()
main = do
  idlers <- mkGameStates idleProgram [10, 20 .. 40]
  trees <- mkGameStates treeProgram [10, 20 .. 40]
  circlers <- mkGameStates circlerProgram [10, 20 .. 40]
  movers <- mkGameStates moverProgram [10, 20 .. 40]
  -- In theory we should force the evaluation of these game states to normal
  -- form before running the benchmarks. In practice, the first of the many
  -- criterion runs for each of these benchmarks doesn't look like an outlier.
  defaultMainWith
    (defaultConfig {timeLimit = 10})
    [ bgroup
        "run 1000 game ticks"
        [ bgroup "idlers" (toBenchmarks idlers)
        , bgroup "trees" (toBenchmarks trees)
        , bgroup "circlers" (toBenchmarks circlers)
        , bgroup "movers" (toBenchmarks movers)
        ]
    ]
 where
  mkGameStates :: ProcessedTerm -> [Int] -> IO [(Int, GameState)]
  mkGameStates prog sizes = zip sizes <$> mapM (mkGameState (initRobot prog)) sizes

  toBenchmarks :: [(Int, GameState)] -> [Benchmark]
  toBenchmarks gameStates =
    [ bench (show n) $ whnfAppIO (runGame 1000) gameState
    | (n, gameState) <- gameStates
    ]
