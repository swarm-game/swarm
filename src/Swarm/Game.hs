{-# LANGUAGE TemplateHaskell #-}

module Swarm.Game
  ( module Swarm.Game
  , module Swarm.Game.Resource
  )
  where

import           Control.Lens
import           Control.Monad.State
import           Data.Hash.Murmur
import           Data.List.Split     (chunksOf)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (catMaybes)
import           Linear
import           System.Random
import           Witch

import           Swarm.AST
import           Swarm.Game.Resource
import qualified Swarm.Game.World    as W

data Robot = Robot
  { _location     :: V2 Int
  , _direction    :: V2 Int
  , _robotProgram :: Program
  , _static       :: Bool
  }
  deriving (Eq, Ord, Show)

mkBase :: Command -> Robot
mkBase cmd = Robot (V2 0 0) (V2 0 0) [cmd] True

data Item = Resource Char
  deriving (Eq, Ord, Show)

data GameState = GameState
  { _robots     :: [Robot]
  , _newRobots  :: [Robot]
  , _world      :: W.World
  , _viewCenter :: V2 Int
  , _inventory  :: Map Item Int
  }

-- initRs = 50
-- initCs = 50

initGameState :: IO GameState
initGameState = do
  return $
    GameState [] []
      (W.newWorld (\(i,j) -> if murmur3 0 (into (show (i + 3947*j))) `mod` 20 == 0 then '.' else ' '))
      (V2 0 0)
      M.empty

makeLenses ''Robot
makeLenses ''GameState

step :: State GameState ()
step = do
  rs <- use robots
  rs' <- catMaybes <$> forM rs stepRobot
  robots .= rs'
  new <- use newRobots
  robots %= (new++)
  newRobots .= []

gameStep :: GameState -> GameState
gameStep = execState step

stepRobot :: Robot -> State GameState (Maybe Robot)
stepRobot r = stepProgram (r ^. robotProgram) r

stepProgram :: Program -> Robot -> State GameState (Maybe Robot)
stepProgram []                 = const (return Nothing)
stepProgram (Block p1 : p2)    = stepProgram (p1 ++ p2)
stepProgram (Repeat 0 _ : p)   = stepProgram p
stepProgram (Repeat n p1 : p2) = stepProgram (p1 : Repeat (n-1) p1 : p2)
stepProgram (cmd : p)          = fmap Just . exec cmd . (robotProgram .~ p)

exec :: Command -> Robot -> State GameState Robot
exec Wait     r = return r
exec Move     r = return (r & location %~ (^+^ (r ^. direction)))
exec (Turn d) r = return (r & direction %~ applyTurn d)
exec Harvest  r = do
  let V2 row col = r ^. location
  (h, w') <- uses world (W.lookup (row,col))
  world .= w'
  world %= W.insert (row,col) ' '
  inventory . at (Resource h) . non 0 += 1
  return r
exec (Build p) r = do
  newRobots %= (Robot (r ^. location) (V2 0 1) [p] False :)
  return r

applyTurn :: Direction -> V2 Int -> V2 Int
applyTurn Lt (V2 x y) = V2 (-y) x
applyTurn Rt (V2 x y) = V2 y (-x)
applyTurn North _     = V2 (-1) 0
applyTurn South _     = V2 1 0
applyTurn East _      = V2 0 1
applyTurn West _      = V2 0 (-1)
