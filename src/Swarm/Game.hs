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
  , _world      :: W.TileCachingWorld
  , _viewCenter :: V2 Int
  , _updated    :: Bool
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
      False
      M.empty

makeLenses ''Robot
makeLenses ''GameState

gameStep :: GameState -> GameState
gameStep = execState step

step :: State GameState ()
step = do
  updated .= False
  rs <- use robots
  rs' <- catMaybes <$> forM rs stepRobot
  robots .= rs'
  new <- use newRobots
  robots %= (new++)
  newRobots .= []

stepRobot :: Robot -> State GameState (Maybe Robot)
stepRobot r = stepProgram (r ^. robotProgram) r

stepProgram :: Program -> Robot -> State GameState (Maybe Robot)
stepProgram []                 = const (updated .= True >> return Nothing)
stepProgram (Block p1 : p2)    = stepProgram (p1 ++ p2)
stepProgram (Repeat 0 _ : p)   = stepProgram p
stepProgram (Repeat n p1 : p2) = stepProgram (p1 : Repeat (n-1) p1 : p2)
stepProgram (cmd : p)          = fmap Just . exec cmd . (robotProgram .~ p)

exec :: Command -> Robot -> State GameState Robot
exec Wait     r = return r
exec Move     r = do
  updated .= True
  return (r & location %~ (^+^ (r ^. direction)))
exec (Turn d) r = do
  updated .= True
  return (r & direction %~ applyTurn d)
exec Harvest  r = do
  let V2 row col = r ^. location
  h <- uses world (W.lookup (row,col))
  world %= W.insert (row,col) ' '
  inventory . at (Resource h) . non 0 += 1
  return r
exec (Build p) r = do
  newRobots %= (Robot (r ^. location) (V2 0 1) [p] False :)
  updated .= True
  return r

applyTurn :: Direction -> V2 Int -> V2 Int
applyTurn Lt (V2 x y) = V2 (-y) x
applyTurn Rt (V2 x y) = V2 y (-x)
applyTurn North _     = north
applyTurn South _     = south
applyTurn East _      = east
applyTurn West _      = west

north, south, east, west :: V2 Int
north = V2 (-1) 0
south = V2 1 0
east = V2 0 1
west = V2 0 (-1)
