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

------------------------------------------------------------
-- CEK machine

data Value where
  VUnit  :: Value
  VInt   :: Integer -> Value
  VDir   :: Direction -> Value
  VPAp   :: Const -> [Value] -> Value

type Env = Map Text Value

emptyEnv :: Env
emptyEnv = M.empty

data Frame
  = FArg Term Env
  | FApp Value
  | FBind Term Env

type Cont = [Frame]

data CEK = In Expr Env Cont | Out Value Cont

isFinal :: CEK -> Maybe Value
isFinal (Out v _ []) = Just v
isFinal _            = Nothing

initMachine :: Expr -> CEK
initMachine e = In e M.empty []

------------------------------------------------------------

data Robot = Robot
  { _location  :: V2 Int
  , _direction :: V2 Int
  , _machine   :: CEK
  , _static    :: Bool
  }
  deriving (Eq, Ord, Show)

mkBase :: Expr -> Robot
mkBase e = Robot
  { _location  = V2 0 0
  , _direction = V2 0 0
  , _machine   = initMachine e
  , _static    = True
  }

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

initGameState :: IO GameState
initGameState = return $
  GameState
  { _robots = []
  , _newRobots = []
  , _world = W.newWorld (\(i,j) -> if murmur3 0 (into (show (i + 3947*j))) `mod` 20 == 0 then '.' else ' ')
  , _viewCenter = V2 0 0
  , _updated = False
  , _inventory = M.empty
  }

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

mkStep :: Robot -> CEK -> State GameState (Maybe Robot)
mkStep r cek = return . Just $ r & machine .~ cek

stepRobot :: Robot -> State GameState (Maybe Robot)
stepRobot r = case r ^. machine of
  In (EConst c) e k      -> stepConst r c e k
  In (EDir d) e k        -> mkStep r $ Out (VDir d) k
  In (EInt n) e k        -> mkStep r $ Out (VInt n) k
  In (EApp t1 t2) e k    -> mkStep r (In t1 e (FArg t2 e : k))
  In (EBind t1 t2) e k   -> mkStep r (In t1 e (FBind t2 e : k))

  Out v1 (FArg t2 e : k) -> mkStep r (In t2 e (FApp v1 : k))
  Out v2 (FApp f@(VPAp c args) : k)
    | arity f == 1       -> stepApp c (v2 : args) k r
    | otherwise          -> mkStep r (Out (VPAp c (v2 : args)) k)

stepConst :: Const -> Env -> Cont -> Robot -> State GameState (Maybe Robot)
stepConst Wait e k r = mkStep r (Out VUnit k)
stepConst Move e k r = do
  updated .= True
  mkStep (r & location %~ (^+^ (r ^. direction))) (Out VUnit k)
stepConst Harvest e k r = do
  let V2 row col = r ^. location
  h <- uses world (W.lookup (row,col))
  world %= W.insert (row,col) ' '
  inventory . at (Resource h) . non 0 += 1
  mkStep r (Out VUnit k)

-- The other constants are waiting for arguments.
stepConst c e k r = mkStep r (Out (VPAp c []) k)

stepApp :: Const -> [Value] -> Cont -> Robot -> State GameState (Maybe Robot)
stepApp Turn [d] k r = do
  updated .= True
  mkStep (r & direction %~ applyTurn d) (Out VUnit k)
stepApp Repeat [c,n]

stepProgram :: Program -> Robot -> State GameState (Maybe Robot)
stepProgram []                 = const (updated .= True >> return Nothing)
stepProgram (Block p1 : p2)    = stepProgram (p1 ++ p2)
stepProgram (Repeat 0 _ : p)   = stepProgram p
stepProgram (Repeat n p1 : p2) = stepProgram (p1 : Repeat (n-1) p1 : p2)
stepProgram (cmd : p)          = fmap Just . exec cmd . (robotProgram .~ p)

exec :: Command -> Robot -> State GameState Robot
exec (Build p) r = do
  newRobots %= (Robot (r ^. location) (V2 0 1) [p] False :)
  updated .= True
  return r

applyTurn :: Direction -> V2 Int -> V2 Int
applyTurn Lt (V2 x y)     = V2 (-y) x
applyTurn Rt (V2 x y)     = V2 y (-x)
applyTurn Around (V2 x y) = V2 (-x) (-y)
applyTurn North _         = north
applyTurn South _         = south
applyTurn East _          = east
applyTurn West _          = west

north, south, east, west :: V2 Int
north = V2 (-1) 0
south = V2 1 0
east  = V2 0 1
west  = V2 0 (-1)
