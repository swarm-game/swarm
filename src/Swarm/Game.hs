{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.Game
  ( module Swarm.Game
  , module Swarm.Game.Resource
  )
  where

import           Numeric.Noise.Perlin
import           Numeric.Noise.Ridged

import           Control.Lens         hiding (Const)
import           Control.Monad.State
-- import           Data.Hash.Murmur
import           Data.Map             (Map, (!))
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes)
import           Data.Text            (Text)
import qualified Data.Text.IO         as T
import           Linear
import           Witch

import           Swarm.AST
import           Swarm.Game.Resource
import qualified Swarm.Game.World     as W
import           Swarm.Util           (processCmd)

------------------------------------------------------------
-- CEK machine types

data Value where
  VUnit   :: Value
  VInt    :: Integer -> Value
  VString :: Text -> Value
  VDir    :: Direction -> Value
  VClo    :: Text -> Term -> Env -> Value
  VCApp   :: Const -> [Value] -> Value
  VBind   :: Value -> Maybe Text -> Term -> Env -> Value
  VNop    :: Value
  deriving (Eq, Ord, Show)

type Env = Map Text Value

emptyEnv :: Env
emptyEnv = M.empty

data Frame
  = FArg Term Env
  | FApp Value
  | FLet Text Term Env
  | FMkBind (Maybe Text) Term Env
  | FExec
  | FExecBind (Maybe Text) Term Env
  | FRepeat Integer Value
  deriving (Eq, Ord, Show)

type Cont = [Frame]

data CEK = In Term Env Cont | Out Value Cont
  deriving (Eq, Ord, Show)

initMachine :: Term -> CEK
initMachine e = In e M.empty [FExec]

initMachineV :: Value -> CEK
initMachineV v = Out v [FExec]

------------------------------------------------------------
-- Game state data types

data Robot = Robot
  { _location  :: V2 Int
  , _direction :: V2 Int
  , _machine   :: CEK
  , _tickSteps :: Int
  , _static    :: Bool
  }
  deriving (Eq, Ord, Show)

mkRobot :: V2 Int -> V2 Int -> CEK -> Robot
mkRobot l d m = Robot
  { _location  = l
  , _direction = d
  , _machine   = m
  , _tickSteps = 0
  , _static    = False
  }

mkBase :: Term -> Robot
mkBase e = Robot
  { _location  = V2 0 0
  , _direction = V2 0 1
  , _machine   = initMachine e
  , _tickSteps = 0
  , _static    = True
  }

data Item = Resource Char
  deriving (Eq, Ord, Show)

data GameState = GameState
  { _robots     :: [Robot]
  , _newRobots  :: [Robot]
  , _world      :: W.SimpleWorld
  , _viewCenter :: V2 Int
  , _updated    :: Bool
  , _inventory  :: Map Item Int
  }

pn1, pn2 :: Perlin
pn1 = perlin 0 5 0.05 0.5
pn2 = perlin 0 5 0.05 0.75

rn :: Ridged
rn = ridged 0 5 0.005 1 2

initGameState :: IO GameState
initGameState = return $
  GameState
  { _robots     = []
  , _newRobots  = []
  , _world      = W.newWorld $ \(i,j) ->
      if noiseValue pn1 (fromIntegral i, fromIntegral j, 0) > 0
        then 'T'
        else
          if noiseValue pn2 (fromIntegral i, fromIntegral j, 0) > 0
            then 'O'
            else '.'
--      if murmur3 0 (into (show (i + 3947*j))) `mod` 20 == 0 then '.' else ' '
  , _viewCenter = V2 0 0
  , _updated    = False
  , _inventory  = M.empty
  }

makeLenses ''Robot
makeLenses ''GameState

------------------------------------------------------------
-- CEK machine

gameStep :: GameState -> IO GameState
gameStep = execStateT step

evalStepsPerTick :: Int
evalStepsPerTick = 30

step :: StateT GameState IO ()
step = do
  updated .= False
  rs <- use robots
  rs' <- catMaybes <$> forM rs (bigStepRobot . (tickSteps .~ evalStepsPerTick))
  robots .= rs'
  new <- use newRobots
  robots %= (new++)
  newRobots .= []

bigStepRobot :: Robot -> StateT GameState IO (Maybe Robot)
bigStepRobot r
  | r ^. tickSteps <= 0 = return (Just r)
  | otherwise           = do
      r' <- stepRobot r
      maybe (return Nothing) (bigStepRobot . (tickSteps -~ 1)) r'

mkStep :: Robot -> CEK -> StateT GameState IO (Maybe Robot)
mkStep r cek = return . Just $ r & machine .~ cek

stepRobot :: Robot -> StateT GameState IO (Maybe Robot)
stepRobot r = case r ^. machine of
  In TUnit _ k                      -> mkStep r $ Out VUnit k
  In (TConst c) _ k                 -> mkStep r $ Out (VCApp c []) k
  In (TDir d) _ k                   -> mkStep r $ Out (VDir d) k
  In (TInt n) _ k                   -> mkStep r $ Out (VInt n) k
  In (TString s) _ k                -> mkStep r $ Out (VString s) k
  In (TVar x) e k                   -> mkStep r $ Out (e!x) k
  In (TLam x _ t) e k               -> mkStep r $ Out (VClo x t e) k
  In (TApp t1 t2) e k               -> mkStep r $ In t1 e (FArg t2 e : k)
  In (TLet x _ t1 t2) e k           -> mkStep r $ In t1 e (FLet x t2 e : k)
  In (TBind mx t1 t2) e k           -> mkStep r $ In t1 e (FMkBind mx t2 e : k)
  In TNop _ k                       -> mkStep r $ Out VNop k

  Out _ []                          -> updated .= True >> return Nothing

  Out v1 (FArg t2 e : k)            -> mkStep r $ In t2 e (FApp v1 : k)
  Out v2 (FApp (VCApp c args) : k)  -> mkStep r $ Out (VCApp c (v2 : args)) k
  Out v2 (FApp (VClo x t e) : k)    -> mkStep r $ In t (M.insert x v2 e) k
  Out v1 (FLet x t2 e : k)          -> mkStep r $ In t2 (M.insert x v1 e) k
  Out v1 (FMkBind mx t2 e : k)      -> mkStep r $ Out (VBind v1 mx t2 e) k
  Out VNop (FExec : k)              -> mkStep r $ Out VUnit k
  Out (VCApp c args) (FExec : k)    -> execConst c args k (r & tickSteps .~ 0)
  Out (VBind c mx t2 e) (FExec : k) -> mkStep r $ Out c (FExec : FExecBind mx t2 e : k)
  Out v (FExecBind mx t2 e : k)     -> mkStep r $ In t2 (maybe id (`M.insert` v) mx e) (FExec : k)

  Out _ (FRepeat n c : k)          -> execConst Repeat [c, VInt n] k r

  cek -> error $ "Panic! Bad machine state in stepRobot: " ++ show cek

appArity :: Const -> [Value] -> Int
appArity c args = constArity c - Prelude.length args

nonStatic :: Cont -> Robot -> StateT GameState IO (Maybe Robot) -> StateT GameState IO (Maybe Robot)
nonStatic k r m
  | r ^. static = mkStep r (Out VUnit k)  -- XXX message saying that the base can't move?
  | otherwise   = m

execConst :: Const -> [Value] -> Cont -> Robot -> StateT GameState IO (Maybe Robot)
execConst Wait _ k r = mkStep r $ Out VUnit k
execConst Move _ k r = nonStatic k r $ do
  updated .= True
  mkStep (r & location %~ (^+^ (r ^. direction))) (Out VUnit k)
execConst Harvest _ k r = nonStatic k r $ do
  let V2 row col = r ^. location
  h <- uses world (W.lookup (row,col))
  world %= W.insert (row,col) ' '
  inventory . at (Resource h) . non 0 += 1
  mkStep r (Out VUnit k)
execConst Turn [VDir d] k r = nonStatic k r $ do
  updated .= True
  mkStep (r & direction %~ applyTurn d) (Out VUnit k)
execConst Turn args k _ = badConst Turn args k
execConst Repeat [_, VInt 0] k r = mkStep r $ Out VUnit k
execConst Repeat [c, VInt n] k r = mkStep r $ Out c (FExec : FRepeat (n-1) c : k)
execConst Repeat args k _ = badConst Repeat args k
execConst Build [c] k r = do
  newRobots %= (mkRobot (r ^. location) (r ^. direction) (initMachineV c) :)
  updated .= True
  mkStep r (Out VUnit k)
execConst Build args k _ = badConst Build args k
execConst Run [VString fileName] k r = do
  f <- liftIO $ T.readFile (into fileName)  -- XXX handle file not existing
  case processCmd f of
    Left  err -> error (into err)  -- XXX
    Right t   -> mkStep r $ In t M.empty (FExec : k)
    -- Note, adding FExec to the stack above is correct.  run has the
    --   type run : String -> Cmd (), i.e. executing (run s) for some
    --   string s causes it to load *and immediately execute* the
    --   program in the file.
    --
    -- If we instead had
    --
    --   load : String -> Cmd (Cmd ())
    --
    -- (which could indeed be useful, once commands have return values
    -- and Bind does more than just sequencing) then the code would be
    -- the same as for run, EXCEPT that we would NOT add the FExec to
    -- the stack above.  The fact that there are two FExec frames
    -- involved in executing 'run' (one to execute the run command
    -- itself, and one to execute the thing it loads) corresponds to
    -- the fact that it is equivalent to (in pseudo-Haskell syntax)
    -- 'join . load'.

execConst Run args k _ = badConst Run args k

badConst :: Const -> [Value] -> Cont -> a
badConst c args k = error $
  "Panic! Bad application of execConst " ++ show c ++ " " ++ show args ++ " " ++ show k

applyTurn :: Direction -> V2 Int -> V2 Int
applyTurn Lt (V2 x y)   = V2 (-y) x
applyTurn Rt (V2 x y)   = V2 y (-x)
applyTurn Back (V2 x y) = V2 (-x) (-y)
applyTurn Fwd v         = v
applyTurn North _       = north
applyTurn South _       = south
applyTurn East _        = east
applyTurn West _        = west

north, south, east, west :: V2 Int
north = V2 (-1) 0
south = V2 1 0
east  = V2 0 1
west  = V2 0 (-1)
