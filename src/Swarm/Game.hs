{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Swarm.Game
  ( module Swarm.Game
  , module Swarm.Game.Resource
  )
  where

import           Numeric.Noise.Perlin
import           Numeric.Noise.Ridged

import           Control.Lens         hiding (Const, from)
import           Control.Monad.State
import           Data.List            (intercalate)
-- import           Data.Hash.Murmur
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes)
import           Data.Text            (Text)
import qualified Data.Text.IO         as T
import           Linear
import           Witch

import           Swarm.Pretty

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
  VBool   :: Bool -> Value
  VClo    :: Text -> UTerm -> Env -> Value
  VCApp   :: Const -> [Value] -> Value
  VBind   :: Value -> Maybe Var -> UTerm -> Env -> Value
  VNop    :: Value
  VDelay  :: UTerm -> Env -> Value
  deriving (Eq, Ord, Show)

type Env = Map Text Value

(!!!) :: Env -> Var -> Value
e !!! x = case M.lookup x e of
  Nothing -> error $ from x ++ " is not a key in the environment!"
  Just v  -> v

emptyEnv :: Env
emptyEnv = M.empty

data Frame
  = FArg UTerm Env
  | FApp Value
  | FLet Text UTerm Env
  | FMkBind (Maybe Text) UTerm Env
  | FExec
  | FExecBind (Maybe Text) UTerm Env
  deriving (Eq, Ord, Show)

type Cont = [Frame]

data CEK = In UTerm Env Cont | Out Value Cont
  deriving (Eq, Ord, Show)

initMachine :: Term' f -> CEK
initMachine e = In (erase e) M.empty [FExec]

initMachineV :: Value -> CEK
initMachineV v = Out v [FExec]

------------------------------------------------------------
-- FOR DEBUGGING ONLY
-- Should really make a nicer version of this code.

prettyCEK :: CEK -> String
prettyCEK (In c _ k) = unlines $
  [ "▶ " ++ prettyString c
  , "  " ++ prettyCont k ]
prettyCEK (Out v k) = unlines $
  [ "◀ " ++ prettyValue v
  , "  " ++ prettyCont k ]

prettyValue :: Value -> String
prettyValue = prettyString . valueToTerm

valueToTerm :: Value -> UTerm
valueToTerm VUnit            = TUnit
valueToTerm (VInt n)         = TInt n
valueToTerm (VString s)      = TString s
valueToTerm (VDir d)         = TDir d
valueToTerm (VBool b)        = TBool b
valueToTerm (VClo x t _)     = TLam x NONE t
valueToTerm (VCApp c vs)     = foldl (TApp NONE) (TConst c) (reverse (map valueToTerm vs))
valueToTerm (VBind v mx t _) = TBind mx NONE (valueToTerm v) t
valueToTerm VNop             = TNop
valueToTerm (VDelay t _)     = TDelay t

prettyCont :: Cont -> String
prettyCont = ("["++) . (++"]") . intercalate " | " . map prettyFrame

prettyFrame :: Frame -> String
prettyFrame (FArg t _)               = "_ " ++ prettyString t
prettyFrame (FApp v)                 = prettyString (valueToTerm v) ++ " _"
prettyFrame (FLet x t _)             = "let " ++ from x ++ " = _ in " ++ prettyString t
prettyFrame (FMkBind Nothing t _)    = "_ ; " ++ prettyString t
prettyFrame (FMkBind (Just x) t _)   = from x ++ " <- _ ; " ++ prettyString t
prettyFrame FExec                    = "exec _"
prettyFrame (FExecBind Nothing t _)  = "_ ; " ++ prettyString t
prettyFrame (FExecBind (Just x) t _) = from x ++ " <- _ ; " ++ prettyString t

-- END DEBUGGING CODE
------------------------------------------------------------

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

mkBase :: ATerm -> Robot
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
  , _world      :: W.TileCachingWorld
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
evalStepsPerTick = 100

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
mkStep r cek = do
  -- liftIO $ appendFile "out.txt" (prettyCEK (r ^. machine))
  return . Just $ r & machine .~ cek

stepRobot :: Robot -> StateT GameState IO (Maybe Robot)
stepRobot r = case r ^. machine of
  In TUnit _ k                      -> mkStep r $ Out VUnit k
  In (TConst c) _ k                 -> mkStep r $ Out (VCApp c []) k
  In (TDir d) _ k                   -> mkStep r $ Out (VDir d) k
  In (TInt n) _ k                   -> mkStep r $ Out (VInt n) k
  In (TString s) _ k                -> mkStep r $ Out (VString s) k
  In (TBool b) _ k                  -> mkStep r $ Out (VBool b) k
  In (TVar x) e k                   -> mkStep r $ Out (e !!! x) k
  In (TLam x _ t) e k               -> mkStep r $ Out (VClo x t e) k
  In (TApp _ t1 t2) e k             -> mkStep r $ In t1 e (FArg t2 e : k)
  In (TLet x _ t1 t2) e k           ->
    let e' = M.insert x (VDelay t1 e') e
    in mkStep r $ In t1 e' (FLet x t2 e : k)
  In (TBind mx _ t1 t2) e k         -> mkStep r $ In t1 e (FMkBind mx t2 e : k)
  In TNop _ k                       -> mkStep r $ Out VNop k
  In (TDelay t) e k                 -> mkStep r $ Out (VDelay t e) k

  Out _ []                          -> updated .= True >> return Nothing

  Out v1 (FArg t2 e : k)            -> mkStep r $ In t2 e (FApp v1 : k)
  Out v2 (FApp (VCApp c args) : k)
    | not (isCmd c) &&
      arity c == length args + 1    -> evalConst c (reverse (v2 : args)) k r
    | otherwise                     -> mkStep r $ Out (VCApp c (v2 : args)) k
  Out v2 (FApp (VClo x t e) : k)    -> mkStep r $ In t (M.insert x v2 e) k
  Out v1 (FLet x t2 e : k)          -> mkStep r $ In t2 (M.insert x v1 e) k
  Out v1 (FMkBind mx t2 e : k)      -> mkStep r $ Out (VBind v1 mx t2 e) k
  Out VNop (FExec : k)              -> mkStep r $ Out VUnit k
  Out (VCApp c args) (FExec : k)    -> execConst c (reverse args) k (r & tickSteps .~ 0)
  Out (VBind c mx t2 e) (FExec : k) -> mkStep r $ Out c (FExec : FExecBind mx t2 e : k)
  Out v (FExecBind mx t2 e : k)     -> mkStep r $ In t2 (maybe id (`M.insert` v) mx e) (FExec : k)

  cek -> error $ "Panic! Bad machine state in stepRobot: " ++ show cek

nonStatic :: Cont -> Robot -> StateT GameState IO (Maybe Robot) -> StateT GameState IO (Maybe Robot)
nonStatic k r m
  | r ^. static = mkStep r (Out VUnit k)  -- XXX message saying that the base can't move?
  | otherwise   = m

-- | At the level of the CEK machine there's no particular difference
--   between *evaluating* a function constant and *executing* a
--   command constant, but it somehow feels better to have two
--   different names for it anyway.
evalConst :: Const -> [Value] -> Cont -> Robot -> StateT GameState IO (Maybe Robot)
evalConst = execConst

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

execConst GetX _ k r = mkStep r $ Out (VInt (fromIntegral col)) k
  where
    V2 _ col = r ^. location
execConst GetY _ k r = mkStep r $ Out (VInt (fromIntegral (-row))) k
  where
    V2 row _ = r ^. location

execConst (Cmp c) [VInt n1, VInt n2] k r = mkStep r $ Out (VBool (evalCmp c n1 n2)) k
execConst (Cmp c) args k _ = badConst (Cmp c) args k

execConst (Arith c) [VInt n1, VInt n2] k r = mkStep r $ Out (VInt (evalArith c n1 n2)) k
execConst (Arith c) args k _ = badConst (Arith c) args k

execConst Force [VDelay t e] k r = mkStep r $ In t e k
execConst Force args k _ = badConst Force args k

  -- Note, if should evaluate the branches lazily, but since
  -- evaluation is eager, by the time we get here thn and els have
  -- already been fully evaluated --- what gives?  The answer is that
  -- we rely on elaboration to add 'lazy' wrappers around the branches
  -- (and a 'force' wrapper around the entire if).
execConst If [VBool True , thn, _] k r = mkStep r $ Out thn k
execConst If [VBool False, _, els] k r = mkStep r $ Out els k
execConst If args k _ = badConst If args k

execConst Build [c] k r = do
  newRobots %= (mkRobot (r ^. location) (r ^. direction) (initMachineV c) :)
  updated .= True
  mkStep r (Out VUnit k)
execConst Build args k _ = badConst Build args k
execConst Run [VString fileName] k r = do
  f <- liftIO $ T.readFile (into fileName)  -- XXX handle file not existing
  case processCmd f of
    Left  err -> error (into err)  -- XXX, display message and do nothing
    Right t   -> mkStep r $ In (erase t) M.empty (FExec : k)
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
applyTurn Lft (V2 x y)  = V2 (-y) x
applyTurn Rgt (V2 x y)  = V2 y (-x)
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

evalCmp :: CmpConst -> Integer -> Integer -> Bool
evalCmp CmpEq  = (==)
evalCmp CmpNeq = (/=)
evalCmp CmpLt  = (<)
evalCmp CmpGt  = (>)
evalCmp CmpLeq = (<=)
evalCmp CmpGeq = (>=)

evalArith :: ArithConst -> Integer -> Integer -> Integer
evalArith Add = (+)
evalArith Sub = (-)
evalArith Mul = (*)
evalArith Div = div
evalArith Exp = (^)
