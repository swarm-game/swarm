{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens
import           Control.Lens.Unsound       (lensProduct)
import           Control.Monad.State
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Void
import           Linear

import           Brick
import           Brick.BChan
import           Brick.Widgets.Border       (border, borderWithLabel, hBorder,
                                             vBorder)
import qualified Brick.Widgets.Border.Style as BS
import           Brick.Widgets.Center       (center, hCenter, vCenter)
import qualified Graphics.Vty               as V

import           Data.Text                  (Text)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

------------------------------------------------------------
-- AST

data Command
  = Wait
  | Move
  | TL
  | TR
  | Harvest
  deriving (Eq, Ord, Show)

type Program = [Command]

------------------------------------------------------------
-- Parsing
------------------------------------------------------------

type Parser = Parsec Void Text

--------------------------------------------------
-- Lexer

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserved :: Text -> Parser ()
reserved w = (lexeme . try) $ string' w *> notFollowedBy alphaNumChar

--------------------------------------------------
-- Parser

parseCommand :: Parser Command
parseCommand =
      Move    <$ reserved "move"
  <|> TL      <$ reserved "left"
  <|> TR      <$ reserved "right"
  <|> Harvest <$ reserved "harvest"

parseProgram :: Parser Program
parseProgram = many parseCommand

------------------------------------------------------------
-- State machine
------------------------------------------------------------

data Robot = Robot
  { _location     :: V2 Int
  , _direction    :: V2 Int
  , _robotProgram :: Program
  }
  deriving (Eq, Ord, Show)

data Item = Resource Char
  deriving (Eq, Ord, Show)

data GameState = GameState
  { _baseProgram :: Program
  , _robots      :: [Robot]
  , _world       :: [[Char]]
  , _inventory   :: Map Item Int
  }
  deriving (Eq, Ord, Show)

makeLenses ''Robot
makeLenses ''GameState

step :: State GameState ()
step = do
  rs <- use robots
  rs' <- catMaybes <$> forM rs stepRobot
  robots .= rs'

doStep :: GameState -> GameState
doStep = execState step

stepRobot :: Robot -> State GameState (Maybe Robot)
stepRobot r = case r ^. robotProgram of
  []        -> return Nothing
  (cmd : p) -> Just <$> exec cmd (r & robotProgram .~ p)

exec :: Command -> Robot -> State GameState Robot
exec Wait    r = return r
exec Move    r = return $ (r & location %~ (^+^ (r ^. direction)))
exec TL      r = return $ (r & direction %~ vLeft)
exec TR      r = return $ (r & direction %~ vRight)
exec Harvest r = do
  let V2 row col = r ^. location
  mh <- preuse $ world . ix row . ix col
  case mh of
    Nothing -> return ()
    Just h  -> do
      world . ix row . ix col .= ' '
      inventory . at (Resource h) . non 0 += 1
  return r

vLeft (V2 x y) = V2 (-y) (x)
vRight (V2 x y) = V2 y (-x)

------------------------------------------------------------
-- Resources

data ResourceInfo = RI
  { _resourceChar :: Char
  , _resourceName :: Text
  , _resourceAttr :: AttrName
  }

makeLenses ''ResourceInfo

resourceMap :: Map Char ResourceInfo
resourceMap = M.fromList $
  [ ('T', RI 'T' "Tree"   treeAttr)
  , ('*', RI '*' "Flower" flowerAttr)
  , ('.', RI '.' "Dirt"   dirtAttr)
  , ('O', RI 'O' "Rock"   rockAttr)
  , (' ', RI ' ' "Air"    defAttr)
  ]

------------------------------------------------------------
-- UI

data Tick = Tick

type Name = ()

app :: App GameState Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

robotAttr, treeAttr, flowerAttr, dirtAttr, rockAttr, defAttr :: AttrName
robotAttr  = "robotAttr"
treeAttr   = "treeAttr"
flowerAttr = "flowerAttr"
dirtAttr   = "dirtAttr"
rockAttr   = "rockAttr"
defAttr    = "defAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (robotAttr, fg V.cyan `V.withStyle` V.bold)
  , (treeAttr, fg V.green)
  , (flowerAttr, fg V.yellow)
  , (dirtAttr, fg (V.rgbColor 165 42 42))
  , (rockAttr, fg (V.rgbColor 80 80 80))
  , (defAttr, V.defAttr)
  ]

handleEvent :: GameState -> BrickEvent Name Tick -> EventM Name (Next GameState)
handleEvent g (AppEvent Tick)                       = continue $ doStep g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

drawUI :: GameState -> [Widget Name]
drawUI g =
  [ joinBorders
  $ border
  $ vBox
    [ hBox
      [ hLimitPercent 75 $ drawWorld g
      , vBorder
      , drawInventory $ g ^. inventory
      ]
    , hBorder
    , vLimit 10 $ center $ str "REPL"
    ]
  ]

drawWorld :: GameState -> Widget Name
drawWorld g
  = center
  $ padAll 1
  $ vBox (imap (\r -> hBox . imap (\c x -> drawLoc r c x)) (g ^. world))
  where
    robotLocs = M.fromList $ g ^.. robots . traverse . lensProduct location direction
    drawLoc r c x = case M.lookup (V2 r c) robotLocs of
      Just dir -> withAttr robotAttr $ str (robotDir dir)
      Nothing  -> drawResource x

robotDir (V2 0 1)    = "▶"
robotDir (V2 0 (-1)) = "◀"
robotDir (V2 1 0)    = "▼"
robotDir (V2 (-1) 0) = "▲"

drawInventory :: Map Item Int -> Widget Name
drawInventory inv
  = vBox
  [ hCenter (str "Inventory")
  , padAll 2
    $ padBottom Max
    $ vBox
    $ map drawItem (M.assocs inv)
  ]

drawItem :: (Item, Int) -> Widget Name
drawItem (Resource c, n) = drawNamedResource c <+> showCount n
  where
    showCount = padLeft Max . str . show

drawNamedResource :: Char -> Widget Name
drawNamedResource c = case M.lookup c resourceMap of
  Nothing -> str [c]
  Just (RI _ nm attr) ->
    hBox [ withAttr attr (padRight (Pad 2) (str [c])), txt nm ]

drawResource :: Char -> Widget Name
drawResource c = case M.lookup c resourceMap of
  Nothing            -> str [c]
  Just (RI _ _ attr) -> withAttr attr (str [c])

------------------------------------------------------------

testGameState :: GameState
testGameState = GameState [] [Robot (V2 0 0) (V2 0 1) testProgram] ["TT*O", "T*.O"] M.empty

testProgram :: Program
testProgram = [Wait, Harvest, Move, Harvest, TR, Move, Harvest, TL, Move, Harvest, Harvest, Move, Harvest]

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 500000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app testGameState
