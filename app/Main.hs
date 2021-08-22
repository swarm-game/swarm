{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

-- XXX way to configure to use fancy Unicode characters or stick to ASCII

module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens
import           Control.Lens.Unsound       (lensProduct)
import           Control.Monad.State
import           Data.List.Split            (chunksOf)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Void
import           Linear
import           System.Random              (randomRIO)

import           Brick
import           Brick.BChan
import           Brick.Focus
import           Brick.Widgets.Border       (border, borderAttr,
                                             borderWithLabel, hBorder, vBorder)
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

  , _uiState     :: UIState
  }

data Name
  = REPLPanel
  | WorldPanel
  | InfoPanel
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data UIState = UIState
  { _uiFocusRing :: FocusRing Name
  }

makeLenses ''Robot
makeLenses ''GameState
makeLenses ''UIState

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
exec Move    r = return (r & location %~ (^+^ (r ^. direction)))
exec TL      r = return (r & direction %~ vLeft)
exec TR      r = return (r & direction %~ vRight)
exec Harvest r = do
  let V2 row col = r ^. location
  mh <- preuse $ world . ix row . ix col
  case mh of
    Nothing -> return ()
    Just h  -> do
      world . ix row . ix col .= ' '
      inventory . at (Resource h) . non 0 += 1
  return r

vLeft (V2 x y) = V2 (-y) x
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
resourceMap = M.fromList
  [ ('T', RI 'T' "Tree"   plantAttr)
  , (',', RI ',' "Grass"  plantAttr)
  , ('*', RI '*' "Flower" flowerAttr)
  , ('.', RI '.' "Dirt"   dirtAttr)
  , ('O', RI 'O' "Rock"   rockAttr)
  , (' ', RI ' ' "Air"    defAttr)
  ]

resourceList :: [Char]
resourceList = M.keys resourceMap

------------------------------------------------------------
-- UI

data Tick = Tick

robotAttr, plantAttr, flowerAttr, dirtAttr, rockAttr, highlightAttr, defAttr :: AttrName
robotAttr     = "robotAttr"
plantAttr     = "plantAttr"
flowerAttr    = "flowerAttr"
dirtAttr      = "dirtAttr"
rockAttr      = "rockAttr"
highlightAttr = "highlightAttr"
defAttr       = "defAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (robotAttr, fg V.white `V.withStyle` V.bold)
  , (plantAttr, fg V.green)
  , (flowerAttr, fg V.yellow)
  , (dirtAttr, fg (V.rgbColor 165 42 42))
  , (rockAttr, fg (V.rgbColor 80 80 80))
  , (highlightAttr, fg V.cyan)
  , (defAttr, V.defAttr)
  ]

data Panel n = Panel { _panelName :: n, _panelContent :: Widget n }

makeLenses ''Panel

instance Named (Panel n) n where
  getName = view panelName

drawPanel :: Eq n => FocusRing n -> Panel n -> Widget n
drawPanel fr p = withFocusRing fr drawPanel' p
  where
    drawPanel' :: Bool -> Panel n -> Widget n
    drawPanel' focused p
      = (if focused then overrideAttr borderAttr plantAttr else id)
      $ border (p ^. panelContent)

panel :: Eq n => FocusRing n -> n -> Widget n -> Widget n
panel fr nm w = drawPanel fr (Panel nm w)

handleEvent :: GameState -> BrickEvent Name Tick -> EventM Name (Next GameState)
handleEvent g (AppEvent Tick)                        = continue $ doStep g
handleEvent g (VtyEvent (V.EvKey (V.KChar '\t') [])) = continue $ g & uiState . uiFocusRing %~ focusNext
handleEvent g (VtyEvent (V.EvKey V.KBackTab []))     = continue $ g & uiState . uiFocusRing %~ focusPrev
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') []))  = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))         = halt g
handleEvent g _                                      = continue g

drawUI :: GameState -> [Widget Name]
drawUI g =
  [ vBox
    [ hBox
      [ panel fr WorldPanel $ hLimitPercent 75 $ drawWorld g
      , panel fr InfoPanel $ drawInventory $ g ^. inventory
      ]
    , panel fr REPLPanel $ vLimit 10 $ center $ str "REPL"
    ]
  ]
  where
    fr = g ^. uiState . uiFocusRing

drawWorld :: GameState -> Widget Name
drawWorld g
  = center
  $ padAll 1
  $ vBox (imap (\r -> hBox . imap (drawLoc r)) (g ^. world))
  where
    robotLocs = M.fromList $ g ^.. robots . traverse . lensProduct location direction
    drawLoc r c x = case M.lookup (V2 r c) robotLocs of
      Just dir -> withAttr robotAttr $ txt (robotDir dir)
      Nothing  -> drawResource x

robotDir :: V2 Int -> Text
robotDir (V2 0 1)    = "▶"
robotDir (V2 0 (-1)) = "◀"
robotDir (V2 1 0)    = "▼"
robotDir (V2 (-1) 0) = "▲"
robotDir _           = "■"

drawInventory :: Map Item Int -> Widget Name
drawInventory inv
  = padBottom Max
  $ vBox
  [ hCenter (str "Inventory")
  , padAll 2
    $ vBox
    $ map drawItem (M.assocs inv)
  , padLeftRight 1
    $ txtWrap "Hello there, this is some long text, to see how the text wrapping feature works. Seems like it works great! Blah blah blah, I like long descriptive texts."
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

app :: App GameState Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

------------------------------------------------------------

initFocusRing :: FocusRing Name
initFocusRing = focusRing [REPLPanel, InfoPanel, WorldPanel]

initUIState :: UIState
initUIState = UIState initFocusRing

testGameState :: GameState
testGameState
  = GameState [] [Robot (V2 0 0) (V2 0 1) testProgram] ["TT*O", "T*.O"] M.empty initUIState

testProgram :: Program
testProgram = [Wait, Harvest, Move, Harvest, TR, Move, Harvest, TL, Move, Harvest, Harvest, Move, Harvest]

longTestProgram :: Program
longTestProgram = take 100 $ cycle [Harvest, Move, TR, Harvest, Move, TL]

initRs = 50
initCs = 50

initGameState :: IO GameState
initGameState = do
  rs <- replicateM (initRs * initCs) (randomRIO (0, length resourceList - 1))
  return $
    GameState
      []
      [ Robot (V2 0 0) (V2 0 1) longTestProgram
      , Robot (V2 15 3) (V2 0 1) longTestProgram
      ]
      (chunksOf initCs (map (resourceList!!) rs))
      M.empty
      initUIState

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 500000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  g <- initGameState
  void $ customMain initialVty buildVty (Just chan) app g
