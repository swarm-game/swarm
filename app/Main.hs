{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- XXX way to configure to use fancy Unicode characters or stick to ASCII

module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens
import           Control.Lens.Unsound       (lensProduct)
import           Control.Monad.State
import           Data.Either                (isRight)
import           Data.List.Split            (chunksOf)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Linear
import           System.Random              (randomRIO)

import           Brick                      hiding (Direction)
import           Brick.BChan
import           Brick.Focus
import           Brick.Forms
import           Brick.Widgets.Border       (border, borderAttr,
                                             borderWithLabel, hBorder, vBorder)
import qualified Brick.Widgets.Border.Style as BS
import           Brick.Widgets.Center       (center, hCenter, vCenter)
import qualified Graphics.Vty               as V

import           Brick.Widgets.Dialog
import           Data.Text                  (Text)

import           Swarm.AST
import           Swarm.Game
import           Swarm.Parse

------------------------------------------------------------
-- State machine
------------------------------------------------------------

mkBase :: Command -> Robot
mkBase cmd = Robot (V2 0 0) (V2 0 0) [cmd] True

step :: State GameState ()
step = do
  rs <- use robots
  rs' <- catMaybes <$> forM rs stepRobot
  robots .= rs'
  new <- use newRobots
  robots %= (new++)
  newRobots .= []

doStep :: GameState -> GameState
doStep = execState step

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
  mh <- preuse $ world . ix row . ix col
  case mh of
    Nothing -> return ()
    Just h  -> do
      world . ix row . ix col .= ' '
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

robotAttr, plantAttr, flowerAttr, dirtAttr, rockAttr, baseAttr, highlightAttr, defAttr :: AttrName
robotAttr     = "robotAttr"
plantAttr     = "plantAttr"
flowerAttr    = "flowerAttr"
dirtAttr      = "dirtAttr"
rockAttr      = "rockAttr"
baseAttr      = "baseAttr"
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
  , (invalidFormInputAttr, fg V.red)
  , (focusedFormInputAttr, V.defAttr)
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

errorDialog :: Dialog ()
errorDialog = dialog (Just "Error") Nothing 80

handleEvent :: GameState -> BrickEvent Name Tick -> EventM Name (Next GameState)
handleEvent g (AppEvent Tick)                        = continue $ doStep g
handleEvent g (VtyEvent (V.EvKey (V.KChar '\t') [])) = continue $ g & uiState . uiFocusRing %~ focusNext
handleEvent g (VtyEvent (V.EvKey V.KBackTab []))     = continue $ g & uiState . uiFocusRing %~ focusPrev
handleEvent g (VtyEvent (V.EvKey V.KEsc []))
  | isJust (g ^. uiState . uiError) = continue $ g & uiState . uiError .~ Nothing
  | otherwise                       = halt g
handleEvent g ev =
  case focusGetCurrent (g ^. uiState . uiFocusRing) of
    Just REPLPanel -> handleREPLEvent g ev
    _              -> continueWithoutRedraw g

handleREPLEvent :: GameState -> BrickEvent Name Tick -> EventM Name (Next GameState)
handleREPLEvent g (VtyEvent (V.EvKey V.KEnter []))
  = case result of
      Right cmd ->
        continue $ g
          & uiState . uiReplForm    %~ updateFormState ""
          & uiState . uiReplHistory %~ (entry :)
          & uiState . uiReplHistIdx .~ (-1)
          & robots                  %~ (mkBase cmd :)
      Left err ->
        continue $ g
          & uiState . uiError ?~ str (errorBundlePretty err)
  where
    entry = formState (g ^. uiState . uiReplForm)
    result = parse parseCommand "" entry
handleREPLEvent g (VtyEvent (V.EvKey V.KUp []))   = continue $ adjReplHistIndex g (+)
handleREPLEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ adjReplHistIndex g (-)
handleREPLEvent g ev = do
  f' <- handleFormEvent ev (g ^. uiState . uiReplForm)
  let result = parse parseCommand "" (formState f')
      f''    = setFieldValid (isRight result) REPLInput f'
  continue $ g & uiState . uiReplForm .~ f''

adjReplHistIndex :: GameState -> (Int -> Int -> Int) -> GameState
adjReplHistIndex g (+/-) =
  g & uiState . uiReplHistIdx .~ newIndex
    & if newIndex /= curIndex then uiState . uiReplForm %~ updateFormState newEntry else id
  where
    curIndex = g ^. uiState . uiReplHistIdx
    histLen  = length (g ^. uiState . uiReplHistory)
    newIndex = min (histLen - 1) (max (-1) (curIndex +/- 1))
    newEntry
      | newIndex == -1 = ""
      | otherwise      = (g ^. uiState . uiReplHistory) !! newIndex

replHeight :: Int
replHeight = 10

drawUI :: GameState -> [Widget Name]
drawUI g =
  [ drawDialog g
  , vBox
    [ hBox
      [ panel fr WorldPanel $ hLimitPercent 75 $ drawWorld g
      , panel fr InfoPanel $ drawInventory $ g ^. inventory
      ]
    , panel fr REPLPanel $ vLimit replHeight $ padBottom Max $ padLeftRight 1 $ drawRepl g
    ]
  ]
  where
    fr = g ^. uiState . uiFocusRing

drawDialog :: GameState -> Widget Name
drawDialog g = case g ^. uiState . uiError of
  Nothing -> emptyWidget
  Just d  -> renderDialog errorDialog d

drawWorld :: GameState -> Widget Name
drawWorld g
  = center
  $ padAll 1
  $ vBox (imap (\r -> hBox . imap (drawLoc r)) (g ^. world))
  where
    robotLocs = M.fromList $ g ^.. robots . traverse . lensProduct location direction
    drawLoc 0 0 _ = withAttr robotAttr $ txt "■"
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

replPrompt :: Text
replPrompt = "> "

drawRepl :: GameState -> Widget Name
drawRepl g = vBox $
  map ((txt replPrompt <+>) . txt) (reverse (take (replHeight - 1) (g ^. uiState . uiReplHistory)))
  ++
  [ renderForm (g ^. uiState . uiReplForm) ]

app :: App GameState Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

------------------------------------------------------------

initFocusRing :: FocusRing Name
initFocusRing = focusRing [REPLPanel, InfoPanel, WorldPanel]

initReplForm :: Form Text Tick Name
initReplForm = newForm
  [(txt replPrompt <+>) @@= editTextField id REPLInput (Just 1)]
  ""

initUIState :: UIState
initUIState = UIState initFocusRing initReplForm [] (-1) Nothing

testGameState :: GameState
testGameState
  = GameState [Robot (V2 0 0) (V2 0 1) testProgram False] [] ["TT*O", "T*.O"] M.empty initUIState

testProgram :: Program
testProgram = [Wait, Harvest, Move, Harvest, Turn Rt, Move, Harvest, Turn Lt, Move, Harvest, Harvest, Move, Harvest]

longTestProgram :: Program
longTestProgram = take 100 $ cycle [Harvest, Move, Turn Rt, Harvest, Move, Turn Lt]

initRs = 50
initCs = 50

initGameState :: IO GameState
initGameState = do
  rs <- replicateM (initRs * initCs) (randomRIO (0, length resourceList - 1))
  return $
    GameState [] []
      (chunksOf initCs (map (resourceList!!) rs))
      M.empty
      initUIState

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 50000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  g <- initGameState
  void $ customMain initialVty buildVty (Just chan) app g
