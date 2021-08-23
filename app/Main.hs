{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

-- XXX way to configure to use fancy Unicode characters or stick to ASCII

module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens
import           Control.Lens.Unsound       (lensProduct)
import           Control.Monad              (forever, replicateM, void)
import           Data.Either                (isRight)
import           Data.List.Split            (chunksOf)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Text                  (Text)
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
import           Brick.Widgets.Dialog
import qualified Graphics.Vty               as V

import           Swarm.AST
import           Swarm.Game
import           Swarm.Parse
import           Swarm.UI
import           Swarm.UI.Attr
import           Swarm.UI.Panel

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

data AppState = AppState
  { _gameState :: GameState
  , _uiState   :: UIState
  }

makeLenses ''AppState

errorDialog :: Dialog ()
errorDialog = dialog (Just "Error") Nothing 80

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent s (AppEvent Tick)                        = continue $ s & gameState %~ doStep
handleEvent s (VtyEvent (V.EvKey (V.KChar '\t') [])) = continue $ s & uiState . uiFocusRing %~ focusNext
handleEvent s (VtyEvent (V.EvKey V.KBackTab []))     = continue $ s & uiState . uiFocusRing %~ focusPrev
handleEvent s (VtyEvent (V.EvKey V.KEsc []))
  | isJust (s ^. uiState . uiError) = continue $ s & uiState . uiError .~ Nothing
  | otherwise                       = halt s
handleEvent s ev =
  case focusGetCurrent (s ^. uiState . uiFocusRing) of
    Just REPLPanel -> handleREPLEvent s ev
    _              -> continueWithoutRedraw s

handleREPLEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleREPLEvent s (VtyEvent (V.EvKey V.KEnter []))
  = case result of
      Right cmd ->
        continue $ s
          & uiState . uiReplForm    %~ updateFormState ""
          & uiState . uiReplHistory %~ (entry :)
          & uiState . uiReplHistIdx .~ (-1)
          & gameState . robots      %~ (mkBase cmd :)
      Left err ->
        continue $ s
          & uiState . uiError ?~ txt err
  where
    entry = formState (s ^. uiState . uiReplForm)
    result = readCommand entry
handleREPLEvent s (VtyEvent (V.EvKey V.KUp []))
  = continue $ s & uiState %~ adjReplHistIndex (+)
handleREPLEvent s (VtyEvent (V.EvKey V.KDown []))
  = continue $ s & uiState %~ adjReplHistIndex (-)
handleREPLEvent s ev = do
  f' <- handleFormEvent ev (s ^. uiState . uiReplForm)
  let result = readCommand (formState f')
      f''    = setFieldValid (isRight result) REPLInput f'
  continue $ s & uiState . uiReplForm .~ f''

adjReplHistIndex :: (Int -> Int -> Int) -> UIState -> UIState
adjReplHistIndex (+/-) s =
  s & uiReplHistIdx .~ newIndex
    & if newIndex /= curIndex then uiReplForm %~ updateFormState newEntry else id
  where
    curIndex = s ^. uiReplHistIdx
    histLen  = length (s ^. uiReplHistory)
    newIndex = min (histLen - 1) (max (-1) (curIndex +/- 1))
    newEntry
      | newIndex == -1 = ""
      | otherwise      = (s ^. uiReplHistory) !! newIndex

replHeight :: Int
replHeight = 10

drawUI :: AppState -> [Widget Name]
drawUI s =
  [ drawDialog (s ^. uiState)
  , vBox
    [ hBox
      [ panel plantAttr fr WorldPanel $ hLimitPercent 75 $ drawWorld (s ^. gameState)
      , panel plantAttr fr InfoPanel $ drawInventory $ (s ^. gameState . inventory)
      ]
    , panel plantAttr fr REPLPanel $ vLimit replHeight $ padBottom Max $ padLeftRight 1 $ drawRepl s
    ]
  ]
  where
    fr = s ^. uiState . uiFocusRing

drawDialog :: UIState -> Widget Name
drawDialog s = case s ^. uiError of
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

drawRepl :: AppState -> Widget Name
drawRepl s = vBox $
  map ((txt replPrompt <+>) . txt) (reverse (take (replHeight - 1) (s ^. uiState . uiReplHistory)))
  ++
  [ renderForm (s ^. uiState . uiReplForm) ]

app :: App AppState Tick Name
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
  = GameState [Robot (V2 0 0) (V2 0 1) testProgram False] [] ["TT*O", "T*.O"] M.empty

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

initAppState :: IO AppState
initAppState = AppState <$> initGameState <*> pure initUIState

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 50000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  s <- initAppState
  void $ customMain initialVty buildVty (Just chan) app s
