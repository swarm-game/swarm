{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Swarm.UI where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Lens.Unsound        (lensProduct)
import           Control.Monad               (when)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Array                  (range)
import           Data.Either                 (isRight)
import           Data.List.Split             (chunksOf)
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe                  (isJust)
import           Data.Text                   (Text)
import           Linear

import           Brick                       hiding (Direction)
import           Brick.Focus
import           Brick.Forms
import           Brick.Widgets.Center        (center, hCenter)
import           Brick.Widgets.Dialog
import qualified Graphics.Vty                as V

import           Swarm.AST                   (east, north, south, west)
import           Swarm.Game
import qualified Swarm.Game.World            as W
import           Swarm.UI.Attr
import           Swarm.UI.Panel
import           Swarm.Util

------------------------------------------------------------
-- Custom UI label types

data Tick = Tick

data Name
  = REPLPanel
  | WorldPanel
  | InfoPanel
  | REPLInput
  | WorldCache
  | WorldExtent
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

------------------------------------------------------------
-- UI state

data UIState = UIState
  { _uiFocusRing      :: FocusRing Name
  , _uiReplForm       :: Form Text Tick Name
  , _uiReplHistory    :: [Text]
  , _uiReplHistIdx    :: Int
  , _uiError          :: Maybe (Widget Name)
  , _needsLoad        :: Bool
  , _lgTicksPerSecond :: TVar Int
  }

makeLenses ''UIState

initFocusRing :: FocusRing Name
initFocusRing = focusRing [REPLPanel, InfoPanel, WorldPanel]

replPrompt :: Text
replPrompt = "> "

initReplForm :: Form Text Tick Name
initReplForm = newForm
  [(txt replPrompt <+>) @@= editTextField id REPLInput (Just 1)]
  ""

initLgTicksPerSecond :: Int
initLgTicksPerSecond = 3    -- 2^3 = 8 ticks per second

initUIState :: IO UIState
initUIState = do
  tv <- newTVarIO initLgTicksPerSecond
  return $ UIState initFocusRing initReplForm [] (-1) Nothing True tv

------------------------------------------------------------
-- App state (= UI state + game state)

data AppState = AppState
  { _gameState :: GameState
  , _uiState   :: UIState
  }

makeLenses ''AppState

initAppState :: IO AppState
initAppState = AppState <$> initGameState <*> initUIState

------------------------------------------------------------
-- UI drawing

drawUI :: AppState -> [Widget Name]
drawUI s =
  [ drawDialog (s ^. uiState)
  , vBox
    [ hBox
      [ panel plantAttr fr WorldPanel $ hLimitPercent 75 $ drawWorld (s ^. gameState)
      , panel plantAttr fr InfoPanel $ drawInventory (s ^. gameState . inventory)
      ]
    , panel plantAttr fr REPLPanel $ vLimit replHeight $ padBottom Max $ padLeftRight 1 $ drawRepl s
    ]
  ]
  where
    fr = s ^. uiState . uiFocusRing

replHeight :: Int
replHeight = 10

errorDialog :: Dialog ()
errorDialog = dialog (Just "Error") Nothing 80

drawDialog :: UIState -> Widget Name
drawDialog s = case s ^. uiError of
  Nothing -> emptyWidget
  Just d  -> renderDialog errorDialog d

drawWorld :: GameState -> Widget Name
drawWorld g
  = center
  $ cached WorldCache
  $ reportExtent WorldExtent
  $ Widget Fixed Fixed $ do
    ctx <- getContext
    let w   = ctx ^. availWidthL
        h   = ctx ^. availHeightL
        ixs = range (viewingRegion g (w,h))
    render . vBox . map hBox . chunksOf w . map drawLoc $ ixs
  where
    robotLocs = M.fromList $ g ^.. robots . traverse . lensProduct location direction
    drawLoc (0,0) = withAttr robotAttr $ txt "■"
    drawLoc (r,c) = case M.lookup (V2 r c) robotLocs of
      Just dir -> withAttr robotAttr $ txt (robotDir dir)
      Nothing  -> drawResource (W.lookup (r,c) (g ^. world))

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

drawRepl :: AppState -> Widget Name
drawRepl s = vBox $
  map ((txt replPrompt <+>) . txt) (reverse (take (replHeight - 1) (s ^. uiState . uiReplHistory)))
  ++
  [ renderForm (s ^. uiState . uiReplForm) ]

------------------------------------------------------------
-- Event handling

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent s (AppEvent Tick)                        = do
  let g = s ^. gameState
  g' <- liftIO $ gameStep g
  when (g' ^. updated) $ invalidateCacheEntry WorldCache

  let s' = s & gameState .~ g'

  s'' <- case s' ^. uiState . needsLoad of
    False -> return s'
    True  -> do
      mext <- lookupExtent WorldExtent
      case mext of
        Nothing -> return s'
        Just _  -> (uiState . needsLoad .~ False) <$> updateView s' id

  continue s''

handleEvent s (VtyEvent (V.EvResize _ _))            = do
  invalidateCacheEntry WorldCache
  continue $ s & uiState . needsLoad .~ True
handleEvent s (VtyEvent (V.EvKey (V.KChar '\t') [])) = continue $ s & uiState . uiFocusRing %~ focusNext
handleEvent s (VtyEvent (V.EvKey V.KBackTab []))     = continue $ s & uiState . uiFocusRing %~ focusPrev
handleEvent s (VtyEvent (V.EvKey V.KEsc []))
  | isJust (s ^. uiState . uiError) = continue $ s & uiState . uiError .~ Nothing
  | otherwise                       = halt s
handleEvent s ev =
  case focusGetCurrent (s ^. uiState . uiFocusRing) of
    Just REPLPanel  -> handleREPLEvent s ev
    Just WorldPanel -> handleWorldEvent s ev
    _               -> continueWithoutRedraw s

handleREPLEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleREPLEvent s (VtyEvent (V.EvKey V.KEnter []))
  = case processCmd entry of
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
handleREPLEvent s (VtyEvent (V.EvKey V.KUp []))
  = continue $ s & uiState %~ adjReplHistIndex (+)
handleREPLEvent s (VtyEvent (V.EvKey V.KDown []))
  = continue $ s & uiState %~ adjReplHistIndex (-)
handleREPLEvent s ev = do
  f' <- handleFormEvent ev (s ^. uiState . uiReplForm)
  let result = processCmd (formState f')
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

worldScrollDist :: Int
worldScrollDist = 8

handleWorldEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleWorldEvent s (VtyEvent (V.EvKey k []))
  | k `elem` [V.KUp, V.KDown, V.KLeft, V.KRight]
  = updateView s (^+^ (worldScrollDist *^ keyToDir k)) >>= continue
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar '<') []))
  = adjustTPS (-) s >> continueWithoutRedraw s
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar '>') []))
  = adjustTPS (+) s >> continueWithoutRedraw s

-- Fall-through case: don't do anything.
handleWorldEvent s _ = continueWithoutRedraw s

updateView :: AppState -> (V2 Int -> V2 Int) -> EventM Name AppState
updateView s update = do
  invalidateCacheEntry WorldCache
  let s' = s & gameState . viewCenter %~ update
  mext <- lookupExtent WorldExtent
  case mext of
    Nothing  -> return s'
    Just (Extent _ _ size) -> return $
      s' & gameState . world %~ W.loadRegion (viewingRegion (s' ^. gameState) size)

keyToDir :: V.Key -> V2 Int
keyToDir V.KUp    = north
keyToDir V.KDown  = south
keyToDir V.KRight = east
keyToDir V.KLeft  = west
keyToDir _        = V2 0 0

viewingRegion :: GameState -> (Int,Int) -> ((Int, Int), (Int, Int))
viewingRegion g (w,h) = ((rmin,cmin), (rmax,cmax))
  where
    V2 cr cc = g ^. viewCenter
    (rmin,rmax) = over both (+ (cr - h`div`2)) (0, h-1)
    (cmin,cmax) = over both (+ (cc - w`div`2)) (0, w-1)

adjustTPS :: (Int -> Int -> Int) -> AppState -> EventM Name ()
adjustTPS (+/-) s =
  liftIO $ atomically $ modifyTVar (s ^. uiState . lgTicksPerSecond) (+/- 1)
