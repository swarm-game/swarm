{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Swarm.TUI where

import           Control.Arrow               ((&&&))
import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Data.Array                  (range)
import           Data.Either                 (isRight)
import           Data.List                   (sortOn)
import           Data.List.Split             (chunksOf)
import qualified Data.Map                    as M
import           Data.Maybe                  (isJust)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import           Linear
import           Text.Read                   (readMaybe)
import           Witch                       (into)

import           Brick                       hiding (Direction)
import           Brick.Focus
import           Brick.Forms
import           Brick.Widgets.Border        (hBorder)
import           Brick.Widgets.Center        (center, hCenter)
import           Brick.Widgets.Dialog
import qualified Brick.Widgets.List          as BL
import qualified Graphics.Vty                as V

import           Control.Monad.State
import           Swarm.Game
import qualified Swarm.Game.Entity           as E
import           Swarm.Game.Robot            (installedDevices)
import           Swarm.Game.Terrain          (displayTerrain)
import qualified Swarm.Game.World            as W
import           Swarm.Language.Pipeline
import           Swarm.Language.Syntax       (east, north, south, west)
import           Swarm.TUI.Attr
import           Swarm.TUI.Panel
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
  | InventoryList
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

------------------------------------------------------------
-- UI state

data REPLHistItem = REPLEntry Bool Text | REPLOutput Text
  deriving (Eq, Ord, Show, Read)

data UIState = UIState
  { _uiFocusRing      :: FocusRing Name
  , _uiReplForm       :: Form Text Tick Name
  , _uiReplHistory    :: [REPLHistItem]
  , _uiReplHistIdx    :: Int
  , _uiInventory      :: Maybe (Int, BL.List Name (Count, Entity))
    -- ^ Stores the hash value of the focused robot entity (so we can
    --   tell if its inventory changed) along with a list with the
    --   items in the focused robot's inventory.
  , _uiError          :: Maybe (Widget Name)
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
  mhist <- (>>= readMaybe @[REPLHistItem]) <$> readFileMay ".swarm_history"
  return $ UIState
    { _uiFocusRing      = initFocusRing
    , _uiReplForm       = initReplForm
    , _uiReplHistory    = mhist ? []
    , _uiReplHistIdx    = -1
    , _uiInventory      = Nothing
    , _uiError          = Nothing
    , _lgTicksPerSecond = tv
    }

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
  , joinBorders $
    hBox
    [ hLimitPercent 25 $ panel highlightAttr fr InfoPanel $ drawInfoPanel s
    , vBox
      [ panel highlightAttr fr WorldPanel $ drawWorld (s ^. gameState)
      , drawMenu (s ^. uiState)
      , panel highlightAttr fr REPLPanel $ vLimit replHeight $ padBottom Max $ padLeftRight 1 $ drawRepl s
      ]
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

drawMenu :: UIState -> Widget Name
drawMenu
  = vLimit 1
  . hBox . map (padLeftRight 1 . drawKeyCmd)
  . (globalKeyCmds++) . keyCmdsFor . focusGetCurrent . view uiFocusRing
  where
    globalKeyCmds =
      [ ("^Q", "quit")
      , ("Tab", "cycle panels")
      ]
    keyCmdsFor (Just REPLPanel) =
      [ ("Enter", "execute")
      ]
    keyCmdsFor (Just WorldPanel) =
      [ ("←↓↑→ / hjkl", "scroll")
      , ("<>", "slower/faster")
      ]
    keyCmdsFor (Just InfoPanel)  =
      []
    keyCmdsFor _ = []

drawKeyCmd :: (Text, Text) -> Widget Name
drawKeyCmd (key, cmd) = txt $ T.concat [ "[", key, "] ", cmd ]

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
    -- XXX update how this works!  Gather all displays, all
    -- entities...  Should make a Display remember which is the
    -- currently selected char (based on orientation); Entity lens for
    -- setting orientation updates the Display too.  Then we can just
    -- get all the Displays for each cell, make a monoid based on
    -- priority.

    robotsByLoc
      = M.fromListWith (maxOn (^. robotDisplay . displayPriority)) . map (view robotLocation &&& id)
      . M.elems $ g ^. robotMap
    drawLoc (row,col) = case M.lookup (V2 col (-row)) robotsByLoc of
      Just r  -> withAttr (r ^. robotDisplay . displayAttr)
                 $ str [lookupDisplay (r ^. robotOrientation) (r ^. robotDisplay)]
      Nothing -> drawCell (row,col) (g ^. world)

drawCell :: W.Worldly w Int Entity => (Int, Int) -> w -> Widget Name
drawCell i w = case W.lookupEntity i w of
  Just e  -> displayEntity e
  Nothing -> displayTerrain (toEnum (W.lookupTerrain i w))

drawInfoPanel :: AppState -> Widget Name
drawInfoPanel s
  = vBox
    [ drawRobotInfo (s ^. gameState)
    , hBorder
    , vLimitPercent 50 $ padBottom Max $ drawMessages (s ^. gameState . messageQueue)
    ]

drawMessages :: [Text] -> Widget Name
drawMessages [] = txt " "
drawMessages ms = Widget Fixed Fixed $ do
  ctx <- getContext
  let h   = ctx ^. availHeightL
  render . vBox . map txt . reverse . take h $ ms

drawRobotInfo :: GameState -> Widget Name
drawRobotInfo = drawRobotInfoFor . focusedRobot

drawRobotInfoFor :: Maybe Robot -> Widget Name
drawRobotInfoFor Nothing = padBottom Max $ str " "
drawRobotInfoFor (Just r)
  = padBottom Max
  $ vBox
  [ hCenter $ txt (r ^. robotName)
  , padAll 1
    $ vBox
    [ drawInventory (r ^. robotInventory)
    , txt " "
    , drawInventory (r ^. installedDevices)
    ]
  ]

drawInventory :: Inventory -> Widget Name
drawInventory = vBox . map drawItem . sortOn (view entityName . snd) . E.elems

-- drawInstalledDevices :: Robot -> Widget Name
-- drawInstalledDevices r
--   = hBox . map (displayEntity . snd) . elems $ (r ^. installedDevices)

drawItem :: (Int, Entity) -> Widget Name
drawItem (1, e) = drawLabelledEntityName e
drawItem (n, e) = drawLabelledEntityName e <+> showCount n
  where
    showCount = padLeft Max . str . show

drawLabelledEntityName :: Entity -> Widget Name
drawLabelledEntityName e = hBox
  [ padRight (Pad 2) (displayEntity e)
  , txt (e ^. entityName)
  ]

drawRepl :: AppState -> Widget Name
drawRepl s = vBox $
  map fmt (reverse (take (replHeight - 1) . filter newEntry $ (s ^. uiState . uiReplHistory)))
  ++
  case isActive <$> (s ^. gameState . robotMap . at "base") of
    Just False -> [ renderForm (s ^. uiState . uiReplForm) ]
    _          -> [ padRight Max $ txt "..." ]
  where
    newEntry (REPLEntry False _) = False
    newEntry _                   = True

    fmt (REPLEntry _ e) = txt replPrompt <+> txt e
    fmt (REPLOutput t)  = txt t

------------------------------------------------------------
-- Event handling

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent s (AppEvent Tick) = execStateT handleTick s >>= continue
  where
    handleTick :: StateT AppState (EventM Name) ()
    handleTick = do

      -- Run one step of the game
      zoom gameState gameStep

      -- If things were updated, invalidate the world cache so it will be redrawn.
      g <- use gameState
      when (g ^. updated) $ lift (invalidateCacheEntry WorldCache)

      -- Check if the inventory list needs to be updated.
      listRobotHash    <- fmap fst <$> use (uiState . uiInventory)
        -- The hash of the robot whose inventory is currently displayed (if any)

      fr <- use (gameState . to focusedRobot)
      let focusedRobotHash = view (robotEntity . entityHash) <$> fr
        -- The hash of the focused robot (if any)

      -- If the hashes don't match (either because which robot (or
      -- whether any robot) is focused changed, or the focused robot's
      -- inventory changed), regenerate the list.
      when (listRobotHash /= focusedRobotHash) (zoom uiState $ populateInventoryList fr)

      -- Now check if the base finished running a program entered at the REPL.
      case g ^. replResult of

        -- It did, and the result was the unit value.  Just reset replResult.
        Just (_, Just VUnit) -> gameState . replResult .= Nothing

        -- It did, and returned some other value.  Pretty-print the
        -- result as a REPL output, and reset the replResult.
        Just (_ty, Just v) -> do
          uiState . uiReplHistory %= (REPLOutput (into (prettyValue v)) :)
          gameState . replResult .= Nothing

        -- Otherwise, do nothing.
        _ -> return ()

handleEvent s (VtyEvent (V.EvResize _ _))            = do
  invalidateCacheEntry WorldCache
  continue s
handleEvent s (VtyEvent (V.EvKey (V.KChar '\t') [])) = continue $ s & uiState . uiFocusRing %~ focusNext
handleEvent s (VtyEvent (V.EvKey V.KBackTab []))     = continue $ s & uiState . uiFocusRing %~ focusPrev
handleEvent s (VtyEvent (V.EvKey V.KEsc []))
  | isJust (s ^. uiState . uiError) = continue $ s & uiState . uiError .~ Nothing
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt s
handleEvent s ev =
  case focusGetCurrent (s ^. uiState . uiFocusRing) of
    Just REPLPanel  -> handleREPLEvent s ev
    Just WorldPanel -> handleWorldEvent s ev
    _               -> continueWithoutRedraw s

populateInventoryList :: MonadState UIState m => Maybe Robot -> m ()
populateInventoryList Nothing  = uiInventory .= Nothing
populateInventoryList (Just r) = do
  let lst = BL.list InventoryList (V.fromList (r ^. robotInventory . to elems)) 1
  uiInventory .= Just (r ^. robotEntity . entityHash, lst)

handleREPLEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleREPLEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]))
  = continue $ s
      & gameState . robotMap . ix "base" . machine .~ idleMachine
handleREPLEvent s (VtyEvent (V.EvKey V.KEnter []))
  = case processTerm entry of
      Right (t ::: ty) ->
        continue $ s
          & uiState . uiReplForm    %~ updateFormState ""
          & uiState . uiReplHistory %~ (REPLEntry True entry :)
          & uiState . uiReplHistIdx .~ (-1)
          & gameState . replResult ?~ (ty, Nothing)
          & gameState . robotMap . ix "base" . machine .~ initMachine t ty
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
  let result = processTerm (formState f')
      f''    = setFieldValid (isRight result) REPLInput f'
  continue $ s & uiState . uiReplForm .~ f''

adjReplHistIndex :: (Int -> Int -> Int) -> UIState -> UIState
adjReplHistIndex (+/-) s =
  s & uiReplHistIdx .~ newIndex
    & if newIndex /= curIndex then uiReplForm %~ updateFormState newEntry else id
  where
    entries = [e | REPLEntry _ e <- s ^. uiReplHistory]
    curIndex = s ^. uiReplHistIdx
    histLen  = length entries
    newIndex = min (histLen - 1) (max (-1) (curIndex +/- 1))
    newEntry
      | newIndex == -1 = ""
      | otherwise      = entries !! newIndex

worldScrollDist :: Int
worldScrollDist = 8

handleWorldEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleWorldEvent s (VtyEvent (V.EvKey k []))
  | k `elem` [ V.KUp, V.KDown, V.KLeft, V.KRight
             , V.KChar 'h', V.KChar 'j', V.KChar 'k', V.KChar 'l' ]
  = scrollView s (^+^ (worldScrollDist *^ keyToDir k)) >>= continue
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar '<') []))
  = adjustTPS (-) s >> continueWithoutRedraw s
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar '>') []))
  = adjustTPS (+) s >> continueWithoutRedraw s
handleWorldEvent s (VtyEvent (V.EvKey (V.KChar 'm') []))
  = continueWithoutRedraw (s & gameState . gameMode %~ cycleEnum)

-- Fall-through case: don't do anything.
handleWorldEvent s _ = continueWithoutRedraw s

cycleEnum :: (Eq e, Enum e, Bounded e) => e -> e
cycleEnum e
  | e == maxBound = minBound
  | otherwise     = succ e


scrollView :: AppState -> (V2 Int -> V2 Int) -> EventM Name AppState
scrollView s update =
  updateView $ s & gameState %~ manualViewCenterUpdate update

updateView :: AppState -> EventM Name AppState
updateView s = do
  invalidateCacheEntry WorldCache
  mext <- lookupExtent WorldExtent
  case mext of
    Nothing  -> return s
    Just (Extent _ _ size) -> return $
      s & gameState . world %~ W.loadRegion (viewingRegion (s ^. gameState) size)

keyToDir :: V.Key -> V2 Int
keyToDir V.KUp         = north
keyToDir V.KDown       = south
keyToDir V.KRight      = east
keyToDir V.KLeft       = west
keyToDir (V.KChar 'h') = west
keyToDir (V.KChar 'j') = south
keyToDir (V.KChar 'k') = north
keyToDir (V.KChar 'l') = east
keyToDir _             = V2 0 0

viewingRegion :: GameState -> (Int,Int) -> ((Int, Int), (Int, Int))
viewingRegion g (w,h) = ((rmin,cmin), (rmax,cmax))
  where
    V2 cx cy = g ^. viewCenter
    (rmin,rmax) = over both (+ (-cy - h`div`2)) (0, h-1)
    (cmin,cmax) = over both (+ (cx - w`div`2)) (0, w-1)

adjustTPS :: (Int -> Int -> Int) -> AppState -> EventM Name ()
adjustTPS (+/-) s =
  liftIO $ atomically $ modifyTVar (s ^. uiState . lgTicksPerSecond) (+/- 1)

shutdown :: AppState -> EventM Name (Next AppState)
shutdown s = do
  let s'   = s & uiState . uiReplHistory . traverse %~ markOld
      hist = filter isEntry (s' ^. uiState . uiReplHistory)
  liftIO $ writeFile ".swarm_history" (show hist)
  halt s'

  where
    markOld (REPLEntry _ e) = REPLEntry False e
    markOld r               = r

    isEntry REPLEntry{} = True
    isEntry _           = False
