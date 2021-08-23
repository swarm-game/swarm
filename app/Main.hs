{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

-- XXX way to configure to use fancy Unicode characters or stick to ASCII

module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens
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
