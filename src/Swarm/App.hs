module Swarm.App where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad      (forever, void)

import           Brick
import           Brick.BChan
import qualified Graphics.Vty       as V

import           Swarm.UI
import           Swarm.UI.Attr

app :: App AppState Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theAttrMap
  }

appMain :: IO ()
appMain = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 50000
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  s <- initAppState
  void $ customMain initialVty buildVty (Just chan) app s
