
-- |
-- Module      :  Swarm.App
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Main entry point for the Swarm application.
module Swarm.App where

import Control.Concurrent (forkIO, threadDelay)

import Brick
import Brick.BChan
import Graphics.Vty qualified as V

import Control.Monad.Except
import Data.Text.IO qualified as T
import Swarm.TUI.Attr
import Swarm.TUI.Controller
import Swarm.TUI.Model
import Swarm.TUI.View

-- | The definition of the app used by the @brick@ library.
app :: App AppState AppEvent Name
app =
  App
    { appDraw = drawUI
    , appChooseCursor = chooseCursor
    , appHandleEvent = handleEvent
    , appStartEvent = (<$ enablePasteMode)
    , appAttrMap = const swarmAttrMap
    }

-- | The main @IO@ computation which initializes the state, sets up
--   some communication channels, and runs the UI.
appMain :: Maybe Seed -> Maybe String -> Maybe String -> Bool -> IO ()
appMain seed scenario toRun cheat = do
  res <- runExceptT $ initAppState seed scenario toRun cheat
  case res of
    Left errMsg -> T.putStrLn errMsg
    Right s -> do
      -- Send Frame events as at a reasonable rate for 30 fps. The
      -- game is responsible for figuring out how many steps to take
      -- each frame to achieve the desired speed, regardless of the
      -- frame rate.  Note that if the game cannot keep up with 30
      -- fps, it's not a problem: the channel will fill up and this
      -- thread will block.  So the force of the threadDelay is just
      -- to set a *maximum* possible frame rate.
      --
      -- 5 is the size of the bounded channel; when it gets that big,
      -- any writes to it will block.  Probably 1 would work fine,
      -- though it seems like it could be good to have a bit of buffer
      -- just so the app never has to wait for the thread to wake up
      -- and do another write.

      chan <- newBChan 5
      _ <- forkIO $
        forever $ do
          threadDelay 33_333 -- cap maximum framerate at 30 FPS
          writeBChan chan Frame

      -- Run the app.

      let buildVty = V.mkVty V.defaultConfig
      initialVty <- buildVty
      V.setMode (V.outputIface initialVty) V.Mouse True
      void $ customMain initialVty buildVty (Just chan) app s

-- | If available for the terminal emulator, enable bracketed paste mode.
enablePasteMode :: EventM s ()
enablePasteMode = do
  vty <- getVtyHandle
  let output = V.outputIface vty
  when (V.supportsMode output V.BracketedPaste) $
    liftIO $ V.setMode output V.BracketedPaste True
