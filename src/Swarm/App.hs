-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.App
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Main entry point for the Swarm application.
--
-----------------------------------------------------------------------------

{-# LANGUAGE NumericUnderscores #-}

module Swarm.App where

import           Control.Concurrent   (forkIO)

import           Brick
import           Brick.BChan
import qualified Graphics.Vty         as V

import           Control.Monad.Except
import qualified Data.Text.IO         as T
import           Swarm.TUI
import           Swarm.TUI.Attr
import           Swarm.TUI.Model
import           Swarm.TUI.View

-- | The definition of the app used by the @brick@ library.
app :: App AppState AppEvent Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const swarmAttrMap
  }

-- | The main @IO@ computation which initializes the state, sets up
--   some communication channels, and runs the UI.
appMain :: IO ()
appMain = do
  res <- runExceptT initAppState
  case res of
    Left errMsg -> T.putStrLn errMsg
    Right s -> do

      -- Just send Frame events as fast as possible.  The game is
      -- responsible for figuring out how many steps to take each
      -- frame to achieve the desired speed, regardless of the frame
      -- rate.
      --
      -- 5 is the size of the bounded channel; when it gets that big,
      -- any writes to it will block.  Probably 1 would work fine,
      -- though it seems like it could be good to have a bit of buffer
      -- just so the app never has to wait for the thread to wake up
      -- and do another write.

      chan <- newBChan 5
      _ <- forkIO $ forever $ writeBChan chan Frame

      -- Run the app.

      let buildVty = V.mkVty V.defaultConfig
      initialVty <- buildVty
      void $ customMain initialVty buildVty (Just chan) app s
