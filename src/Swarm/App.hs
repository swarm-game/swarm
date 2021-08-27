{-# LANGUAGE NumericUnderscores #-}

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

module Swarm.App where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Lens                ((^.))
import           Control.Monad               (forever, void)
import           Data.Bits                   (shiftL)

import           Brick
import           Brick.BChan
import qualified Graphics.Vty                as V

import           Swarm.UI
import           Swarm.UI.Attr

-- | The definition of the app used by the @brick@ library.
app :: App AppState Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theAttrMap
  }

-- | The main @IO@ computation which initializes the state, sets up
--   some communication channels, and runs the UI.
appMain :: IO ()
appMain = do

  s <- initAppState

  chan <- newBChan 10
  let tpsTV = s ^. uiState . lgTicksPerSecond
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    lgTPS <- readTVarIO tpsTV
    let delay
          | lgTPS < 0 = 1_000_000 * (1 `shiftL` (-lgTPS))
          | otherwise = 1_000_000 `div` (1 `shiftL` lgTPS)
    threadDelay delay

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app s
