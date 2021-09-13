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

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Lens                ((^.))
import           Data.Bits                   (shiftL)

import           Brick
import           Brick.BChan
import qualified Graphics.Vty                as V

import           Control.Monad.Except
import qualified Data.Text.IO                as T
import           Swarm.TUI
import           Swarm.TUI.Attr

-- | The definition of the app used by the @brick@ library.
app :: App AppState Tick Name
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
