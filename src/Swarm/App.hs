{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Main entry point for the Swarm application.
module Swarm.App where

import Brick
import Brick.BChan
import Control.Carrier.Lift (runM)
import Control.Carrier.Throw.Either (runThrow)
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens (view, (%~), (&), (?~))
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, writeIORef)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Graphics.Vty qualified as V
import Swarm.Game.Failure.Render (prettyFailure)
import Swarm.Game.Robot (ErrorLevel (..), LogSource (ErrorTrace, Said))
import Swarm.ReadableIORef (mkReadonly)
import Swarm.TUI.Controller
import Swarm.TUI.Model
import Swarm.TUI.Model.StateUpdate
import Swarm.TUI.Model.UI (uiAttrMap)
import Swarm.TUI.View
import Swarm.Version (getNewerReleaseVersion)
import Swarm.Web
import System.IO (stderr)

type EventHandler = BrickEvent Name AppEvent -> EventM Name AppState ()

-- | The definition of the app used by the @brick@ library.
app :: EventHandler -> App AppState AppEvent Name
app eventHandler =
  App
    { appDraw = drawUI
    , appChooseCursor = chooseCursor
    , appHandleEvent = eventHandler
    , appStartEvent = enablePasteMode
    , appAttrMap = view $ uiState . uiAttrMap
    }

-- | The main @IO@ computation which initializes the state, sets up
--   some communication channels, and runs the UI.
appMain :: AppOpts -> IO ()
appMain opts = do
  res <- runM . runThrow $ initAppState opts
  case res of
    Left err -> T.hPutStrLn stderr (prettyFailure err)
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

      _ <- forkIO $ do
        upRel <- getNewerReleaseVersion (repoGitInfo opts)
        writeBChan chan (UpstreamVersion upRel)

      -- Start the web service with a reference to the game state.
      -- NOTE: This reference should be considered read-only by
      -- the web service; the game alone shall host the canonical state.
      appStateRef <- newIORef s
      eport <-
        Swarm.Web.startWebThread
          (userWebPort opts)
          (mkReadonly appStateRef)
          chan

      let logP p = logEvent Said ("Web API", -2) ("started on :" <> T.pack (show p))
      let logE e = logEvent (ErrorTrace Error) ("Web API", -2) (T.pack e)
      let s' =
            s
              & runtimeState
                %~ case eport of
                  Right p -> (webPort ?~ p) . (eventLog %~ logP p)
                  Left e -> eventLog %~ logE e

      -- Update the reference for every event
      let eventHandler e = do
            curSt <- get
            liftIO $ writeIORef appStateRef curSt
            handleEvent e

      -- Setup virtual terminal
      let buildVty = V.mkVty $ V.defaultConfig {V.colorMode = colorMode opts}
      initialVty <- buildVty
      V.setMode (V.outputIface initialVty) V.Mouse True

      -- Run the app.
      void $ customMain initialVty buildVty (Just chan) (app eventHandler) s'

-- | A demo program to run the web service directly, without the terminal application.
-- This is useful to live update the code using `ghcid -W --test "Swarm.App.demoWeb"`
demoWeb :: IO ()
demoWeb = do
  let demoPort = 8080
  res <-
    runM . runThrow $ initAppState (defaultAppOpts {userScenario = demoScenario})
  case res of
    Left err -> T.putStrLn (prettyFailure err)
    Right s -> do
      appStateRef <- newIORef s
      chan <- newBChan 5
      webMain
        Nothing
        demoPort
        (mkReadonly appStateRef)
        chan
 where
  demoScenario = Just "./data/scenarios/Testing/475-wait-one.yaml"

-- | If available for the terminal emulator, enable bracketed paste mode.
enablePasteMode :: EventM n s ()
enablePasteMode = do
  vty <- getVtyHandle
  let output = V.outputIface vty
  when (V.supportsMode output V.BracketedPaste) $
    liftIO $
      V.setMode output V.BracketedPaste True
