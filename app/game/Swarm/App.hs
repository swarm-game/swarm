{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use underscore" -}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Application entry point
--
-- Main entry point for the Swarm application.
module Swarm.App (
  app,
  appMain,
  EventHandler,

  -- * Metrics
  defaultMetrics,

  -- * Demo web
  demoWeb,
) where

import Brick
import Brick.BChan
import Control.Carrier.Lift (runM)
import Control.Carrier.Throw.Either (runThrow)
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens (Setter', view, (%~), (?~), (^.))
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GitHash (GitInfo)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform qualified as V
import Swarm.Failure (SystemFailure)
import Swarm.Game.State.Runtime
import Swarm.Log (LogSource (SystemLog), Severity (..))
import Swarm.Pretty (prettyText)
import Swarm.TUI.Controller
import Swarm.TUI.Model
import Swarm.TUI.Model.StateUpdate
import Swarm.TUI.Model.UI (uiAttrMap)
import Swarm.TUI.View
import Swarm.Version (getNewerReleaseVersion)
import Swarm.Web
import System.Exit
import System.IO (stderr)
import System.Metrics (Store)
import System.Remote.Monitoring.Wai qualified as WaiMetrics

type EventHandler = BrickEvent Name AppEvent -> EventM Name AppState ()

-- | The configuration of the Swarm app which we pass to the @brick@
--   library.
app :: EventHandler -> App AppState AppEvent Name
app eventHandler =
  App
    { appDraw = drawUI
    , appChooseCursor = chooseCursor
    , appHandleEvent = eventHandler
    , appStartEvent = pure ()
    , appAttrMap = view $ uiState . uiAttrMap
    }

-- | The main @IO@ computation which initializes the state, sets up
--   some communication channels, and runs the UI.
appMain :: AppOpts -> IO ()
appMain opts = do
  chan <- createChannel
  res <- runM . runThrow $ initAppState opts (Just chan)
  case res of
    Left err -> do
      T.hPutStrLn stderr (prettyText @SystemFailure err)
      exitFailure
    Right s -> do
      -- NOTE: The state reference is passed read-only by the web service;
      -- the brick app has the real state and updates the reference.
      appStateRef <- newIORef s

      eMetrics <- startMetricsThread opts.userMetricsPort (s ^. runtimeState . metrics)
      modifyIORef appStateRef $ logPort "Metrics API" metricsPort eMetrics

      sendFrameEvents chan
      sendUpstreamVersion chan opts.repoGitInfo

      -- Start web service
      eport <-
        Swarm.Web.startWebThread
          opts.userWebPort
          (readIORef appStateRef)
          (writeBChan chan)

      modifyIORef appStateRef $ logPort "Web API" webPort eport

      -- Setup virtual terminal
      vty <- buildVty opts.colorMode
      modifyIORef appStateRef $ logColorMode vty

      -- Run the app.
      sFinal <- readIORef appStateRef
          >>= customMain
            vty
            (buildVty opts.colorMode)
            (Just chan)
            (app $ handleEventAndUpdateWeb appStateRef)
      
      -- Finish writing logs before exiting
      waitForLogger (sFinal ^. runtimeState . logger)


-- | A demo program to run the web service directly, without the terminal application.
--   This is useful to live update the code using @ghcid -W --test "Swarm.App.demoWeb"@.
demoWeb :: IO ()
demoWeb = do
  let demoPort = 8080
  chan <- createChannel
  res <-
    runM . runThrow $ initAppState (defaultAppOpts {userScenario = demoScenario}) (Just chan)
  case res of
    Left err -> T.putStrLn (prettyText @SystemFailure err)
    Right s -> do
      webMain
        Nothing
        demoPort
        (pure s)
        (writeBChan chan)
 where
  demoScenario = Just "./data/scenarios/Testing/475-wait-one.yaml"

defaultMetrics :: Int
defaultMetrics = 6543

startMetricsThread :: Maybe Int -> Store -> IO (Either String Int)
startMetricsThread (Just 0) _ = pure $ Left "Metrics API disabled."
startMetricsThread mPort store = do
  let p = fromMaybe 6543 mPort
  _ <- WaiMetrics.forkServerWith store "localhost" p
  pure $ Right p

-- | Create a channel for app events.
--
-- 5 is the size of the bounded channel; when it gets that big,
-- any writes to it will block.  Probably 1 would work fine,
-- though it seems like it could be good to have a bit of buffer
-- just so the app never has to wait for the thread to wake up
-- and do another write.
--
-- Note that there are occasionally other events (web, version)
-- so this buffer is big enough for them too.
createChannel :: IO (BChan AppEvent)
createChannel = newBChan 5

-- | Send Frame events as at a reasonable rate for 30 fps.
--
-- The game is responsible for figuring out how many steps to take
-- each frame to achieve the desired speed, regardless of the
-- frame rate.  Note that if the game cannot keep up with 30
-- fps, it's not a problem: the channel will fill up and this
-- thread will block.  So the force of the threadDelay is just
-- to set a *maximum* possible frame rate.
sendFrameEvents :: BChan AppEvent -> IO ()
sendFrameEvents chan = void . forkIO . forever $ do
  writeBChan chan Frame
  threadDelay 33_333 -- cap maximum framerate at 30 FPS

-- | Get newer upstream version and send event to channel.
sendUpstreamVersion :: BChan AppEvent -> Maybe GitInfo -> IO ()
sendUpstreamVersion chan gitInfo = void . forkIO $ do
  upRel <- getNewerReleaseVersion gitInfo
  writeBChan chan (UpstreamVersion upRel)

-- | Log and save the web port or log web startup failure.
logPort ::
  T.Text ->
  Setter' RuntimeState (Maybe Int) ->
  Either String Int ->
  AppState ->
  AppState
logPort api savePort eport =
  runtimeState %~ case eport of
    Right p -> (savePort ?~ p) . (eventLog %~ logP p)
    Left e -> eventLog %~ logE e
 where
  logP p = logEvent SystemLog Info api ("started on :" <> T.pack (show p))
  logE e = logEvent SystemLog Error api (T.pack e)

-- | Build VTY with preffered color mode and bracketed paste mode if available.
--
-- Note that this will also run whenever the event loop needs to reinitialize
-- the terminal, e.g. on resume after suspension. See 'customMain'.
buildVty :: Maybe ColorMode -> IO V.Vty
buildVty cm = do
  vty <- V.mkVty V.defaultConfig {V.configPreferredColorMode = cm}
  let output = V.outputIface vty
  V.setMode output V.Mouse True
  when (V.supportsMode output V.BracketedPaste) $
    V.setMode output V.BracketedPaste True
  return vty

-- | Log the VTY color mode to system log.
logColorMode :: V.Vty -> AppState -> AppState
logColorMode vty = runtimeState . eventLog %~ logEvent SystemLog Info "Graphics" ("Color mode: " <> T.pack (show cm))
 where
  cm = V.outputColorMode $ V.outputIface vty

-- | Update the reference after every event.
handleEventAndUpdateWeb :: IORef AppState -> BrickEvent Name AppEvent -> EventM Name AppState ()
handleEventAndUpdateWeb appStateRef e = do
  handleEvent e
  curSt <- get
  liftIO $ writeIORef appStateRef curSt
