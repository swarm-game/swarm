{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- A web service for Swarm.
--
-- The service can be started using the `--port 5357` command line argument,
-- or through the REPL by calling `Swarm.App.demoWeb`.
--
-- Once running, here are the available endpoints:
--
--   * /robots : return the list of robots
--   * /robot/ID : return a single robot identified by its id
--
-- Missing endpoints:
--
--   * TODO: #625 run endpoint to load definitions
--   * TODO: #493 export the whole game state
module Swarm.Web where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef)
import Data.IntMap qualified as IM
import Network.Wai qualified
import Network.Wai.Handler.Warp qualified as Warp
import Servant
import Swarm.Game.Robot
import Swarm.Game.State
import System.Timeout (timeout)

type SwarmApi =
  "robots" :> Get '[JSON] [Robot]
    :<|> "robot" :> Capture "id" Int :> Get '[JSON] (Maybe Robot)

mkApp :: IORef GameState -> Servant.Server SwarmApi
mkApp gsRef =
  robotsHandler
    :<|> robotHandler
 where
  robotsHandler = do
    g <- liftIO (readIORef gsRef)
    pure $ IM.elems $ g ^. robotMap
  robotHandler rid = do
    g <- liftIO (readIORef gsRef)
    pure $ IM.lookup rid (g ^. robotMap)

webMain :: Maybe (MVar ()) -> Warp.Port -> IORef GameState -> IO ()
webMain baton port gsRef = do
  let settings = Warp.setPort port $ onReady Warp.defaultSettings
  Warp.runSettings settings app
 where
  onReady = case baton of
    Just mv -> Warp.setBeforeMainLoop $ do
      putStrLn $ "Web interface listening on :" <> show port
      putMVar mv ()
    Nothing -> id
  app :: Network.Wai.Application
  app = Servant.serve (Proxy @SwarmApi) (mkApp gsRef)

defaultPort :: Warp.Port
defaultPort = 5357

startWebThread :: Maybe Warp.Port -> IORef GameState -> IO ()
startWebThread portM gsRef = do
  res <- go
  case res of
    Nothing -> fail "Fail to start the web api"
    Just _ -> pure ()
 where
  go = case portM of
    Just 0 -> pure (Just ())
    Just port -> do
      -- The user provided a port, so we ensure the api does starts
      baton <- newEmptyMVar
      void $ forkIO $ webMain (Just baton) port gsRef
      timeout 500_000 (takeMVar baton)
    Nothing -> do
      -- No port was given, the api may fail to start
      void $ forkIO $ webMain Nothing defaultPort gsRef
      pure (Just ())
