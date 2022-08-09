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
import Data.Maybe (fromMaybe)
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
    Just mv -> Warp.setBeforeMainLoop $ putMVar mv ()
    Nothing -> id
  app :: Network.Wai.Application
  app = Servant.serve (Proxy @SwarmApi) (mkApp gsRef)

defaultPort :: Warp.Port
defaultPort = 5357

-- | Attempt to start a web thread on the requested port, or a default
--   one if none is requested (or don't start a web thread if the
--   requested port is 0).  If an explicit port was requested, fail if
--   startup doesn't work.  Otherwise, ignore the failure.  In any
--   case, return a @Maybe Port@ value representing whether a web
--   server is actually running, and if so, what port it is on.
startWebThread :: Maybe Warp.Port -> IORef GameState -> IO (Maybe Warp.Port)
-- User explicitly provided port '0': don't run the web server
startWebThread (Just 0) _ = pure Nothing
startWebThread portM gsRef = do
  baton <- newEmptyMVar
  let port = fromMaybe defaultPort portM
  void $ forkIO $ webMain (Just baton) port gsRef
  res <- timeout 500_000 (takeMVar baton)
  case (portM, res) of
    -- User requested explicit port but server didn't start: fail
    (Just _, Nothing) -> fail $ "Failed to start the web API  on :" <> show port
    -- Otherwise, just report whether the server is running, and if so, on what port
    _ -> return (port <$ res)
