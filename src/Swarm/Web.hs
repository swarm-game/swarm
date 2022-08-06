{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- A web service for Swarm.
--
-- The service can be started using the `--port 8080` command line argument,
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

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef)
import Data.IntMap qualified as IM
import Network.Wai qualified
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified
import Servant
import Swarm.Game.Robot
import Swarm.Game.State

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

webMain :: Port -> IORef GameState -> IO ()
webMain port gsRef = do
  putStrLn $ "Web interface listening on :" <> show port
  Network.Wai.Handler.Warp.run port app
 where
  app :: Network.Wai.Application
  app = Servant.serve (Proxy @SwarmApi) (mkApp gsRef)
