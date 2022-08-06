{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Swarm.Web where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef)
import Data.IntMap qualified as IM
import Data.Maybe (fromMaybe)
import Network.Wai qualified
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified
import Servant
import Swarm.Game.Robot
import Swarm.Game.State

type SwarmApi =
  "robots" :> Get '[JSON] [Robot]
    :<|> "robot" :> Capture "id" Int :> Get '[JSON] Robot

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
    pure $ fromMaybe (error "Unknown robot") (IM.lookup rid (g ^. robotMap))

webMain :: Port -> IORef GameState -> IO ()
webMain port gsRef = do
  putStrLn $ "Web interface listening on :" <> show port
  Network.Wai.Handler.Warp.run port app
 where
  app :: Network.Wai.Application
  app = Servant.serve (Proxy @SwarmApi) (mkApp gsRef)
