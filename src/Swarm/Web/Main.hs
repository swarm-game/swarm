{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Swarm.Web.Main where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef)
import Data.Text (Text)
import Lucid
import Network.Wai qualified
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified
import Paths_swarm (getDataDir)
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Server.StaticFiles qualified

import Swarm.Game.State (GameState)
import Swarm.Web.Index

type GetRequest = Header "HX-Request" Text :> Get '[HTML] (Html ())

type SwarmApi =
  GetRequest
    :<|> "robots" :> GetRequest
    :<|> "robot" :> Capture "id" Int :> GetRequest
    :<|> "about" :> GetRequest
    :<|> "dists" :> Raw

mkApp :: IORef GameState -> FilePath -> Servant.Server SwarmApi
mkApp gsRef dataDir =
  handlePage WelcomePage
    :<|> handlePage RobotsPage
    :<|> handlePage . RobotPage
    :<|> handlePage AboutPage
    :<|> Servant.Server.StaticFiles.serveDirectoryWebApp dataDir
 where
  handlePage page htmxRequest = do
    gs <- liftIO $ readIORef gsRef
    -- Here we handle two situations.
    pure $ case htmxRequest of
      -- The HX-Request header is not set, thus we send the whole page
      Nothing -> mainBody gs page
      -- Otherwise, we send the body
      Just _ -> do
        componentNav page
        mkPage gs page

webMain :: Port -> IORef GameState -> IO ()
webMain port gsRef = do
  putStrLn $ "Web interface listening on :" <> show port
  dataDir <- getDataDir
  Network.Wai.Handler.Warp.run port (app (dataDir <> "/assets"))
 where
  app :: FilePath -> Network.Wai.Application
  app = Servant.serve (Proxy @SwarmApi) . mkApp gsRef
