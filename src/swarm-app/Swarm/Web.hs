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
import Control.Exception (Exception (displayException), IOException, catch, throwIO)
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.IORef (IORef, readIORef)
import Data.IntMap qualified as IM
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Network.Wai qualified
import Network.Wai.Handler.Warp qualified as Warp
import Servant
import Swarm.Game.Robot
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Graph
import Swarm.TUI.Model.Goal
import Swarm.Game.Scenario.Objective.WinCheck
import Swarm.Game.State
import Swarm.TUI.Model
import Swarm.TUI.Model.UI
import System.Timeout (timeout)

type SwarmApi =
  "robots" :> Get '[JSON] [Robot]
    :<|> "robot" :> Capture "id" Int :> Get '[JSON] (Maybe Robot)
    :<|> "goals" :> "prereqs" :> Get '[JSON] [PrereqSatisfaction]
    :<|> "goals" :> "active" :> Get '[JSON] [Objective]
    :<|> "goals" :> "graph" :> Get '[JSON] (Maybe GraphInfo)
    :<|> "goals" :> "uigoal" :> Get '[JSON] GoalTracking
    :<|> "goals" :> Get '[JSON] WinCondition
    :<|> "repl" :> "history" :> "full" :> Get '[JSON] [T.Text]

mkApp :: IORef AppState -> Servant.Server SwarmApi
mkApp appStateRef =
  robotsHandler
    :<|> robotHandler
    :<|> prereqsHandler
    :<|> activeGoalsHandler
    :<|> goalsGraphHandler
    :<|> uiGoalHandler
    :<|> goalsHandler
    :<|> replHandler
 where
  robotsHandler = do
    appState <- liftIO (readIORef appStateRef)
    pure $ IM.elems $ appState ^. gameState . robotMap
  robotHandler rid = do
    appState <- liftIO (readIORef appStateRef)
    pure $ IM.lookup rid (appState ^. gameState . robotMap)
  prereqsHandler = do
    appState <- liftIO (readIORef appStateRef)
    case appState ^. gameState . winCondition of
      WinConditions _winState oc -> return $ getSatisfaction oc
      _ -> return []
  activeGoalsHandler = do
    appState <- liftIO (readIORef appStateRef)
    case appState ^. gameState . winCondition of
      WinConditions _winState oc -> return $ getActiveObjectives oc
      _ -> return []
  goalsGraphHandler = do
    appState <- liftIO (readIORef appStateRef)
    return $ case appState ^. gameState . winCondition of
      WinConditions _winState oc -> Just $ makeGraphInfo oc
      _ -> Nothing
  uiGoalHandler = do
    appState <- liftIO (readIORef appStateRef)
    return $ appState ^. uiState . uiGoal . goalsContent
  goalsHandler = do
    appState <- liftIO (readIORef appStateRef)
    return $ appState ^. gameState . winCondition
  replHandler = do
    appState <- liftIO (readIORef appStateRef)
    let replHistorySeq = appState ^. uiState . uiREPL . replHistory . replSeq
        items = [x | REPLEntry x <- toList replHistorySeq]
    pure items

webMain :: Maybe (MVar (Either String ())) -> Warp.Port -> IORef AppState -> IO ()
webMain baton port appStateRef = catch (Warp.runSettings settings app) handleErr
 where
  settings = Warp.setPort port $ onReady Warp.defaultSettings
  onReady = case baton of
    Just mv -> Warp.setBeforeMainLoop $ putMVar mv (Right ())
    Nothing -> id
  app :: Network.Wai.Application
  app = Servant.serve (Proxy @SwarmApi) (mkApp appStateRef)
  handleErr :: IOException -> IO ()
  handleErr e = case baton of
    Just mv -> putMVar mv (Left $ displayException e)
    Nothing -> throwIO e

defaultPort :: Warp.Port
defaultPort = 5357

-- | Attempt to start a web thread on the requested port, or a default
--   one if none is requested (or don't start a web thread if the
--   requested port is 0).  If an explicit port was requested, fail if
--   startup doesn't work.  Otherwise, ignore the failure.  In any
--   case, return a @Maybe Port@ value representing whether a web
--   server is actually running, and if so, what port it is on.
startWebThread :: Maybe Warp.Port -> IORef AppState -> IO (Either String Warp.Port)
-- User explicitly provided port '0': don't run the web server
startWebThread (Just 0) _ = pure $ Left "The web port has been turned off."
startWebThread portM appStateRef = do
  baton <- newEmptyMVar
  let port = fromMaybe defaultPort portM
  void $ forkIO $ webMain (Just baton) port appStateRef
  res <- timeout 500_000 (takeMVar baton)
  case (portM, res) of
    -- User requested explicit port but server didn't start: fail
    (Just _, Nothing) -> fail $ failMsg port
    -- If we are using the default port, we just report the timeout
    (Nothing, Nothing) -> return . Left $ failMsg port <> " (timeout)"
    (_, Just (Left e)) -> return . Left $ failMsg port <> " - " <> e
    -- If all works, we report on what port the web server is running
    (_, Just _) -> return (Right port)
 where
  failMsg p = "Failed to start the web API on :" <> show p
