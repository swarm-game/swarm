{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
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

import Brick.BChan
import CMarkGFM qualified as CMark (commonmarkToHtml)
import Control.Arrow (left)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (Exception (displayException), IOException, catch, throwIO)
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (toList)
import Data.IntMap qualified as IM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Tree (Tree (Node), drawTree)
import Network.HTTP.Types (ok200)
import Network.Wai (responseLBS)
import Network.Wai qualified
import Network.Wai.Handler.Warp qualified as Warp
import Servant
import Servant.Docs (ToCapture)
import Servant.Docs qualified as SD
import Swarm.Game.Robot
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Graph
import Swarm.Game.Scenario.Objective.WinCheck
import Swarm.Game.State
import Swarm.Language.Module
import Swarm.Language.Pipeline
import Swarm.Language.Pretty (prettyString)
import Swarm.Language.Syntax
import Swarm.ReadableIORef
import Swarm.TUI.Model
import Swarm.TUI.Model.Goal
import Swarm.TUI.Model.UI
import System.Timeout (timeout)
import Text.Read (readEither)
import Witch (into)

newtype RobotID = RobotID Int

instance FromHttpApiData RobotID where
  parseUrlPiece = fmap RobotID . left T.pack . readEither . T.unpack

instance SD.ToSample T.Text where
  toSamples _ = SD.noSamples

type SwarmAPI =
  "robots" :> Get '[JSON] [Robot]
    :<|> "robot" :> Capture "id" RobotID :> Get '[JSON] (Maybe Robot)
    :<|> "goals" :> "prereqs" :> Get '[JSON] [PrereqSatisfaction]
    :<|> "goals" :> "active" :> Get '[JSON] [Objective]
    :<|> "goals" :> "graph" :> Get '[JSON] (Maybe GraphInfo)
    :<|> "goals" :> "uigoal" :> Get '[JSON] GoalTracking
    :<|> "goals" :> Get '[JSON] WinCondition
    :<|> "code" :> "render" :> ReqBody '[PlainText] T.Text :> Post '[PlainText] T.Text
    :<|> "code" :> "run" :> ReqBody '[PlainText] T.Text :> Post '[PlainText] T.Text
    :<|> "repl" :> "history" :> "full" :> Get '[JSON] [REPLHistItem]

instance ToCapture (Capture "id" RobotID) where
  toCapture _ =
    SD.DocCapture
      "id" -- name
      "(integer) robot ID" -- description

swarmApi :: Proxy SwarmAPI
swarmApi = Proxy

type ToplevelAPI = SwarmAPI :<|> Raw

api :: Proxy ToplevelAPI
api = Proxy

docsBS :: ByteString
docsBS =
  encodeUtf8
    . L.fromStrict
    . CMark.commonmarkToHtml [] []
    . T.pack
    . SD.markdownWith
      ( SD.defRenderingOptions
          & SD.requestExamples .~ SD.FirstContentType
          & SD.responseExamples .~ SD.FirstContentType
      )
    $ SD.docsWithIntros [intro] swarmApi
 where
  intro = SD.DocIntro "Swarm Web API" ["All of the valid endpoints are documented below."]

mkApp ::
  ReadableIORef AppState ->
  -- | Writable
  BChan AppEvent ->
  Servant.Server SwarmAPI
mkApp appStateRef chan =
  robotsHandler
    :<|> robotHandler
    :<|> prereqsHandler
    :<|> activeGoalsHandler
    :<|> goalsGraphHandler
    :<|> uiGoalHandler
    :<|> goalsHandler
    :<|> codeRenderHandler
    :<|> codeRunHandler
    :<|> replHandler
 where
  robotsHandler = do
    appState <- liftIO (readIORef appStateRef)
    pure $ IM.elems $ appState ^. gameState . robotMap
  robotHandler (RobotID rid) = do
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
  codeRenderHandler contents = do
    return $ case processTermEither contents of
      Right (ProcessedTerm (Module stx@(Syntax' _srcLoc _term _) _) _ _) ->
        into @Text . drawTree . fmap prettyString . para Node $ stx
      Left x -> x
  codeRunHandler contents = do
    liftIO . writeBChan chan . Web $ RunWebCode contents
    return $ T.pack "Sent\n"
  replHandler = do
    appState <- liftIO (readIORef appStateRef)
    let replHistorySeq = appState ^. uiState . uiREPL . replHistory . replSeq
        items = toList replHistorySeq
    pure items

webMain ::
  Maybe (MVar (Either String ())) ->
  Warp.Port ->
  ReadableIORef AppState ->
  -- | Writable
  BChan AppEvent ->
  IO ()
webMain baton port appStateRef chan = catch (Warp.runSettings settings app) handleErr
 where
  settings = Warp.setPort port $ onReady Warp.defaultSettings
  onReady = case baton of
    Just mv -> Warp.setBeforeMainLoop $ putMVar mv (Right ())
    Nothing -> id

  server :: Server ToplevelAPI
  server = mkApp appStateRef chan :<|> Tagged serveDocs
   where
    serveDocs _ resp =
      resp $ responseLBS ok200 [plain] docsBS
    plain = ("Content-Type", "text/html")

  app :: Network.Wai.Application
  app = Servant.serve api server

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
startWebThread ::
  Maybe Warp.Port ->
  -- | Read-only reference to the application state.
  ReadableIORef AppState ->
  -- | Writable channel to send events to the game
  BChan AppEvent ->
  IO (Either String Warp.Port)
-- User explicitly provided port '0': don't run the web server
startWebThread (Just 0) _ _ = pure $ Left "The web port has been turned off."
startWebThread portM appStateRef chan = do
  baton <- newEmptyMVar
  let port = fromMaybe defaultPort portM
  void $ forkIO $ webMain (Just baton) port appStateRef chan
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
