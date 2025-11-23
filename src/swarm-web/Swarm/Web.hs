{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- A web service for Swarm.
--
-- The service can be started using the `--port 5357` command line argument,
-- or through the REPL by calling `Swarm.App.demoWeb`.
--
-- See 'SwarmAPI' for the available endpoints. You can also see them in your
-- browser on the top level endpoint:
-- @lynx localhost:5357 -dump@
-- or you can output the markdown documentation to your terminal:
-- @cabal run swarm -O0 -- generate endpoints@
--
-- Missing endpoints:
--
--   * TODO: #493 export the whole game state
module Swarm.Web (
  startWebThread,
  defaultPort,

  -- ** Docs
  SwarmAPI,
  swarmApiHtml,
  swarmApiMarkdown,

  -- ** Development
  webMain,
) where

import Commonmark qualified as Mark (commonmark, renderHtml)
import Control.Arrow (left)
import Control.Carrier.Error.Either (runError)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (Exception (displayException), IOException, catch, throwIO)
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (toList)
import Data.IntMap qualified as IM
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Map.NonEmpty qualified as NEM
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Tree (Tree (Node), drawTree)
import Network.HTTP.Types (ok200)
import Network.Wai (responseLBS)
import Network.Wai qualified
import Network.Wai.Application.Static (defaultFileServerSettings, ssIndices)
import Network.Wai.Handler.Warp qualified as Warp
import Servant
import Servant.Docs (ToCapture)
import Servant.Docs qualified as SD
import Servant.Docs.Internal qualified as SD (renderCurlBasePath)
import Servant.Types.SourceT qualified as S
import Swarm.Failure (SystemFailure)
import Swarm.Game.Entity (EntityName, entityName)
import Swarm.Game.Location (Location)
import Swarm.Game.Robot
import Swarm.Game.Robot.Concrete (robotLog)
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Graph
import Swarm.Game.Scenario.Objective.WinCheck
import Swarm.Game.Scenario.Status
import Swarm.Game.Scenario.Topography.Area (AreaDimensions (..))
import Swarm.Game.Scenario.Topography.Navigation.Portal
import Swarm.Game.Scenario.Topography.Structure.Recognition
import Swarm.Game.Scenario.Topography.Structure.Recognition.Log
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Runtime (eventLog)
import Swarm.Game.State.Substate
import Swarm.Game.Step.Path.Type
import Swarm.Game.Universe (SubworldName)
import Swarm.Language.Load (SyntaxWithImports (..))
import Swarm.Language.Pipeline (processSource, requireNonEmptyTerm)
import Swarm.Language.Syntax (Phase (Elaborated, Instantiated))
import Swarm.Log (LogEntry)
import Swarm.Pretty (prettyText, prettyTextLine)
import Swarm.TUI.Model hiding (SwarmKeyDispatchers (..))
import Swarm.TUI.Model.Dialog.Goal
import Swarm.TUI.Model.Repl (REPLHistItem, getSessionREPLHistoryItems, replHistory, replSeq)
import Swarm.TUI.Model.UI.Gameplay
import Swarm.Util (applyJust)
import Swarm.Util.RingBuffer
import Swarm.Web.GraphRender
import Swarm.Web.Worldview
import System.Timeout (timeout)
import Text.Read (readEither)
import WaiAppStatic.Types (unsafeToPiece)
import Witch (into)

-- ------------------------------------------------------------------
-- Docs
-- ------------------------------------------------------------------

newtype RobotID = RobotID Int

type SwarmAPI =
  "robots" :> Get '[JSON] [Robot Instantiated]
    :<|> "robot" :> Capture "id" RobotID :> Get '[JSON] (Maybe (Robot Instantiated))
    :<|> "robot" :> Capture "id" RobotID :> "log" :> StreamGet NewlineFraming JSON (SourceIO LogEntry)
    :<|> "goals" :> "prereqs" :> Get '[JSON] [PrereqSatisfaction]
    :<|> "goals" :> "active" :> Get '[JSON] [Objective Elaborated]
    :<|> "goals" :> "graph" :> Get '[JSON] (Maybe (GraphInfo Elaborated))
    :<|> "goals" :> "render" :> Get '[PlainText] T.Text
    :<|> "goals" :> "uigoal" :> Get '[JSON] GoalTracking
    :<|> "goals" :> Get '[JSON] (WinCondition Elaborated)
    :<|> "navigation" :> Get '[JSON] (Navigation (M.Map SubworldName) Location)
    :<|> "recognize" :> "log" :> Get '[JSON] [SearchLog EntityName]
    :<|> "recognize" :> "found" :> Get '[JSON] [StructureLocation]
    :<|> "code" :> "render" :> ReqBody '[PlainText] T.Text :> Post '[PlainText] T.Text
    :<|> "code" :> "run" :> ReqBody '[PlainText] T.Text :> StreamGet NewlineFraming JSON (SourceIO WebInvocationState)
    :<|> "paths" :> "log" :> Get '[JSON] (RingBuffer CacheLogEntry)
    :<|> "repl" :> "history" :> "current" :> Get '[JSON] [REPLHistItem]
    :<|> "repl" :> "history" :> "full" :> Get '[JSON] [REPLHistItem]
    :<|> "map" :> Capture "size" AreaDimensions :> Get '[JSON] GridResponse
    :<|> "runtime" :> "log" :> StreamGet NewlineFraming JSON (SourceIO LogEntry)

swarmApi :: Proxy SwarmAPI
swarmApi = Proxy

type ToplevelAPI =
  SwarmAPI
    :<|> "api" :> Raw
    :<|> Raw

api :: Proxy ToplevelAPI
api = Proxy

swarmApiHtml :: ByteString
swarmApiHtml =
  encodeUtf8
    . either (TL.pack . show) (Mark.renderHtml @())
    . Mark.commonmark ""
    $ T.pack swarmApiMarkdown

swarmApiMarkdown :: String
swarmApiMarkdown =
  SD.markdownWith
    ( SD.defRenderingOptions
        & SD.requestExamples .~ SD.FirstContentType
        & SD.responseExamples .~ SD.FirstContentType
        & SD.renderCurlBasePath ?~ "http://localhost:" <> show defaultPort
    )
    $ SD.docsWithIntros [intro] swarmApi
 where
  intro = SD.DocIntro "Swarm Web API" ["All of the valid endpoints are documented below."]

-- ------------------------------------------------------------------
-- Handlers
-- ------------------------------------------------------------------

mkApp ::
  -- | Read-only access to the current AppState
  IO AppState ->
  -- | Writable channel to send events to the game
  EventChannel ->
  Servant.Server SwarmAPI
mkApp state events =
  robotsHandler state
    :<|> robotHandler state
    :<|> robotLogHandler state
    :<|> prereqsHandler state
    :<|> activeGoalsHandler state
    :<|> goalsGraphHandler state
    :<|> goalsRenderHandler state
    :<|> uiGoalHandler state
    :<|> goalsHandler state
    :<|> navigationHandler state
    :<|> recogLogHandler state
    :<|> recogFoundHandler state
    :<|> codeRenderHandler
    :<|> codeRunHandler events
    :<|> pathsLogHandler state
    :<|> replHistHandler Current state
    :<|> replHistHandler Full state
    :<|> mapViewHandler state
    :<|> runtimeLogHandler state

robotsHandler :: IO AppState -> Handler [Robot Instantiated]
robotsHandler appStateRef = do
  appState <- liftIO appStateRef
  pure $ IM.elems $ appState ^. playState . scenarioState . gameState . robotInfo . robotMap

robotHandler :: IO AppState -> RobotID -> Handler (Maybe (Robot Instantiated))
robotHandler appStateRef (RobotID rid) = do
  appState <- liftIO appStateRef
  pure $ IM.lookup rid (appState ^. playState . scenarioState . gameState . robotInfo . robotMap)

prereqsHandler :: IO AppState -> Handler [PrereqSatisfaction]
prereqsHandler appStateRef = do
  appState <- liftIO appStateRef
  case appState ^. playState . scenarioState . gameState . winCondition of
    WinConditions _winState oc -> return $ getSatisfaction oc
    _ -> return []

activeGoalsHandler :: IO AppState -> Handler [Objective Elaborated]
activeGoalsHandler appStateRef = do
  appState <- liftIO appStateRef
  case appState ^. playState . scenarioState . gameState . winCondition of
    WinConditions _winState oc -> return $ getActiveObjectives oc
    _ -> return []

goalsGraphHandler :: IO AppState -> Handler (Maybe (GraphInfo Elaborated))
goalsGraphHandler appStateRef = do
  appState <- liftIO appStateRef
  return $ case appState ^. playState . scenarioState . gameState . winCondition of
    WinConditions _winState oc -> Just $ makeGraphInfo oc
    _ -> Nothing

goalsRenderHandler :: IO AppState -> Handler T.Text
goalsRenderHandler appStateRef = do
  appState <- liftIO appStateRef
  return $ case appState ^. playState . scenarioState . gameState . winCondition of
    WinConditions _winState oc -> T.pack $ renderGoalsGraph oc
    _ -> mempty

uiGoalHandler :: IO AppState -> Handler GoalTracking
uiGoalHandler appStateRef = do
  appState <- liftIO appStateRef
  return $ appState ^. playState . scenarioState . uiGameplay . uiDialogs . uiGoal . goalsContent

goalsHandler :: IO AppState -> Handler (WinCondition Elaborated)
goalsHandler appStateRef = do
  appState <- liftIO appStateRef
  return $ appState ^. playState . scenarioState . gameState . winCondition

deriving instance ToJSON (Navigation (M.Map SubworldName) Location)

instance SD.ToSample (Navigation (M.Map SubworldName) Location) where
  toSamples _ = SD.noSamples

navigationHandler :: IO AppState -> Handler (Navigation (M.Map SubworldName) Location)
navigationHandler appStateRef = do
  appState <- liftIO appStateRef
  return $ appState ^. playState . scenarioState . gameState . landscape . worldNavigation

recogLogHandler :: IO AppState -> Handler [SearchLog EntityName]
recogLogHandler appStateRef = do
  appState <- liftIO appStateRef
  return $
    map (fmap (view entityName)) $
      appState ^. playState . scenarioState . gameState . discovery . structureRecognition . recognitionLog

recogFoundHandler :: IO AppState -> Handler [StructureLocation]
recogFoundHandler appStateRef = do
  appState <- liftIO appStateRef
  let registry = appState ^. playState . scenarioState . gameState . discovery . structureRecognition . foundStructures
  return
    . map (uncurry StructureLocation)
    . concatMap (\(x, ys) -> map (x,) $ NE.toList ys)
    . M.toList
    . M.map NEM.keys
    $ foundByName registry

codeRenderHandler :: Text -> Handler Text
codeRenderHandler contents = do
  res <- liftIO . runError @SystemFailure $ requireNonEmptyTerm =<< processSource Nothing contents Nothing
  pure $ case res of
    Right (SyntaxWithImports _ _ t) ->
      into @Text . drawTree . fmap (T.unpack . prettyTextLine) . para Node $ t
    Left x -> prettyText x

{- Note [How to stream back responses as we get results]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Servant has a builtin simple streaming:
https://docs.servant.dev/en/stable/cookbook/basic-streaming/Streaming.html

What we need is to:
1. run IO with 'Effect'
2. send the result with 'Yield'
3. if we are done 'Stop'
4. otherwise continue recursively

With the endpoint type 'StreamGet NewlineFraming JSON', servant will send each
result as a JSON on a separate line. That is not a valid JSON document, but
it's commonly used because it works well with line-oriented tools.

This gives the user an immediate feedback (did the code parse) and is well
suited for streaming large collections of data like the logs while consuming
constant memory.
-}

runtimeLogHandler :: IO AppState -> Handler (S.SourceT IO LogEntry)
runtimeLogHandler appStateRef = logHandler $ runtimeLogsGetter <$> appStateRef
 where
  getRuntimeLogs :: AppState -> [LogEntry]
  getRuntimeLogs = view $ runtimeState . eventLog . notificationsContent
  runtimeLogsGetter :: AppState -> Maybe (Seq LogEntry)
  runtimeLogsGetter = Just . Seq.fromList . reverse . getRuntimeLogs

robotLogHandler :: IO AppState -> RobotID -> Handler (S.SourceT IO LogEntry)
robotLogHandler appStateRef (RobotID rid) = logHandler $ getRobotLogs <$> appStateRef
 where
  robotMapGetter :: Getter AppState (IM.IntMap (Robot Instantiated))
  robotMapGetter = playState . scenarioState . gameState . robotInfo . robotMap
  getRobotLogs :: AppState -> Maybe (Seq LogEntry)
  getRobotLogs = preview $ robotMapGetter . at rid . _Just . robotLog

logHandler :: IO (Maybe (Seq LogEntry)) -> Handler (S.SourceT IO LogEntry)
logHandler getLogs = pure . S.fromStepT $ go 0
 where
  -- See note [How to stream back responses as we get results]
  go :: Int -> S.StepT IO LogEntry
  go shownCount =
    S.Effect $
      liftIO getLogs >>= \case
        Nothing -> pure S.Stop
        Just logs ->
          let unshownLogs = Seq.drop shownCount logs
              next :: S.StepT IO LogEntry
              next = S.Effect $ do
                threadDelay 1_000_000 -- check each second
                pure $ go (Seq.length logs)
           in pure $ foldr S.Yield next unshownLogs

codeRunHandler :: EventChannel -> Text -> Handler (S.SourceT IO WebInvocationState)
codeRunHandler chan contents = do
  replyVar <- liftIO newEmptyMVar
  let putReplyForce r = do
        void $ tryTakeMVar replyVar
        putMVar replyVar r
  liftIO . chan . Web $ RunWebCode contents putReplyForce
  -- See note [How to stream back responses as we get results]
  let waitForReply = S.Effect $ do
        reply <- takeMVar replyVar
        return . S.Yield reply $ case reply of
          InProgress -> waitForReply
          _ -> S.Stop
  return $ S.fromStepT waitForReply

pathsLogHandler :: IO AppState -> Handler (RingBuffer CacheLogEntry)
pathsLogHandler appStateRef = do
  appState <- liftIO appStateRef
  pure $ appState ^. playState . scenarioState . gameState . pathCaching . pathCachingLog

data ReplHistoryType = Current | Full

replHistHandler :: ReplHistoryType -> IO AppState -> Handler [REPLHistItem]
replHistHandler historyType appStateRef = do
  appState <- liftIO appStateRef
  let replHist = appState ^. playState . scenarioState . uiGameplay . uiREPL . replHistory
  pure . toList $ case historyType of
    Full -> replHist ^. replSeq
    Current -> getSessionREPLHistoryItems replHist

mapViewHandler :: IO AppState -> AreaDimensions -> Handler GridResponse
mapViewHandler appStateRef areaSize = do
  appState <- liftIO appStateRef
  let maybeScenario = appState ^? playState . scenarioState . uiGameplay . scenarioRef . _Just . getScenario
  pure $ case maybeScenario of
    Just s ->
      GridResponse True
        . Just
        . getCellGrid s (appState ^. playState . scenarioState . gameState)
        $ areaSize
    Nothing -> GridResponse False Nothing

-- ------------------------------------------------------------------
-- Main app (used by service and for development)
-- ------------------------------------------------------------------

-- | Simple result type to report errors from forked startup thread.
data WebStartResult = WebStarted | WebStartError String

type EventChannel = AppEvent -> IO ()

webMain ::
  Maybe (MVar WebStartResult) ->
  Warp.Port ->
  -- | Read-only reference to the application state.
  IO AppState ->
  -- | Writable channel to send events to the game
  EventChannel ->
  IO ()
webMain baton port appStateRef chan = catch (Warp.runSettings settings app) handleErr
 where
  settings = Warp.setPort port $ onReady Warp.defaultSettings
  onReady = applyJust $ Warp.setBeforeMainLoop . flip putMVar WebStarted <$> baton

  server :: Server ToplevelAPI
  server =
    mkApp appStateRef chan
      :<|> Tagged serveDocs
      :<|> serveDirectoryWith (defaultFileServerSettings "web") {ssIndices = [unsafeToPiece "index.html"]}
   where
    serveDocs _ resp =
      resp $ responseLBS ok200 [plain] swarmApiHtml
    plain = ("Content-Type", "text/html")

  app :: Network.Wai.Application
  app = Servant.serve api server

  handleErr :: IOException -> IO ()
  handleErr e = case baton of
    Just mv -> putMVar mv (WebStartError $ displayException e)
    Nothing -> throwIO e

-- ------------------------------------------------------------------
-- Web service
-- ------------------------------------------------------------------

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
  IO AppState ->
  -- | Writable channel to send events to the game
  EventChannel ->
  IO (Either String Warp.Port)
-- User explicitly provided port '0': don't run the web server
startWebThread (Just 0) _ _ = pure $ Left "The web port has been turned off."
startWebThread userPort appStateRef chan = do
  baton <- newEmptyMVar
  let port = fromMaybe defaultPort userPort
      failMsg = "Failed to start the web API on :" <> show port
  void $ forkIO $ webMain (Just baton) port appStateRef chan
  res <- timeout 500_000 (takeMVar baton)
  case res of
    Just WebStarted -> return (Right port)
    Just (WebStartError e) -> return . Left $ failMsg <> " - " <> e
    -- If user explicitly specified port exit, otherwise just report timeout
    Nothing -> case userPort of
      Just _p -> fail failMsg
      Nothing -> return . Left $ failMsg <> " (timeout)"

-- ------------------------------------------------------------------
-- Necessary instances
-- ------------------------------------------------------------------

instance SD.ToSample T.Text where
  toSamples _ = SD.noSamples

instance FromHttpApiData RobotID where
  parseUrlPiece = fmap RobotID . left T.pack . readEither . T.unpack

instance SD.ToSample RobotID where
  toSamples _ = SD.samples [RobotID 0, RobotID 1]

instance ToCapture (Capture "id" RobotID) where
  toCapture _ =
    SD.DocCapture
      "id" -- name
      "(integer) robot ID" -- description

instance FromHttpApiData AreaDimensions where
  parseUrlPiece x = left T.pack $ do
    pieces <- mapM (readEither . T.unpack) $ T.splitOn "x" x
    case pieces of
      [w, h] -> return $ AreaDimensions w h
      _ -> Left "Need two dimensions"

instance SD.ToSample AreaDimensions where
  toSamples _ = SD.samples [AreaDimensions 20 30]

instance ToCapture (Capture "size" AreaDimensions) where
  toCapture _ =
    SD.DocCapture
      "size" -- name
      "(integer, integer) dimensions of area" -- description
