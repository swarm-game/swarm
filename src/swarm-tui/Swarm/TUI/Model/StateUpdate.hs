{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.StateUpdate (
  initAppState,
  initPersistentState,
  constructAppState,
  initAppStateForScenario,
  classicGame0,
  startGame,
  startGameWithSeed,
  restartGame,
  attainAchievement,
  attainAchievement',
  scenarioToAppState,

  -- ** Keybindings
  initKeyHandlingState,
  KeybindingPrint (..),
  showKeybindings,
) where

import Brick.AttrMap (applyAttrMappings)
import Brick.Focus
import Brick.Keybindings as BK
import Brick.Widgets.List qualified as BL
import Control.Applicative ((<|>))
import Control.Carrier.Accum.FixedStrict (runAccum)
import Control.Carrier.Lift (runM)
import Control.Carrier.Throw.Either (runThrow)
import Control.Effect.Accum
import Control.Effect.Lift
import Control.Effect.Throw
import Control.Lens hiding (from, (<.>))
import Control.Monad (guard, void)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState, execStateT)
import Data.Bifunctor (first)
import Data.Foldable qualified as F
import Data.List qualified as List
import Data.List.Extra (enumerate)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getZonedTime)
import Swarm.Game.Failure (Asset (..), LoadingFailure (..), SystemFailure (..))
import Swarm.Game.Land
import Swarm.Game.ResourceLoading (getSwarmConfigIniFile)
import Swarm.Game.Scenario (
  ScenarioInputs (..),
  gsiScenarioInputs,
  loadScenario,
  scenarioAttrs,
  scenarioLandscape,
  scenarioOperation,
  scenarioSolution,
  scenarioWorlds,
 )
import Swarm.Game.Scenario.Scoring.Best
import Swarm.Game.Scenario.Scoring.ConcreteMetrics
import Swarm.Game.Scenario.Scoring.GenericMetrics
import Swarm.Game.Scenario.Status
import Swarm.Game.Scenario.Topography.Structure.Recognition (automatons)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Type (originalStructureDefinitions)
import Swarm.Game.ScenarioInfo (
  loadScenarioInfo,
  normalizeScenarioPath,
  scenarioItemByPath,
  _SISingle,
 )
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Runtime
import Swarm.Game.State.Substate
import Swarm.Game.World.Gen (Seed)
import Swarm.Language.Pretty (prettyText)
import Swarm.Log (LogSource (SystemLog), Severity (..))
import Swarm.TUI.Controller.MainEventHandler (mainEventHandlers)
import Swarm.TUI.Controller.REPLEventHandler (replEventHandlers)
import Swarm.TUI.Editor.Model qualified as EM
import Swarm.TUI.Editor.Util qualified as EU
import Swarm.TUI.Inventory.Sorting
import Swarm.TUI.Launch.Model (toSerializableParams)
import Swarm.TUI.Model
import Swarm.TUI.Model.Achievements
import Swarm.TUI.Model.Event (SwarmEvent, defaultSwarmBindings, swarmEvents)
import Swarm.TUI.Model.Goal (emptyGoalDisplay)
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.Repl
import Swarm.TUI.Model.Structure
import Swarm.TUI.Model.UI
import Swarm.TUI.View.Attribute.Attr (getWorldAttrName, swarmAttrMap)
import Swarm.TUI.View.Attribute.CustomStyling (toAttrPair)
import Swarm.TUI.View.Structure qualified as SR
import Swarm.Util.Effect (asExceptT, warn, withThrow)
import System.Clock

createEventHandlers ::
  (Has (Throw SystemFailure) sig m) =>
  KeyConfig SwarmEvent ->
  m EventHandlers
createEventHandlers config = do
  mainHandler <- buildDispatcher mainEventHandlers
  replHandler <- buildDispatcher replEventHandlers
  return EventHandlers {..}
 where
  -- this error handling code is modified version of the brick demo app:
  -- https://github.com/jtdaugherty/brick/blob/764e66897/programs/CustomKeybindingDemo.hs#L216
  buildDispatcher handlers = case keyDispatcher config handlers of
    Right d -> return d
    Left collisions -> do
      let errorHeader = "Error: some key events have the same keys bound to them.\n"
      let handlerErrors = flip map collisions $ \(b, hs) ->
            let hsm = "Handlers with the '" <> BK.ppBinding b <> "' binding:"
                hss = flip map hs $ \h ->
                  let trigger = case BK.kehEventTrigger $ BK.khHandler h of
                        ByKey k -> "triggered by the key '" <> BK.ppBinding k <> "'"
                        ByEvent e -> "triggered by the event '" <> fromMaybe "<unknown>" (BK.keyEventName swarmEvents e) <> "'"
                      desc = BK.handlerDescription $ BK.kehHandler $ BK.khHandler h
                   in "  " <> desc <> " (" <> trigger <> ")"
             in T.intercalate "\n" (hsm : hss)
      throwError $ CustomFailure (T.intercalate "\n" $ errorHeader : handlerErrors)

loadKeybindingConfig ::
  (Has (Accum (Seq SystemFailure)) sig m, Has (Lift IO) sig m) =>
  m [(SwarmEvent, BindingState)]
loadKeybindingConfig = do
  (iniExists, ini) <- sendIO getSwarmConfigIniFile
  if not iniExists
    then return []
    else do
      loadedCustomBindings <- sendIO $ keybindingsFromFile swarmEvents "keybindings" ini
      case loadedCustomBindings of
        Left e -> do
          warn $ AssetNotLoaded Keybindings ini (CustomMessage $ T.pack e)
          return []
        Right bs -> pure $ fromMaybe [] bs

initKeyHandlingState ::
  (Has (Throw SystemFailure) sig m, Has (Accum (Seq SystemFailure)) sig m, Has (Lift IO) sig m) =>
  m KeyEventHandlingState
initKeyHandlingState = do
  customBindings <- loadKeybindingConfig
  let cfg = newKeyConfig swarmEvents defaultSwarmBindings customBindings
  handlers <- createEventHandlers cfg
  return $ KeyEventHandlingState cfg handlers

data KeybindingPrint = MarkdownPrint | TextPrint

showKeybindings :: KeybindingPrint -> IO Text
showKeybindings kPrint = do
  bindings <-
    runM
      . runThrow @SystemFailure
      . runAccum (mempty :: Seq SystemFailure)
      $ initKeyHandlingState
  pure $ case bindings of
    Left e -> prettyText e
    Right (w, bs) ->
      showTable kPrint (bs ^. keyConfig) sections
        <> "\n"
        <> T.unlines (map prettyText $ F.toList w)
 where
  showTable = \case
    MarkdownPrint -> keybindingMarkdownTable
    TextPrint -> keybindingTextTable
  sections =
    [ ("main", mainEventHandlers)
    , ("repl", replEventHandlers)
    ]

-- | Initialize the 'AppState' from scratch.
initAppState ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  AppOpts ->
  m AppState
initAppState opts = do
  (rs, ui, keyHandling) <- initPersistentState opts
  constructAppState rs ui keyHandling opts

-- | Add some system failures to the list of messages in the
--   'RuntimeState'.
addWarnings :: RuntimeState -> [SystemFailure] -> RuntimeState
addWarnings = List.foldl' logWarning
 where
  logWarning rs' w = rs' & eventLog %~ logEvent SystemLog Error "UI Loading" (prettyText w)

-- | Based on the command line options, should we skip displaying the
--   menu?
skipMenu :: AppOpts -> Bool
skipMenu AppOpts {..} = isJust userScenario || isRunningInitialProgram || isJust userSeed
 where
  isRunningInitialProgram = isJust scriptToRun || autoPlay

-- | Initialize the more persistent parts of the app state, /i.e./ the
--   'RuntimeState' and 'UIState'.  This is split out into a separate
--   function so that in the integration test suite we can call this
--   once and reuse the resulting states for all tests.
initPersistentState ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  AppOpts ->
  m (RuntimeState, UIState, KeyEventHandlingState)
initPersistentState opts@(AppOpts {..}) = do
  (warnings :: Seq SystemFailure, (initRS, initUI, initKs)) <- runAccum mempty $ do
    rs <- initRuntimeState
    ui <- initUIState speed (not (skipMenu opts)) cheatMode
    ks <- initKeyHandlingState
    return (rs, ui, ks)
  let initRS' = addWarnings initRS (F.toList warnings)
  return (initRS', initUI, initKs)

-- | Construct an 'AppState' from an already-loaded 'RuntimeState' and
--   'UIState', given the 'AppOpts' the app was started with.
constructAppState ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  RuntimeState ->
  UIState ->
  KeyEventHandlingState ->
  AppOpts ->
  m AppState
constructAppState rs ui key opts@(AppOpts {..}) = do
  let gs = initGameState $ rs ^. stdGameConfigInputs
  case skipMenu opts of
    False -> return $ AppState gs (ui & uiGameplay . uiTiming . lgTicksPerSecond .~ defaultInitLgTicksPerSecond) key rs
    True -> do
      let tem = gs ^. landscape . terrainAndEntities
      (scenario, path) <-
        loadScenario
          (fromMaybe "classic" userScenario)
          (ScenarioInputs (initWorldMap . gsiScenarioInputs . initState $ rs ^. stdGameConfigInputs) tem)
      maybeRunScript <- traverse parseCodeFile scriptToRun

      let maybeAutoplay = do
            guard autoPlay
            soln <- scenario ^. scenarioOperation . scenarioSolution
            return $ CodeToRun ScenarioSuggested soln
          codeToRun = maybeAutoplay <|> maybeRunScript

      eitherSi <- sendIO . runM . runThrow $ loadScenarioInfo path
      let (si, newRs) = case eitherSi of
            Right x -> (x, rs)
            Left e -> (ScenarioInfo path NotStarted, addWarnings rs [e])
      sendIO $
        execStateT
          (startGameWithSeed (scenario, si) $ LaunchParams (pure userSeed) (pure codeToRun))
          (AppState gs ui key newRs)

-- | Load a 'Scenario' and start playing the game.
startGame :: (MonadIO m, MonadState AppState m) => ScenarioInfoPair -> Maybe CodeToRun -> m ()
startGame siPair = startGameWithSeed siPair . LaunchParams (pure Nothing) . pure

-- | Re-initialize the game from the stored reference to the current scenario.
--
-- Note that "restarting" is intended only for "scenarios";
-- with some scenarios, it may be possible to get stuck so that it is
-- either impossible or very annoying to win, so being offered an
-- option to restart is more user-friendly.
--
-- Since scenarios are stored as a Maybe in the UI state, we handle the Nothing
-- case upstream so that the Scenario passed to this function definitely exists.
restartGame :: (MonadIO m, MonadState AppState m) => Seed -> ScenarioInfoPair -> m ()
restartGame currentSeed siPair = startGameWithSeed siPair $ LaunchParams (pure (Just currentSeed)) (pure Nothing)

-- | Load a 'Scenario' and start playing the game, with the
--   possibility for the user to override the seed.
startGameWithSeed ::
  (MonadIO m, MonadState AppState m) =>
  ScenarioInfoPair ->
  ValidatedLaunchParams ->
  m ()
startGameWithSeed siPair@(_scene, si) lp = do
  t <- liftIO getZonedTime
  ss <- use $ runtimeState . scenarios
  p <- liftIO $ normalizeScenarioPath ss (si ^. scenarioPath)
  runtimeState
    . scenarios
    . scenarioItemByPath p
    . _SISingle
    . _2
    . scenarioStatus
    .= Played
      (toSerializableParams lp)
      (Metric Attempted $ ProgressStats t emptyAttemptMetric)
      (prevBest t)
  scenarioToAppState siPair lp
  -- Beware: currentScenarioPath must be set so that progress/achievements can be saved.
  -- It has just been cleared in scenarioToAppState.
  gameState . currentScenarioPath .= Just p
 where
  prevBest t = case si ^. scenarioStatus of
    NotStarted -> emptyBest t
    Played _ _ b -> b

-- | Modify the 'AppState' appropriately when starting a new scenario.
scenarioToAppState ::
  (MonadIO m, MonadState AppState m) =>
  ScenarioInfoPair ->
  ValidatedLaunchParams ->
  m ()
scenarioToAppState siPair@(scene, _) lp = do
  rs <- use runtimeState
  gs <- liftIO $ scenarioToGameState scene lp $ rs ^. stdGameConfigInputs
  gameState .= gs
  void $ withLensIO uiState $ scenarioToUIState isAutoplaying siPair gs
 where
  isAutoplaying = case runIdentity (initialCode lp) of
    Just (CodeToRun ScenarioSuggested _) -> True
    _ -> False

  withLensIO :: (MonadIO m, MonadState AppState m) => Lens' AppState x -> (x -> IO x) -> m x
  withLensIO l a = do
    x <- use l
    x' <- liftIO $ a x
    l .= x'
    return x'

-- | Modify the UI state appropriately when starting a new scenario.
scenarioToUIState ::
  Bool ->
  ScenarioInfoPair ->
  GameState ->
  UIState ->
  IO UIState
scenarioToUIState isAutoplaying siPair@(scenario, _) gs u = do
  curTime <- getTime Monotonic
  return $
    u
      & uiPlaying .~ True
      & uiCheatMode ||~ isAutoplaying
      & uiAttrMap
        .~ applyAttrMappings
          ( map (first getWorldAttrName . toAttrPair) $
              fst siPair ^. scenarioLandscape . scenarioAttrs
          )
          swarmAttrMap
      & uiGameplay . uiGoal .~ emptyGoalDisplay
      & uiGameplay . uiHideGoals .~ (isAutoplaying && not (u ^. uiCheatMode))
      & uiGameplay . uiFocusRing .~ initFocusRing
      & uiGameplay . uiInventory . uiInventorySearch .~ Nothing
      & uiGameplay . uiInventory . uiInventoryList .~ Nothing
      & uiGameplay . uiInventory . uiInventorySort .~ defaultSortOptions
      & uiGameplay . uiInventory . uiShowZero .~ True
      & uiGameplay . uiTiming . uiShowFPS .~ False
      & uiGameplay . uiREPL .~ initREPLState (u ^. uiGameplay . uiREPL . replHistory)
      & uiGameplay . uiREPL . replHistory %~ restartREPLHistory
      & uiGameplay . scenarioRef ?~ siPair
      & uiGameplay . uiTiming . lastFrameTime .~ curTime
      & uiGameplay . uiWorldEditor . EM.entityPaintList %~ BL.listReplace entityList Nothing
      & uiGameplay . uiWorldEditor . EM.editingBounds . EM.boundsRect %~ setNewBounds
      & uiGameplay . uiStructure
        .~ StructureDisplay
          (SR.makeListWidget . M.elems $ gs ^. discovery . structureRecognition . automatons . originalStructureDefinitions)
          (focusSetCurrent (StructureWidgets StructuresList) $ focusRing $ map StructureWidgets enumerate)
 where
  entityList = EU.getEntitiesForList $ gs ^. landscape . terrainAndEntities . entityMap

  (isEmptyArea, newBounds) =
    EU.getEditingBounds $
      NE.head $
        scenario ^. scenarioLandscape . scenarioWorlds

  setNewBounds maybeOldBounds =
    if isEmptyArea
      then maybeOldBounds
      else Just newBounds

-- | Create an initial app state for a specific scenario.  Note that
--   this function is used only for unit tests, integration tests, and
--   benchmarks.
--
--   In normal play, an 'AppState' already exists and we simply need
--   to update it using 'scenarioToAppState'.
initAppStateForScenario :: String -> Maybe Seed -> Maybe FilePath -> ExceptT Text IO AppState
initAppStateForScenario sceneName userSeed toRun =
  asExceptT . withThrow (prettyText @SystemFailure) . initAppState $
    defaultAppOpts
      { userScenario = Just sceneName
      , userSeed = userSeed
      , scriptToRun = toRun
      }

-- | For convenience, the 'AppState' corresponding to the classic game
--   with seed 0.  This is used only for benchmarks and unit tests.
classicGame0 :: ExceptT Text IO AppState
classicGame0 = initAppStateForScenario "classic" (Just 0) Nothing
