{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.Editor.Controller where

import Brick hiding (Direction (..), Location (..))
import Brick qualified as B
import Brick.Focus
import Brick.Widgets.List qualified as BL
import Control.Lens
import Control.Monad (guard, when)
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson.KeyMap qualified as KM
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Tuple (swap)
import Data.Yaml qualified as Y
import Graphics.Vty qualified as V
import Swarm.Game.Display (Display, defaultChar)
import Swarm.Game.Entity (entitiesByName)
import Swarm.Game.Location
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Cell
import Swarm.Game.Scenario.EntityFacade
import Swarm.Game.Scenario.WorldDescription
import Swarm.Game.State
import Swarm.Game.Terrain (TerrainType (BlankT), getTerrainDefaultPaletteChar)
import Swarm.Game.World qualified as W
import Swarm.TUI.Controller.Util
import Swarm.TUI.Editor.Area (AreaDimensions (..), getAreaDimensions)
import Swarm.TUI.Editor.Json (SkeletonScenario (SkeletonScenario))
import Swarm.TUI.Editor.Model
import Swarm.TUI.Editor.Util qualified as EU
import Swarm.TUI.Model
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI
import Swarm.Util (binTuples, histogram)
import Swarm.Util qualified as U
import System.Clock

------------------------------------------------------------
-- World Editor panel events
------------------------------------------------------------

activateWorldEditorFunction :: WorldEditorFocusable -> EventM Name AppState ()
activateWorldEditorFunction BrushSelector = openModal TerrainPaletteModal
activateWorldEditorFunction EntitySelector = openModal EntityPaletteModal
activateWorldEditorFunction AreaSelector = do
  selectorStage <- use $ uiState . uiWorldEditor . editingBounds . boundsSelectionStep
  case selectorStage of
    SelectionComplete -> uiState . uiWorldEditor . editingBounds . boundsSelectionStep .= UpperLeftPending
    _ -> return ()
activateWorldEditorFunction OutputPathSelector =
  -- TODO
  liftIO $ putStrLn "File selection"
activateWorldEditorFunction MapSaveButton = saveMapFile
activateWorldEditorFunction ClearEntityButton =
  uiState . uiWorldEditor . entityPaintList . BL.listSelectedL .= Nothing

handleCtrlLeftClick :: B.Location -> EventM Name AppState ()
handleCtrlLeftClick mouseLoc = do
  worldEditor <- use $ uiState . uiWorldEditor
  _ <- runMaybeT $ do
    guard $ worldEditor ^. isWorldEditorEnabled
    let getSelected x = snd <$> BL.listSelectedElement x
        maybeTerrainType = getSelected $ worldEditor ^. terrainList
        maybeEntityPaint = getSelected $ worldEditor ^. entityPaintList
    -- TODO: Use hoistMaybe when available
    terrain <- MaybeT . pure $ maybeTerrainType
    mouseCoords <- MaybeT $ Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
    uiState . uiWorldEditor . paintedTerrain %= M.insert mouseCoords (terrain, maybeEntityPaint)
    uiState . uiWorldEditor . lastWorldEditorMessage .= Nothing
  immediatelyRedrawWorld
  return ()

handleRightClick :: B.Location -> EventM Name AppState ()
handleRightClick mouseLoc = do
  worldEditor <- use $ uiState . uiWorldEditor
  _ <- runMaybeT $ do
    guard $ worldEditor ^. isWorldEditorEnabled
    mouseCoords <- MaybeT $ Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
    uiState . uiWorldEditor . paintedTerrain %= M.delete mouseCoords
  immediatelyRedrawWorld
  return ()

-- | "Eye Dropper" tool:
handleMiddleClick :: B.Location -> EventM Name AppState ()
handleMiddleClick mouseLoc = do
  worldEditor <- use $ uiState . uiWorldEditor
  when (worldEditor ^. isWorldEditorEnabled) $ do
    w <- use $ gameState . world
    let setTerrainPaint coords = do
          let (terrain, maybeElementPaint) =
                EU.getContentAt
                  worldEditor
                  w
                  coords
          uiState . uiWorldEditor . terrainList %= BL.listMoveToElement terrain
          case maybeElementPaint of
            Nothing -> return ()
            Just elementPaint ->
              uiState . uiWorldEditor . entityPaintList %= BL.listMoveToElement p
             where
              p = case elementPaint of
                Facade efd -> efd
                Ref r -> mkPaint r

    mouseCoordsM <- Brick.zoom gameState $ mouseLocToWorldCoords mouseLoc
    whenJust mouseCoordsM setTerrainPaint

-- | Handle user input events in the robot panel.
handleWorldEditorPanelEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleWorldEditorPanelEvent = \case
  Key V.KEsc -> uiState . uiWorldEditor . editingBounds . boundsSelectionStep .= SelectionComplete
  Key V.KEnter -> do
    fring <- use $ uiState . uiWorldEditor . editorFocusRing
    case focusGetCurrent fring of
      Just (WorldEditorPanelControl x) -> activateWorldEditorFunction x
      _ -> return ()
  ControlChar 's' -> saveMapFile
  CharKey '\t' -> uiState . uiWorldEditor . editorFocusRing %= focusNext
  Key V.KBackTab -> uiState . uiWorldEditor . editorFocusRing %= focusPrev
  _ -> return ()

-- | Return value: whether the cursor position should be updated
updateAreaBounds :: Maybe W.Coords -> EventM Name AppState Bool
updateAreaBounds = \case
  Nothing -> return True
  Just mouseCoords -> do
    selectorStage <- use $ uiState . uiWorldEditor . editingBounds . boundsSelectionStep
    case selectorStage of
      UpperLeftPending -> do
        uiState . uiWorldEditor . editingBounds . boundsSelectionStep .= LowerRightPending mouseCoords
        return False
      -- TODO: Validate that the lower-right click is below and to the right of the top-left coord
      LowerRightPending upperLeftMouseCoords -> do
        uiState . uiWorldEditor . editingBounds . boundsRect
          .= Just (upperLeftMouseCoords, mouseCoords)
        uiState . uiWorldEditor . lastWorldEditorMessage .= Nothing
        uiState . uiWorldEditor . editingBounds . boundsSelectionStep .= SelectionComplete
        t <- liftIO $ getTime Monotonic
        uiState . uiWorldEditor . editingBounds . boundsPersistDisplayUntil .= t + TimeSpec 2 0
        setFocus WorldEditorPanel
        return False
      SelectionComplete -> return True

makeSuggestedPalette :: Maybe Scenario -> [[CellPaintDisplay]] -> KM.KeyMap CellPaintDisplay
makeSuggestedPalette maybeOriginalScenario cellGrid =
  KM.fromMapText $
    M.fromList $
      -- NOTE: the left-most maps take precedence!
      M.elems (paletteCellsByKey <> pairsWithDisplays <> terrainOnlyPalette)
 where
  getMaybeEntityDisplay (Cell _terrain maybeEntity _) = do
    EntityFacade eName d <- maybeEntity
    return (eName, d)

  getMaybeEntityNameTerrainPair (Cell terrain maybeEntity _) = do
    EntityFacade eName _ <- maybeEntity
    return (eName, terrain)

  getEntityTerrainMultiplicity :: Map EntityName (Map TerrainType Int)
  getEntityTerrainMultiplicity =
    M.map histogram $ binTuples $ concatMap (mapMaybe getMaybeEntityNameTerrainPair) cellGrid

  usedEntityDisplays :: Map EntityName Display
  usedEntityDisplays =
    M.fromList $ concatMap (mapMaybe getMaybeEntityDisplay) cellGrid

  -- Finds the most-used terrain type (the "mode" in the statistical sense)
  -- paired with each entity
  entitiesWithModalTerrain :: [(TerrainType, EntityName)]
  entitiesWithModalTerrain =
    map (swap . fmap (fst . NE.head)) $
      mapMaybe sequenceA $
        M.toList $
          M.map (NE.nonEmpty . sortOn snd . M.toList) getEntityTerrainMultiplicity

  invertPaletteMapToDedupe ::
    Map a CellPaintDisplay ->
    [(TerrainEntityNamePair, (a, CellPaintDisplay))]
  invertPaletteMapToDedupe =
    map (\x@(_, c) -> (toKey $ cellToTerrainEntityNamePair c, x)) . M.toList

  paletteCellsByKey :: Map TerrainEntityNamePair (T.Text, CellPaintDisplay)
  paletteCellsByKey =
    M.map (NE.head . NE.sortWith toSortVal) $
      binTuples $
        invertPaletteMapToDedupe $
          KM.toMapText originalPalette
   where
    toSortVal (symbol, Cell _terrain _maybeEntity robots) = Down (null robots, symbol)

  excludedPaletteChars :: Set Char
  excludedPaletteChars = Set.fromList [' ']

  originalPalette :: KM.KeyMap CellPaintDisplay
  originalPalette =
    KM.map toCellPaintDisplay $
      maybe mempty (unPalette . palette . (^. scenarioWorld)) maybeOriginalScenario

  pairsWithDisplays :: Map TerrainEntityNamePair (T.Text, CellPaintDisplay)
  pairsWithDisplays = M.fromList $ mapMaybe g entitiesWithModalTerrain
   where
    g (terrain, eName) = do
      eDisplay <- M.lookup eName usedEntityDisplays
      let displayChar = eDisplay ^. defaultChar
      guard $ Set.notMember displayChar excludedPaletteChars
      let cell = Cell terrain (Just $ EntityFacade eName eDisplay) []
      return ((terrain, Just eName), (T.singleton displayChar, cell))

  -- TODO: Filter out terrain-only palette entries that aren't actually
  -- used in the map.
  terrainOnlyPalette :: Map TerrainEntityNamePair (T.Text, CellPaintDisplay)
  terrainOnlyPalette = M.fromList $ map f U.listEnums
   where
    f x = ((x, Nothing), (T.singleton $ getTerrainDefaultPaletteChar x, Cell x Nothing []))

constructScenario :: Maybe Scenario -> [[CellPaintDisplay]] -> SkeletonScenario
constructScenario maybeOriginalScenario cellGrid =
  SkeletonScenario
    (maybe 1 (^. scenarioVersion) maybeOriginalScenario)
    (maybe "My Scenario" (^. scenarioName) maybeOriginalScenario)
    (maybe "The scenario description..." (^. scenarioDescription) maybeOriginalScenario)
    -- (maybe True (^. scenarioCreative) maybeOriginalScenario)
    True
    (M.elems $ entitiesByName customEntities)
    wd
    [] -- robots
 where
  customEntities = maybe mempty (^. scenarioEntities) maybeOriginalScenario
  wd =
    WorldDescription
      { defaultTerrain = Just $ Cell BlankT Nothing []
      , offsetOrigin = False
      , scrollable = True
      , palette = WorldPalette suggestedPalette
      , ul = upperLeftCoord
      , area = cellGrid
      }

  suggestedPalette = makeSuggestedPalette maybeOriginalScenario cellGrid

  upperLeftCoord =
    Location
      (negate $ w `div` 2)
      (h `div` 2)
   where
    AreaDimensions w h = getAreaDimensions cellGrid

saveMapFile :: EventM Name AppState ()
saveMapFile = do
  worldEditor <- use $ uiState . uiWorldEditor
  maybeBounds <- use $ uiState . uiWorldEditor . editingBounds . boundsRect
  w <- use $ gameState . world
  let mapCellGrid = EU.getEditedMapRectangle worldEditor maybeBounds w

  let fp = worldEditor ^. outputFilePath
  maybeScenarioPair <- use $ uiState . scenarioRef
  liftIO $ Y.encodeFile fp $ constructScenario (fst <$> maybeScenarioPair) mapCellGrid

  uiState . uiWorldEditor . lastWorldEditorMessage .= Just "Saved."
