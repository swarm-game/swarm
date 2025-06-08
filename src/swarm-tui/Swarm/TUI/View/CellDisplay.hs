{-# LANGUAGE OverloadedStrings #-}

-- |
-- Rendering of cells in the map view
--
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.CellDisplay where

import Brick
import Control.Lens ((^.))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Graphics.Vty qualified as V
import Linear (zero)
import Swarm.Game.Cosmetic.Color (AttributeMap, TrueColor (..), PreservableColor)
import Swarm.Game.Cosmetic.Display
import Swarm.Game.Cosmetic.Texel (Texel, getTexelData, texelFromColor)
import Swarm.Game.Entity
import Swarm.Game.Land
import Swarm.Game.Location (Point (..), toHeading)
import Swarm.Game.Robot
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Scenario.Topography.Structure.Recognition (foundStructures)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry (foundByLocation)
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Substate
import Swarm.Game.Terrain
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.Game.World.Coords
import Swarm.Language.Syntax.Direction (AbsoluteDir (..))
import Swarm.TUI.Editor.Masking
import Swarm.TUI.Editor.Model
import Swarm.TUI.Editor.Util qualified as EU
import Swarm.TUI.Model.Name
import Swarm.TUI.Model.UI.Gameplay
import Swarm.TUI.View.Attribute.Attr
import Swarm.TUI.View.Static
import Swarm.Util (applyWhen)
import Swarm.Util.Content (getContentAt)

-- | Render a texel as a UI widget.
renderTexel :: Texel TrueColor -> Widget n
renderTexel t =
  let (mfg, mbg) = getTexelData t
      displayChar = maybe ' ' fst mfg
      setFG = maybe id (\(_, c) -> modifyDefAttr (`V.withForeColor` (mkBrickColor c))) mfg
      setBG = maybe id (\c -> modifyDefAttr (`V.withBackColor` (mkBrickColor c))) mbg
  in setBG . setFG $ str [displayChar]

-- | Render the 'Display' for a specific location.
drawLoc :: UIGameplay -> GameState -> Cosmic Coords -> Widget Name
drawLoc ui g cCoords@(Cosmic _ coords) =
  if shouldHideWorldCell ui coords
    then str " "
    else boldStructure drawCell
 where
  showRobots = ui ^. uiShowRobots
  we = ui ^. uiWorldEditor . worldOverdraw
  aMap = ui ^. uiAttributeMap
  drawCell = renderTexel $ renderLoc showRobots we g aMap cCoords

  boldStructure = applyWhen isStructure $ modifyDefAttr (`V.withStyle` V.bold)
   where
    sMap = foundByLocation $ g ^. discovery . structureRecognition . foundStructures
    isStructure = M.member (coordsToLoc <$> cCoords) sMap

-- | Subset of the game state needed to render the world
data RenderingInput = RenderingInput
  { multiworldInfo :: W.MultiWorld Int Entity
  , isKnownFunc :: EntityPaint -> Bool
  , terrMap :: TerrainMap
  , attributeMap :: Map Attribute PreservableColor
  }

-- | Render a single terrain cell as a texel.
renderTerrainCell ::
  WorldOverdraw ->
  RenderingInput ->
  Cosmic Coords ->
  Texel TrueColor
renderTerrainCell worldEditor ri coords =
  maybe mempty (terrainTexel (attributeMap ri)) $ M.lookup t tm
 where
  tm = terrainByName $ terrMap ri
  t = EU.getEditorTerrainAt (terrMap ri) worldEditor (multiworldInfo ri) coords

terrainTexel :: Map Attribute PreservableColor -> TerrainObj -> Texel TrueColor
terrainTexel aMap terrain =
  let mcolor = M.lookup (terrainAttr terrain) aMap
  in  maybe mempty (texelFromColor 0 ' ') mcolor

-- | Render all the robots on a given cell as a combined texel.
renderRobotCell ::
  AttributeMap ->
  GameState ->
  Cosmic Coords ->
  Texel TrueColor
renderRobotCell aMap g coords =
  foldMap (renderRobot aMap) $
    robotsAtLocation (fmap coordsToLoc coords) g

-- | Extract the relevant subset of information from the 'GameState' to be able
-- to compute whether an entity is "known".
mkEntityKnowledge :: GameState -> EntityKnowledgeDependencies
mkEntityKnowledge gs =
  EntityKnowledgeDependencies
    { isCreativeMode = gs ^. creativeMode
    , globallyKnownEntities = gs ^. discovery . knownEntities
    , theFocusedRobot = focusedRobot gs
    }

-- | The subset of information required to compute whether
-- an entity is "known", and therefore should be rendered
-- normally vs as a question mark.
data EntityKnowledgeDependencies = EntityKnowledgeDependencies
  { isCreativeMode :: Bool
  , globallyKnownEntities :: Set EntityName
  , theFocusedRobot :: Maybe Robot
  }

-- | Determines whether an entity should be rendered
-- normally vs as a question mark.
getEntityIsKnown :: EntityKnowledgeDependencies -> EntityPaint -> Bool
getEntityIsKnown knowledge ep = case ep of
  Facade (EntityFacade {}) -> True
  Ref e -> or reasonsToShow
   where
    reasonsToShow =
      [ isCreativeMode knowledge
      , e `hasProperty` Known
      , (e ^. entityName) `S.member` globallyKnownEntities knowledge
      , showBasedOnRobotKnowledge
      ]
    showBasedOnRobotKnowledge = maybe False (`robotKnows` e) $ theFocusedRobot knowledge

-- | Render the entity at the given coordinates (if any) to a texel,
--   taking into account things such as neighboring entities with the
--   boundary property, and whether the entity should have its
--   identity hidden.
renderEntityCell ::
  WorldOverdraw ->
  RenderingInput ->
  Cosmic Coords ->
  Texel TrueColor
renderEntityCell worldEditor ri coords =
  maybe mempty (renderEntityPaint (attributeMap ri) checkPresence known) mEntPaint
 where
  mEntPaint = getEntityPaintAtCoord coords
  known = fromMaybe False . fmap (isKnownFunc ri) $ mEntPaint
  getEntityPaintAtCoord = snd . EU.getEditorContentAt (terrMap ri) worldEditor (multiworldInfo ri)
  coordHasBoundary = maybe False (`hasProperty` Boundary) . snd . getContentAt (terrMap ri) (multiworldInfo ri)

  checkPresence :: Maybe AbsoluteDir -> Bool
  checkPresence d = coordHasBoundary offsetCoord
   where
    offsetCoord = (`addTuple` xy) <$> coords
    Coords xy = locToCoords . P . fromMaybe zero $ toHeading <$> d

-- | Render a specific location, by combining the
--   texels for the terrain, entity, and robots at the location, and
--   taking into account "static" based on the distance to the robot
--   being @view@ed.
renderLoc :: Bool -> WorldOverdraw -> GameState -> Map Attribute PreservableColor -> Cosmic Coords -> Texel TrueColor
renderLoc showRobots we g aMap cCoords@(Cosmic _ coords) =
  renderStaticAt g coords <> robots <> renderBaseLoc we ri cCoords
 where
  ri =
    RenderingInput
      (g ^. landscape . multiWorld)
      (getEntityIsKnown $ mkEntityKnowledge g)
      (g ^. landscape . terrainAndEntities . terrainMap)
      aMap

  robots =
    if showRobots
      then renderRobotCell aMap g cCoords
      else mempty

-- | Render a base location without robots, /i.e./ just the terrain and
--   entity (if any).
renderBaseLoc ::
  WorldOverdraw ->
  RenderingInput ->
  Cosmic Coords ->
  Texel TrueColor
renderBaseLoc worldEditor ri coords =
  renderTerrainCell worldEditor ri coords <> renderEntityCell worldEditor ri coords

