{-# LANGUAGE OverloadedStrings #-}

-- |
-- Rendering of cells in the map view
--
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.CellDisplay where

import Brick
import Control.Lens (to, view, (&), (.~), (^.), _Just)
import Data.ByteString (ByteString)
import Data.Hash.Murmur
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Semigroup (sconcat)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tagged (unTagged)
import Data.Word (Word32)
import Graphics.Vty qualified as V
import Linear.Affine ((.-.))
import Swarm.Game.Cosmetic.Color (AttributeMap, TrueColor (..), NamedColor (..), PreservableColor)
import Swarm.Game.Cosmetic.Display
import Swarm.Game.Cosmetic.Texel (Texel, getTexelData, mkTexel, texelFromColor)
import Swarm.Game.Entity
import Swarm.Game.Land
import Swarm.Game.Location (Point (..), toHeading)
import Swarm.Game.Robot
import Swarm.Game.Scenario (scenarioCosmetics, scenarioLandscape)
import Swarm.Game.Scenario.Status (getScenario)
import Swarm.Game.Scenario.Topography.EntityFacade
import Swarm.Game.Scenario.Topography.Structure.Recognition (foundStructures)
import Swarm.Game.Scenario.Topography.Structure.Recognition.Registry (foundByLocation)
import Swarm.Game.State
import Swarm.Game.State.Landscape
import Swarm.Game.State.Robot
import Swarm.Game.State.Substate
import Swarm.Game.Terrain
import Swarm.Game.Tick (TickNumber (..))
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
import Swarm.Util (applyWhen)
import Swarm.Util.Content (getContentAt)
import Witch (from)
import Witch.Encoding qualified as Encoding

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

-- | XXX draw terrain
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

-- | XXX draw all the robots
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

-- XXX this should DEFINITELY be simplified!  What is it doing?
renderEntityCell ::
  WorldOverdraw ->
  RenderingInput ->
  Cosmic Coords ->
  Texel TrueColor
renderEntityCell worldEditor ri coords =
  maybe mempty (renderEntityPaint (attributeMap ri)) (getEntityPaintAtCoord coords)
 where
  getEntityPaintAtCoord = snd . EU.getEditorContentAt (terrMap ri) worldEditor (multiworldInfo ri)
  coordHasBoundary = maybe False (`hasProperty` Boundary) . snd . getContentAt (terrMap ri) (multiworldInfo ri)

  -- assignBoundaryOverride = applyWhen (coordHasBoundary coords) (boundaryOverride .~ getBoundaryDisplay checkPresence)
  --  where
  --   checkPresence :: AbsoluteDir -> Bool
  --   checkPresence d = coordHasBoundary offsettedCoord
  --    where
  --     offsettedCoord = (`addTuple` xy) <$> coords
  --     Coords xy = locToCoords $ P $ toHeading d

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

------------------------------------------------------------
-- Static
------------------------------------------------------------

-- XXX move static to another module?

-- | Random "static" based on the distance to the robot being
--   @view@ed.
renderStaticAt :: GameState -> Coords -> Texel TrueColor
renderStaticAt g coords = maybe mempty renderStatic (getStatic g coords)

-- | Draw static given a number from 0-15 representing the state of
--   the four quarter-pixels in a cell
renderStatic :: Word32 -> Texel TrueColor
renderStatic s = mkTexel (Just (maxBound, (staticChar s, AnsiColor White))) Nothing

-- | Given a value from 0--15, considered as 4 bits, pick the
--   character with the corresponding quarter pixels turned on.
staticChar :: Word32 -> Char
staticChar = \case
  0 -> ' '
  1 -> '▖'
  2 -> '▗'
  3 -> '▄'
  4 -> '▘'
  5 -> '▌'
  6 -> '▚'
  7 -> '▙'
  8 -> '▝'
  9 -> '▞'
  10 -> '▐'
  11 -> '▟'
  12 -> '▀'
  13 -> '▛'
  14 -> '▜'
  15 -> '█'
  _ -> ' '

-- | Random "static" based on the distance to the robot being
--   @view@ed.  A cell can either be static-free (represented by
--   @Nothing@) or can have one of sixteen values (representing the
--   state of the four quarter-pixels in one cell).
getStatic :: GameState -> Coords -> Maybe Word32
getStatic g coords
  | isStatic = Just (h `mod` 16)
  | otherwise = Nothing
 where
  -- Offset from the location of the view center to the location under
  -- consideration for display.
  offset = coordsToLoc coords .-. (g ^. robotInfo . viewCenter . planar)

  -- Hash.
  h =
    murmur3 1 . unTagged . from @String @(Encoding.UTF_8 ByteString) . show $
      -- include the current tick count / 16 in the hash, so the pattern of static
      -- changes once every 16 ticks
      (offset, getTickNumber (g ^. temporal . ticks) `div` 16)

  -- Hashed probability, i.e. convert the hash into a floating-point number between 0 and 1
  hp :: Double
  hp = fromIntegral h / fromIntegral (maxBound :: Word32)

  isStatic = case focusedRange g of
    -- If we're not viewing a robot, display static.  This
    -- can happen if e.g. the robot we were viewing drowned.
    -- This is overridden by creative mode, e.g. when no robots
    -- have been defined for the scenario.
    Nothing -> not $ g ^. creativeMode
    -- Don't display static if the robot is close, or when we're in
    -- creative mode or the player is allowed to scroll the world.
    Just Close -> False
    -- At medium distances, replace cell with static with a
    -- probability that increases with distance.
    Just (MidRange s) -> hp < 1 - cos (s * (pi / 2))
    -- Far away, everything is static.
    Just Far -> True
