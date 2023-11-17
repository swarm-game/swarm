-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- TUI-independent world rendering.
module Swarm.Game.World.Render where

import Brick (AttrMap, applyAttrMappings, attrMapLookup)
import Codec.Picture
import Control.Applicative ((<|>))
import Control.Effect.Lift (sendIO)
import Control.Lens (to, view, (^.))
import Data.Bifunctor (first)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Swarm.Doc.Gen (loadStandaloneScenario)
import Swarm.Game.Display (defaultChar, displayAttr)
import Swarm.Game.ResourceLoading (initNameGenerator, readAppData)
import Swarm.Game.Scenario (Scenario, area, scenarioAttrs, scenarioWorlds, ul, worldName)
import Swarm.Game.Scenario.Status (seedLaunchParams)
import Swarm.Game.Scenario.Topography.Area (AreaDimensions (..), getAreaDimensions, isEmpty, upperLeftToBottomRight)
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.EntityFacade (EntityFacade (..), mkFacade)
import Swarm.Game.State
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.TUI.Editor.Util (getContentAt, getMapRectangle)
import Swarm.TUI.View.Attribute.Attr (getWorldAttrName, swarmAttrMap, toAttrName)
import Swarm.TUI.View.Attribute.CustomStyling (toAttrPair)
import Swarm.Util (surfaceEmpty)
import Swarm.Util.Effect (simpleErrorHandle)
import Swarm.Util.Erasable (erasableToMaybe)

data OuputFormat
  = ConsoleText
  | PngImage

-- | Command-line options for configuring the app.
data RenderOpts = RenderOpts
  { renderSeed :: Maybe Seed
  -- ^ Explicit seed chosen by the user.
  , outputFormat :: OuputFormat
  , outputFilepath :: FilePath
  , gridSize :: Maybe AreaDimensions
  }

getDisplayChar :: PCell EntityFacade -> Char
getDisplayChar = maybe ' ' facadeChar . erasableToMaybe . cellEntity
 where
  facadeChar (EntityFacade _ d) = view defaultChar d

getDisplayColor :: AttrMap -> PCell EntityFacade -> PixelRGBA8
getDisplayColor aMap (Cell terr cellEnt _) =
  maybe transparent facadeColor $ erasableToMaybe cellEnt
 where
  transparent = PixelRGBA8 0 0 0 0
  facadeColor (EntityFacade _ d) = PixelRGBA8 255 0 255 255
   where
    -- facadeColor (EntityFacade _ d) = case attrForeColor attr of
    --   SetTo c -> case c of
    --     RGBColor r g b -> PixelRGBA8 r g b 255
    --     _ -> transparent
    --   _ -> transparent

    attr = attrMapLookup (d ^. displayAttr . to toAttrName) aMap

getDisplayGrid :: Scenario -> GameState -> Maybe AreaDimensions -> [[PCell EntityFacade]]
getDisplayGrid myScenario gs maybeSize =
  getMapRectangle
    mkFacade
    (getContentAt worlds . mkCosmic)
    boundingBox
 where
  worlds = view (landscape . multiWorld) gs

  firstScenarioWorld = NE.head $ view scenarioWorlds myScenario
  worldArea = area firstScenarioWorld
  upperLeftLocation = ul firstScenarioWorld
  rawAreaDims = getAreaDimensions worldArea
  areaDims = fromMaybe (AreaDimensions 20 10) $ maybeSize <|> surfaceEmpty isEmpty rawAreaDims
  lowerRightLocation = upperLeftToBottomRight areaDims upperLeftLocation

  mkCosmic = Cosmic $ worldName firstScenarioWorld
  boundingBox = (W.locToCoords upperLeftLocation, W.locToCoords lowerRightLocation)

getRenderableGrid :: RenderOpts -> FilePath -> IO ([[PCell EntityFacade]], AttrMap)
getRenderableGrid (RenderOpts maybeSeed _ _ maybeSize) fp = simpleErrorHandle $ do
  (myScenario, (worldDefs, entities, recipes)) <- loadStandaloneScenario fp
  let aMap = applyAttrMappings (map (first getWorldAttrName . toAttrPair) $ myScenario ^. scenarioAttrs) swarmAttrMap
  appDataMap <- readAppData
  nameGen <- initNameGenerator appDataMap
  let gsc = GameStateConfig nameGen entities recipes worldDefs
  gs <-
    sendIO $
      scenarioToGameState
        myScenario
        (seedLaunchParams maybeSeed)
        gsc
  return (getDisplayGrid myScenario gs maybeSize, aMap)

doRenderCmd :: RenderOpts -> FilePath -> IO ()
doRenderCmd opts@(RenderOpts _ asPng _ _) mapPath =
  case asPng of
    ConsoleText -> do
      outputLines <- renderScenarioMap opts mapPath
      printScenarioMap outputLines
    PngImage -> renderScenarioPng opts mapPath

renderScenarioMap :: RenderOpts -> FilePath -> IO [String]
renderScenarioMap opts fp = do
  (grid, _) <- getRenderableGrid opts fp
  return $ map (map getDisplayChar) grid

gridToVec :: [[a]] -> V.Vector (V.Vector a)
gridToVec = V.fromList . map V.fromList

renderScenarioPng :: RenderOpts -> FilePath -> IO ()
renderScenarioPng opts fp = do
  (grid, aMap) <- getRenderableGrid opts fp
  writePng (outputFilepath opts) $ mkImg aMap grid
 where
  mkImg aMap g = generateImage (pixelRenderer vecGrid) (fromIntegral w) (fromIntegral h)
   where
    vecGrid = gridToVec g
    AreaDimensions w h = getAreaDimensions g
    pixelRenderer vg x y = getDisplayColor aMap $ (vg V.! y) V.! x

printScenarioMap :: [String] -> IO ()
printScenarioMap =
  sendIO . mapM_ putStrLn
