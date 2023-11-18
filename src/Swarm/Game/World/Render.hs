-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- TUI-independent world rendering.
module Swarm.Game.World.Render where

import Codec.Picture
import Control.Applicative ((<|>))
import Control.Effect.Lift (sendIO)
import Control.Lens (view, (^.))
import Data.Colour.SRGB (RGB (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import Linear (V2 (..))
import Swarm.Doc.Gen (loadStandaloneScenario)
import Swarm.Game.Display (Attribute (AWorld), defaultChar, displayAttr)
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Location
import Swarm.Game.ResourceLoading (initNameGenerator, readAppData)
import Swarm.Game.Scenario (Scenario, area, scenarioCosmetics, scenarioWorlds, ul, worldName)
import Swarm.Game.Scenario.Status (seedLaunchParams)
import Swarm.Game.Scenario.Topography.Area (AreaDimensions (..), getAreaDimensions, isEmpty, upperLeftToBottomRight)
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.EntityFacade (EntityFacade (..), mkFacade)
import Swarm.Game.State
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.TUI.Editor.Util (getContentAt, getMapRectangle)
import Swarm.TUI.View.Util (determineViewCenter)
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

getDisplayColor :: M.Map WorldAttr HiFiColor -> PCell EntityFacade -> PixelRGBA8
getDisplayColor aMap (Cell _terr cellEnt _) =
  maybe transparent facadeColor $ erasableToMaybe cellEnt
 where
  transparent = PixelRGBA8 0 0 0 0
  facadeColor (EntityFacade _ d) = maybe transparent mkPixelColor $ case d ^. displayAttr of
    AWorld n -> M.lookup (WorldAttr $ T.unpack n) aMap
    _ -> Nothing

mkPixelColor :: HiFiColor -> PixelRGBA8
mkPixelColor h = PixelRGBA8 r g b 255
 where
  RGB r g b = case h of
    FgOnly c -> c
    BgOnly c -> c
    -- TODO: if displayChar is whitespace, use bg color. Otherwise use fg color.
    FgAndBg _ c -> c

getDisplayGrid :: Scenario -> GameState -> Maybe AreaDimensions -> [[PCell EntityFacade]]
getDisplayGrid myScenario gs maybeSize =
  getMapRectangle
    mkFacade
    (getContentAt worlds . mkCosmic)
    boundingBox
 where
  mkCosmic = Cosmic $ worldName firstScenarioWorld

  worlds = view (landscape . multiWorld) gs

  worldTuples = buildWorldTuples myScenario
  vc = determineViewCenter myScenario worldTuples

  firstScenarioWorld = NE.head $ view scenarioWorlds myScenario
  worldArea = area firstScenarioWorld
  mapAreaDims = getAreaDimensions worldArea
  areaDims@(AreaDimensions w h) =
    fromMaybe (AreaDimensions 20 10) $
      maybeSize <|> surfaceEmpty isEmpty mapAreaDims

  upperLeftLocation = view planar vc .+^ V2 (negate $ floor $ fromIntegral w / 2) (floor $ fromIntegral h / 2)
  lowerRightLocation = upperLeftToBottomRight areaDims upperLeftLocation

  locationBounds = (upperLeftLocation, lowerRightLocation)
  boundingBox = both W.locToCoords locationBounds

getRenderableGrid :: RenderOpts -> FilePath -> IO ([[PCell EntityFacade]], M.Map WorldAttr HiFiColor)
getRenderableGrid (RenderOpts maybeSeed _ _ maybeSize) fp = simpleErrorHandle $ do
  (myScenario, (worldDefs, entities, recipes)) <- loadStandaloneScenario fp
  appDataMap <- readAppData
  nameGen <- initNameGenerator appDataMap
  let gsc = GameStateConfig nameGen entities recipes worldDefs
  gs <-
    sendIO $
      scenarioToGameState
        myScenario
        (seedLaunchParams maybeSeed)
        gsc
  return (getDisplayGrid myScenario gs maybeSize, myScenario ^. scenarioCosmetics)

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
