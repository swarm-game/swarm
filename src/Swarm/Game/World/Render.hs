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
import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import Graphics.Vty.Attributes.Color240
import Linear (V2 (..))
import Swarm.Game.Display (defaultChar)
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Location
import Swarm.Game.ResourceLoading (initNameGenerator, readAppData)
import Swarm.Game.Scenario (Scenario, area, loadStandaloneScenario, scenarioCosmetics, scenarioWorlds, ul, worldName)
import Swarm.Game.Scenario.Status (seedLaunchParams)
import Swarm.Game.Scenario.Topography.Area (AreaDimensions (..), getAreaDimensions, isEmpty, upperLeftToBottomRight)
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Center
import Swarm.Game.Scenario.Topography.EntityFacade (EntityFacade (..), mkFacade)
import Swarm.Game.State
import Swarm.Game.State.Substate
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.TUI.Editor.Util (getContentAt, getMapRectangle)
import Swarm.TUI.View.CellDisplay (getTerrainEntityColor)
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

getDisplayColor :: M.Map WorldAttr PreservableColor -> PCell EntityFacade -> PixelRGBA8
getDisplayColor aMap c =
  maybe transparent mkPixelColor $ getTerrainEntityColor aMap c
 where
  transparent = PixelRGBA8 0 0 0 0

-- | Round-trip conversion to fit into the terminal color space
roundTripVty :: RGBColor -> RGBColor
roundTripVty c@(RGB r g b) =
  maybe
    c
    (\(r', g', b') -> fromIntegral <$> RGB r' g' b')
    converted
 where
  converted = color240CodeToRGB $ rgbColorToColor240 r g b

mkPixelColor :: PreservableColor -> PixelRGBA8
mkPixelColor h = PixelRGBA8 r g b 255
 where
  RGB r g b = case fromHiFi h of
    FgOnly c -> c
    BgOnly c -> c
    FgAndBg _ c -> c

-- | Since terminals can customize these named
-- colors using themes or explicit user overrides,
-- these color assignments are somewhat arbitrary.
namedToTriple :: NamedColor -> RGBColor
namedToTriple = \case
  White -> RGB 208 207 204
  BrightRed -> RGB 246 97 81
  Red -> RGB 192 28 40
  Green -> RGB 38 162 105
  Blue -> RGB 18 72 139
  BrightYellow -> RGB 233 173 12
  Yellow -> RGB 162 115 76

fromHiFi :: PreservableColor -> ColorLayers RGBColor
fromHiFi = fmap $ \case
  Triple x -> roundTripVty x
  -- The triples we've manually assigned for named
  -- ANSI colors do not need to be round-tripped, since
  -- those triples are not inputs to the VTY attribute creation.
  AnsiColor x -> namedToTriple x

-- | When output size is not explicitly provided on command line,
-- uses natural map bounds (if a map exists).
getDisplayGrid ::
  Scenario ->
  GameState ->
  Maybe AreaDimensions ->
  [[PCell EntityFacade]]
getDisplayGrid myScenario gs maybeSize =
  getMapRectangle
    mkFacade
    (getContentAt worlds . mkCosmic)
    (mkBoundingBox areaDims upperLeftLocation)
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

  upperLeftLocation =
    if null maybeSize && not (isEmpty mapAreaDims)
      then ul firstScenarioWorld
      else view planar vc .+^ ((`div` 2) <$> V2 (negate w) h)

  mkBoundingBox areaDimens upperLeftLoc =
    both W.locToCoords locationBounds
   where
    lowerRightLocation = upperLeftToBottomRight areaDimens upperLeftLoc
    locationBounds = (upperLeftLoc, lowerRightLocation)

getRenderableGrid ::
  RenderOpts ->
  FilePath ->
  IO ([[PCell EntityFacade]], M.Map WorldAttr PreservableColor)
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
    ConsoleText -> printScenarioMap =<< renderScenarioMap opts mapPath
    PngImage -> renderScenarioPng opts mapPath

renderScenarioMap :: RenderOpts -> FilePath -> IO [String]
renderScenarioMap opts fp = do
  (grid, _) <- getRenderableGrid opts fp
  return $ map (map getDisplayChar) grid

-- | Converts linked lists to vectors to facilitate
-- random access when assembling the image
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
