-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- GameState- and TUI-independent world rendering.
module Swarm.Game.World.Render where

import Codec.Picture
import Control.Applicative ((<|>))
import Control.Effect.Lift (sendIO)
import Control.Lens (view, (^.))
import Control.Monad.IO.Class (liftIO)
import Data.Colour.SRGB (RGB (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (both)
import Data.Vector qualified as V
import Linear (V2 (..))
import Swarm.Game.Display (defaultChar)
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Location
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Center
import Swarm.Game.Scenario.Topography.EntityFacade (EntityFacade (..), mkFacade)
import Swarm.Game.State.Landscape
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.Game.World.Gen (Seed)
import Swarm.Util (surfaceEmpty)
import Swarm.Util.Content
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

mkPixelColor :: PreservableColor -> PixelRGBA8
mkPixelColor h = PixelRGBA8 r g b 255
 where
  RGB r g b = flattenBg $ fromHiFi h

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
  Triple x -> x
  -- The triples we've manually assigned for named
  -- ANSI colors do not need to be round-tripped, since
  -- those triples are not inputs to the VTY attribute creation.
  AnsiColor x -> namedToTriple x

-- | When output size is not explicitly provided,
-- uses natural map bounds (if a map exists).
getBoundingBox ::
  Location ->
  PWorldDescription e ->
  Maybe AreaDimensions ->
  W.BoundsRectangle
getBoundingBox vc scenarioWorld maybeSize =
  mkBoundingBox areaDims upperLeftLocation
 where
  upperLeftLocation =
    if null maybeSize && not (isEmpty mapAreaDims)
      then ul scenarioWorld
      else vc .+^ ((`div` 2) <$> V2 (negate w) h)

  mkBoundingBox areaDimens upperLeftLoc =
    both W.locToCoords locationBounds
   where
    lowerRightLocation = upperLeftToBottomRight areaDimens upperLeftLoc
    locationBounds = (upperLeftLoc, lowerRightLocation)

  worldArea = area scenarioWorld
  mapAreaDims = getAreaDimensions worldArea
  areaDims@(AreaDimensions w h) =
    fromMaybe (AreaDimensions 20 10) $
      maybeSize <|> surfaceEmpty isEmpty mapAreaDims

getDisplayGrid ::
  Location ->
  Scenario ->
  Landscape ->
  Maybe AreaDimensions ->
  Grid CellPaintDisplay
getDisplayGrid vc myScenario ls maybeSize =
  getMapRectangle
    mkFacade
    (getContentAt worlds . mkCosmic)
    (getBoundingBox vc firstScenarioWorld maybeSize)
 where
  mkCosmic = Cosmic $ worldName firstScenarioWorld
  worlds = view multiWorld ls

  firstScenarioWorld = NE.head $ view scenarioWorlds myScenario

getRenderableGrid ::
  RenderOpts ->
  FilePath ->
  IO (Grid (PCell EntityFacade), M.Map WorldAttr PreservableColor)
getRenderableGrid (RenderOpts maybeSeed _ _ maybeSize) fp = simpleErrorHandle $ do
  (myScenario, gsi) <- loadStandaloneScenario fp
  theSeed <- liftIO $ arbitrateSeed maybeSeed myScenario

  let em = integrateScenarioEntities gsi myScenario
      worldTuples = buildWorldTuples myScenario
      myLandscape = mkLandscape myScenario em worldTuples theSeed

      vc =
        view planar $
          determineStaticViewCenter myScenario $
            buildWorldTuples myScenario

  return (getDisplayGrid vc myScenario myLandscape maybeSize, myScenario ^. scenarioCosmetics)

doRenderCmd :: RenderOpts -> FilePath -> IO ()
doRenderCmd opts@(RenderOpts _ asPng _ _) mapPath =
  case asPng of
    ConsoleText -> printScenarioMap =<< renderScenarioMap opts mapPath
    PngImage -> renderScenarioPng opts mapPath

renderScenarioMap :: RenderOpts -> FilePath -> IO [String]
renderScenarioMap opts fp = do
  (grid, _) <- getRenderableGrid opts fp
  return $ unGrid $ getDisplayChar <$> grid

-- | Converts linked lists to vectors to facilitate
-- random access when assembling the image
gridToVec :: Grid a -> V.Vector (V.Vector a)
gridToVec (Grid g) = V.fromList . map V.fromList $ g

renderScenarioPng :: RenderOpts -> FilePath -> IO ()
renderScenarioPng opts fp = do
  (grid, aMap) <- getRenderableGrid opts fp
  writePng (outputFilepath opts) $ mkImg aMap grid
 where
  mkImg aMap g = generateImage (pixelRenderer vecGrid) (fromIntegral w) (fromIntegral h)
   where
    vecGrid = gridToVec g
    AreaDimensions w h = getGridDimensions g
    pixelRenderer vg x y = getDisplayColor aMap $ (vg V.! y) V.! x

printScenarioMap :: [String] -> IO ()
printScenarioMap =
  sendIO . mapM_ putStrLn
