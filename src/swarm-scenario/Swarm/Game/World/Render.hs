-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- GameState- and TUI-independent world rendering.
module Swarm.Game.World.Render where

import Swarm.Game.Scenario.Topography.WorldDescription
import Codec.Picture
import Control.Applicative ((<|>))
import Control.Carrier.Throw.Either (runThrow)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Throw
import Control.Lens (view, (^.))
import Data.Colour.SRGB (RGB (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (both)
import Linear (V2 (..))
import Swarm.Game.Display (defaultChar)
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Failure (SystemFailure, simpleErrorHandle)
import Swarm.Game.Land
import Swarm.Game.Location
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Center
import Swarm.Game.Scenario.Topography.EntityFacade (EntityFacade (..), mkFacade)
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Rasterize
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Swarm.Game.State.Landscape
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.Game.World.Gen (Seed)
import Swarm.Language.Pretty (prettyString)
import Swarm.Util (surfaceEmpty)
import Swarm.Util.Content
import Swarm.Util.Erasable (erasableToMaybe)
import System.IO (hPutStrLn, stderr)

data OuputFormat
  = ConsoleText
  | PngImage

data FailureMode
  = Terminate
  | RenderBlankImage

-- | Command-line options for configuring the app.
data RenderOpts = RenderOpts
  { renderSeed :: Maybe Seed
  -- ^ Explicit seed chosen by the user.
  , outputFormat :: OuputFormat
  , outputFilepath :: FilePath
  , gridSize :: Maybe AreaDimensions
  , failureMode :: FailureMode
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
  BoundsRectangle
getBoundingBox vc scenarioWorld maybeSize =
  mkBoundingBox areaDims upperLeftLocation
 where
  upperLeftLocation =
    if null maybeSize && not (isEmpty mapAreaDims)
      then gridPosition $ area scenarioWorld
      else vc .+^ ((`div` 2) <$> V2 (negate w) h)

  mkBoundingBox areaDimens upperLeftLoc =
    both locToCoords locationBounds
   where
    lowerRightLocation = computeBottomRightFromUpperLeft areaDimens upperLeftLoc
    locationBounds = (upperLeftLoc, lowerRightLocation)

  worldArea = gridContent $ area scenarioWorld
  mapAreaDims = getGridDimensions worldArea
  areaDims@(AreaDimensions w h) =
    fromMaybe (AreaDimensions 20 10) $
      maybeSize <|> surfaceEmpty isEmpty mapAreaDims

getDisplayGrid ::
  Location ->
  ScenarioLandscape ->
  Landscape ->
  Maybe AreaDimensions ->
  Grid CellPaintDisplay
getDisplayGrid vc sLandscape ls maybeSize =
  getMapRectangle
    mkFacade
    (getContentAt (sLandscape ^. scenarioTerrainAndEntities . terrainMap) worlds . mkCosmic)
    (getBoundingBox vc firstScenarioWorld maybeSize)
 where
  mkCosmic = Cosmic $ worldName firstScenarioWorld
  worlds = view multiWorld ls

  firstScenarioWorld = NE.head $ view scenarioWorlds sLandscape

getRenderableGrid ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  RenderOpts ->
  FilePath ->
  m (Grid (PCell EntityFacade), M.Map WorldAttr PreservableColor)
getRenderableGrid (RenderOpts maybeSeed _ _ maybeSize _) fp = do
  (myScenario, _gsi) <- loadStandaloneScenario fp
  let sLandscape = myScenario ^. scenarioLandscape
  theSeed <- sendIO $ arbitrateSeed maybeSeed sLandscape

  let worldTuples = buildWorldTuples sLandscape
      myLandscape = mkLandscape sLandscape worldTuples theSeed

      vc =
        view planar $
          determineStaticViewCenter sLandscape worldTuples

  return (getDisplayGrid vc sLandscape myLandscape maybeSize, sLandscape ^. scenarioCosmetics)

doRenderCmd :: RenderOpts -> FilePath -> IO ()
doRenderCmd opts@(RenderOpts _ asPng _ _ _) mapPath =
  case asPng of
    ConsoleText -> printScenarioMap =<< renderScenarioMap opts mapPath
    PngImage -> renderScenarioPng opts mapPath

renderScenarioMap :: RenderOpts -> FilePath -> IO [String]
renderScenarioMap opts fp = simpleErrorHandle $ do
  (grid, _) <- getRenderableGrid opts fp
  return $ getRows $ getDisplayChar <$> grid

renderScenarioPng :: RenderOpts -> FilePath -> IO ()
renderScenarioPng opts fp = do
  result <- runThrow $ getRenderableGrid opts fp
  img <- case result of
    Left (err :: SystemFailure) -> case failureMode opts of
      Terminate -> fail errorMsg
      RenderBlankImage -> do
        hPutStrLn stderr errorMsg
        let s = maybe (1, 1) (both fromIntegral . asTuple) $ gridSize opts
        return $ uncurry (generateImage $ \_x _y -> PixelRGBA8 0 0 0 255) s
     where
      errorMsg :: String
      errorMsg = prettyString err
    Right (grid, aMap) -> return $ (makeImage . getDisplayColor) aMap grid
  writePng (outputFilepath opts) img

printScenarioMap :: [String] -> IO ()
printScenarioMap =
  sendIO . mapM_ putStrLn
