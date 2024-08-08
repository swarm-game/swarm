-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- GameState- and TUI-independent world rendering.
module Swarm.Game.World.Render (
  FailureMode (..),
  RenderOpts (..),
  OuputFormat (..),
  ColorableCell,
  getDisplayGrid,
  doRenderCmd,
) where

import Codec.Picture
import Control.Applicative ((<|>))
import Control.Carrier.Throw.Either (runThrow)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Throw
import Control.Lens (view, (^.))
import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (both)
import GHC.Generics (Generic)
import Linear (V2 (..))
import Swarm.Game.Display (defaultChar)
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Failure (SystemFailure, simpleErrorHandle)
import Swarm.Game.Land
import Swarm.Game.Location
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Style (HexColor (..))
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Cell
import Swarm.Game.Scenario.Topography.Center
import Swarm.Game.Scenario.Topography.EntityFacade (EntityFacade (..), mkFacade)
import Swarm.Game.Scenario.Topography.Grid
import Swarm.Game.Scenario.Topography.Rasterize
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Swarm.Game.Scenario.Topography.WorldDescription
import Swarm.Game.State.Landscape
import Swarm.Game.Universe
import Swarm.Game.World.Coords
import Swarm.Game.World.Gen (Seed)
import Swarm.Language.Pretty (prettyString)
import Swarm.Util (surfaceEmpty)
import Swarm.Util.Content
import Swarm.Util.Erasable (erasableToMaybe)
import Swarm.Util.Yaml
import System.IO (hPutStrLn, stderr)

newtype OneBitColor = OneBitColor Bool
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance ToPixel OneBitColor where
  toPixel (OneBitColor b) = case b of
    False -> PixelRGBA8 0 0 0 255
    True -> PixelRGBA8 255 255 255 255

data ColorableCell
  = OneBit OneBitColor
  | Hex HexColor

instance ToPixel ColorableCell where
  toPixel (OneBit x) = toPixel x
  toPixel (Hex x) = toPixel x

instance FromJSON ColorableCell where
  parseJSON x =
    try OneBit
      <|> try Hex
   where
    try f = f <$> parseJSON x

instance FromJSONE e ColorableCell

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
    Right (grid, aMap) ->
      return $
        makeImage $
          getTerrainEntityColor aMap <$> grid
  writePng (outputFilepath opts) img

printScenarioMap :: [String] -> IO ()
printScenarioMap =
  sendIO . mapM_ putStrLn
