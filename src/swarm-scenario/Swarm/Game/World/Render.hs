-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- GameState- and TUI-independent world rendering.
module Swarm.Game.World.Render (
  FailureMode (..),
  RenderOpts (..),
  RenderComputationContext (..),
  OuputFormat (..),
  ColorableCell,
  getDisplayGrid,
  doRenderCmd,
  getRenderableGrid,
  renderImage,
) where

import Codec.Picture
import Control.Applicative ((<|>))
import Control.Carrier.Error.Either (runError)
import Control.Effect.Error
import Control.Effect.Lift (Lift, sendIO)
import Control.Lens (view, (^.))
import Control.Monad.Extra (guarded)
import Control.Monad.Logger
import Control.Monad.Trans (MonadIO)
import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (both)
import GHC.Generics (Generic)
import Linear (V2 (..))
import Swarm.Failure (SystemFailure, simpleErrorHandle)
import Swarm.Game.Cosmetic.Color (AttributeMap)
import Swarm.Game.Cosmetic.Display (defaultChar)
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
import Swarm.Language.Syntax (Phase (..))
import Swarm.Pretty (prettyText)
import Swarm.Util (failT)
import Swarm.Util.Content
import Swarm.Util.Erasable (erasableToMaybe)
import Swarm.Util.Yaml

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

data RenderComputationContext = RenderComputationContext
  { _renderSeed :: Maybe Seed
  , gridSize :: Maybe AreaDimensions
  }

-- | Command-line options for configuring the app.
data RenderOpts = RenderOpts
  { renderComputation :: RenderComputationContext
  -- ^ Explicit seed chosen by the user.
  , outputFormat :: OuputFormat
  , outputFilepath :: FilePath
  , failureMode :: FailureMode
  }

getCellChar :: PCell EntityFacade phase -> Char
getCellChar = maybe ' ' facadeChar . erasableToMaybe . cellEntity
 where
  facadeChar (EntityFacade _ d _) = d ^. defaultChar

-- | When output size is not explicitly provided,
-- uses natural map bounds (if a map exists).
getBoundingBox ::
  Location ->
  PWorldDescription e phase ->
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
      maybeSize <|> guarded (not . isEmpty) mapAreaDims

getDisplayGrid ::
  Location ->
  ScenarioLandscape phase ->
  Landscape phase ->
  Maybe AreaDimensions ->
  Grid (CellPaintDisplay phase)
getDisplayGrid vc sLandscape ls maybeSize =
  getMapRectangle
    mkFacade
    (getContentAt (sLandscape ^. scenarioTerrainAndEntities . terrainMap) worlds . mkCosmic)
    (getBoundingBox vc firstScenarioWorld maybeSize)
 where
  mkCosmic = Cosmic $ worldName firstScenarioWorld
  worlds = view multiWorld ls

  firstScenarioWorld = NE.head $ view scenarioWorlds sLandscape

getRenderableGridFromPath ::
  (Has (Error SystemFailure) sig m, Has (Lift IO) sig m) =>
  RenderOpts ->
  FilePath ->
  m (ThumbnailRenderContext Elaborated)
getRenderableGridFromPath (RenderOpts ctx _ _ _) fp = do
  (myScenario, _gsi) <- loadStandaloneScenario fp
  getRenderableGrid ctx myScenario

getRenderableGrid ::
  Has (Lift IO) sig m =>
  RenderComputationContext ->
  Scenario Elaborated ->
  m (ThumbnailRenderContext Elaborated)
getRenderableGrid (RenderComputationContext maybeSeed maybeSize) myScenario = do
  let sLandscape = myScenario ^. scenarioLandscape
  theSeed <- sendIO $ arbitrateSeed maybeSeed sLandscape

  let worldTuples = buildWorldTuples sLandscape
      myLandscape = mkLandscape sLandscape worldTuples theSeed

      vc =
        view planar $
          determineStaticViewCenter sLandscape worldTuples

  return $
    ThumbnailRenderContext
      (getDisplayGrid vc sLandscape myLandscape maybeSize)
      (sLandscape ^. scenarioCosmetics)

doRenderCmd :: RenderOpts -> FilePath -> IO ()
doRenderCmd opts@(RenderOpts _ asPng _ _) mapPath =
  case asPng of
    ConsoleText -> printScenarioMap =<< renderScenarioMap opts mapPath
    PngImage -> renderScenarioPng opts mapPath

renderScenarioMap :: RenderOpts -> FilePath -> IO [String]
renderScenarioMap opts fp = simpleErrorHandle $ do
  ThumbnailRenderContext grid _ <- getRenderableGridFromPath opts fp
  return $ getRows $ getCellChar <$> grid

data ThumbnailRenderContext phase
  = ThumbnailRenderContext
      (Grid (PCell EntityFacade phase))
      AttributeMap

renderImage ::
  ThumbnailRenderContext phase ->
  Image PixelRGBA8
renderImage (ThumbnailRenderContext grid aMap) =
  makeImage $ getTerrainEntityColor aMap <$> grid

renderImageHandleFailure ::
  (MonadFail m, MonadIO m) =>
  RenderOpts ->
  Either SystemFailure (ThumbnailRenderContext phase) ->
  LoggingT m (Image PixelRGBA8)
renderImageHandleFailure opts result =
  case result of
    Left err -> handleFailure err
    Right ctx -> return $ renderImage ctx
 where
  handleFailure err = case failureMode opts of
    Terminate -> failT $ pure errorMsg
    RenderBlankImage -> do
      logWarnN errorMsg
      let s = maybe (1, 1) (both fromIntegral . asTuple) $ gridSize $ renderComputation opts
      return $ uncurry (generateImage $ \_x _y -> PixelRGBA8 0 0 0 255) s
   where
    errorMsg = prettyText err

renderScenarioPng :: RenderOpts -> FilePath -> IO ()
renderScenarioPng opts fp = do
  result <- runError $ getRenderableGridFromPath opts fp
  img <- runStderrLoggingT $ renderImageHandleFailure opts result
  writePng (outputFilepath opts) img

printScenarioMap :: [String] -> IO ()
printScenarioMap = sendIO . mapM_ putStrLn
