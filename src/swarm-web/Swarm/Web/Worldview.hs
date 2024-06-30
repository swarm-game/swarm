-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Render game world for web display
module Swarm.Web.Worldview where

import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import Data.Colour.Palette.BrewerSet (Kolor)
import Data.Colour.SRGB (RGB (..), sRGB24, sRGB24show)
import Data.IntMap qualified as IM
import Data.Text qualified as T
import GHC.Generics (Generic)
import Servant.Docs qualified as SD
import Swarm.Game.Entity.Cosmetic (RGBColor, flattenBg)
import Swarm.Game.Scenario (Scenario, scenarioCosmetics, scenarioLandscape)
import Swarm.Game.Scenario.Style
import Swarm.Game.Scenario.Topography.Area (AreaDimensions (..))
import Swarm.Game.Scenario.Topography.Grid (Grid)
import Swarm.Game.State (GameState, landscape, robotInfo)
import Swarm.Game.State.Robot (viewCenter)
import Swarm.Game.Universe (planar)
import Swarm.Game.World.Render
import Swarm.Util.Content (getTerrainEntityColor)
import Swarm.Util.OccurrenceEncoder

data GridResponse = GridResponse
  { isPlaying :: Bool
  , grid :: Maybe CellGrid
  }
  deriving (Generic, ToJSON)

getCellGrid ::
  Scenario ->
  GameState ->
  AreaDimensions ->
  CellGrid
getCellGrid myScenario gs requestedSize =
  CellGrid indexGrid encoding
 where
  vc = gs ^. robotInfo . viewCenter
  sLandscape = myScenario ^. scenarioLandscape
  dg = getDisplayGrid (vc ^. planar) sLandscape (gs ^. landscape) (Just requestedSize)
  aMap = sLandscape ^. scenarioCosmetics

  asColour :: RGBColor -> Kolor
  asColour (RGB r g b) = sRGB24 r g b
  asHex = HexColor . T.pack . sRGB24show . asColour

  f = asHex . maybe (RGB 0 0 0) (flattenBg . fromHiFi) . getTerrainEntityColor aMap
  (indexGrid, encoding) = runEncoder $ f <$> dg

data CellGrid = CellGrid
  { coords :: Grid IM.Key
  , colors :: [HexColor]
  }
  deriving (Generic, ToJSON)

instance SD.ToSample GridResponse where
  toSamples _ = SD.noSamples
