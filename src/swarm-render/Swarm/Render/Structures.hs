{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Render.Structures where

import Control.Carrier.Throw.Either
import Control.Effect.Lift
import Control.Lens ((^.))
import Control.Monad (forM_)
import Data.ByteString.Lazy qualified as LBS
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Swarm.Failure (SystemFailure)
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Topography.Cell (Cell)
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Scenario.Topography.Structure.Assembly
import Swarm.Game.Scenario.Topography.Structure.Named
import Swarm.Game.Scenario.Topography.WorldPalette
import Swarm.Render.Image
import Swarm.Util.Content (getTerrainEntityColor)
import System.FilePath
import Text.Dot

renderStructuresGraph ::
  M.Map k (NamedStructure a) ->
  Dot ()
renderStructuresGraph sMap =
  nlg
 where
  gEdges = makeGraphEdges $ M.elems sMap

  edgeLookup = M.fromList $ map (\x@(_, b, _) -> (b, x)) gEdges
  nlg =
    netlistGraph
      (\k -> maybe mempty mkAttrs $ M.lookup k edgeLookup)
      (\k -> maybe mempty (\(_, _, c) -> c) $ M.lookup k edgeLookup)
      ([(a, a) | (_, a, _) <- gEdges])

  mkAttrs (_, b, _) =
    [ ("label", sname)
    , ("height", "1")
    , ("image", imgPath)
    , ("shape", "box")
    , ("style", "filled")
    , ("penwidth", "0")
    , ("fillcolor", "#b0b0b0:#f0f0f0")
    , ("imagepos", "tc")
    , ("labelloc", "b")
    ]
   where
    imgPath = sname <.> "png"
    sname = T.unpack $ getStructureName b

renderImages ::
  ImgRendering ->
  FilePath ->
  Map WorldAttr PreservableColor ->
  Map StructureName (NamedArea (PStructure (Maybe Cell))) ->
  IO ()
renderImages imgRendering outputFolder aMap sMap = do
  forM_ (M.toList modifiedMap) $ \(StructureName n, parentStruct) -> do
    let fp = outputFolder </> T.unpack n <.> "png"
        encodedImgBytestring = mkStructurePng imgRendering modifiedMap $ structure parentStruct
    LBS.writeFile fp encodedImgBytestring
 where
  modifiedMap = M.map ((fmap . fmap . fmap) (getTerrainEntityColor aMap . toCellPaintDisplay)) sMap

doRenderStructures ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  FilePath ->
  FilePath ->
  m ()
doRenderStructures scenarioFilepath outputFilepath = do
  (myScenario, _gsi) <- loadStandaloneScenario scenarioFilepath
  -- sendIO $ print $ myScenario ^. scenarioMetadata . scenarioName

  let sMap = myScenario ^. scenarioDiagnostic . scenarioStructureMap
      aMap = myScenario ^. scenarioLandscape . scenarioCosmetics
      imgOutputFolder = "blarg"

      sGraph = do
        renderStructuresGraph sMap

  -- attribute ("imagepath", imgOutputFolder)

  sendIO $ do
    renderImages (ImgRendering 8 DiagonalIndicators) imgOutputFolder aMap sMap
    writeFile outputFilepath $ showDot sGraph
