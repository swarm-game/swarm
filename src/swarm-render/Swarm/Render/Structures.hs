{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Render.Structures where

import Codec.Picture as JP
import Control.Carrier.Throw.Either
import Control.Effect.Lift
import Data.Foldable (foldl')
import Data.GraphViz
import Data.GraphViz.Attributes.Complete as GVA
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Diagrams.Backend.Rasterific
import Diagrams.Prelude hiding (p2)
import Diagrams.TwoD.GraphViz
import Diagrams.TwoD.Image
import Swarm.Failure (SystemFailure)
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Topography.Cell (Cell)
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Scenario.Topography.Structure.Assembly
import Swarm.Game.Scenario.Topography.Structure.Named
import Swarm.Game.Scenario.Topography.Structure.Named (getStructureName)
import Swarm.Game.Scenario.Topography.WorldPalette
import Swarm.Render.Image
import Swarm.Util
import Swarm.Util.Content (getTerrainEntityColor)

instance IsName StructureName

renderStructuresGraph ::
  ImgRendering ->
  Map StructureName (NamedStructure (Maybe PreservableColor)) ->
  IO (Diagram B)
renderStructuresGraph imgRendering sMap = do
  g' <- layoutGraph' params Dot g

  putStrLn . LT.unpack . printDotGraph $ graphToDot params g
  let drawing =
        drawGraph
          (place . maybe mempty fst . (`M.lookup` nodeDiagrams))
          (\_ _ _ _ _ _ -> mempty)
          g'

      drawingWithEdges = foldl' (\d (n1, n2) -> connectOutside n1 n2 d) drawing edgeList

  -- mapM_ (print . snd) $ M.elems nodeDiagrams

  return $ drawingWithEdges # frame 1
 where
  params :: GraphvizParams Int StructureName e () StructureName
  params =
    defaultDiaParams
      { fmtEdge = const [arrowTo noArrow]
      , fmtNode = nodeFmt
      }

  nodeFmt (_, s) = maybe mempty getSize . (`M.lookup` nodeDiagrams) $ s
   where
    getSize = f . snd

    -- Image dimensions in terms of pixels are manipulated.
    --
    -- Explicit width/height are expected to be provided in inches.
    -- Internally, they are then multiplied by 72
    -- (see https://gitlab.com/graphviz/graphviz/-/blob/main/lib/sparse/DotIO.c#L482 )
    -- to obtain a dimension in units of "points".
    --
    -- Conversely, if an "image" attribute is supplied to a node,
    -- its width and height in pixels is actually scaled to an
    -- internal value by a factor of (72 / 96 = 0.75), where 96 is a configurable DPI
    -- at the graph level (see https://graphviz.org/docs/attrs/dpi/ ),
    -- https://gitlab.com/graphviz/graphviz/-/blob/main/lib/gvc/gvusershape.c#L752-753
    -- https://gitlab.com/graphviz/graphviz/-/blob/main/cmake/config_checks.cmake#L74
    --
    -- Therefore, if one wants to use an explicit width and height replicate an identical
    -- behavior of the "image" property, one must take the pixel dimensions of the image
    -- and divide them by 96 for use as the "height" and "width" properties.

    f (V2 w h) =
      [ GVA.Shape GVA.BoxShape
      , -- , FixedSize GrowAsNeeded
        FixedSize SetNodeSize
      , Width $ fromIntegral w / 96
      , Height $ fromIntegral h / 96
      , GVA.Label $ GVA.StrLabel ""
      -- , GVA.Image $ LT.fromStrict $ "blarg/" <> getStructureName s <> ".png"
      ]

  nodeDiagrams = M.fromList $ map (\n -> (n, drawNode n)) nodeList
  drawNode n =
    (d, b)
   where
    d =
      vsep
        5
        [ boxThing # named n
        , scale 15 . text . T.unpack $ nameText
        ]
    -- boxThing = roundedRect 30 15 2 <> structureThumbnail
    boxThing = fst structureThumbnail
    -- b = boxExtents $ boundingBox boxThing
    b = snd structureThumbnail

    nameText = getStructureName n
    structureThumbnail = maybe (defaultDiagram, V2 1 1) getImg $ M.lookup n sMap

    defaultDiagram =
      scale 10 $
        vsep
          1
          [ text "Not found"
          , text "World"
          ]

  getImg x = (scale 0.75 . image . embeddedImage . ImageRGBA8 $ i, V2 w h)
   where
    i@(JP.Image w h _) = genStructureImage imgRendering sMap x

  gEdges = makeGraphEdges $ M.elems sMap

  edgeList = [(m, n) | (_, n, neighbors) <- gEdges, m <- neighbors]
  nodeList = [a | (_, a, _) <- gEdges]
  g =
    mkGraph
      nodeList
      [(m, n, ()) | (m, n) <- edgeList]

genStructureImage ::
  ImgRendering ->
  Map StructureName (NamedArea (PStructure (Maybe PreservableColor))) ->
  NamedArea (PStructure (Maybe PreservableColor)) ->
  Image PixelRGBA8
genStructureImage imgRendering modifiedMap s =
  mkStructureImage imgRendering modifiedMap $ structure s

applyStructureColors ::
  Map WorldAttr PreservableColor ->
  NamedArea (PStructure (Maybe Cell)) ->
  NamedArea (PStructure (Maybe PreservableColor))
applyStructureColors aMap =
  (fmap . fmap) (getTerrainEntityColor aMap . toCellPaintDisplay =<<)

doRenderStructures ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  FilePath ->
  FilePath ->
  m ()
doRenderStructures scenarioFilepath outputFilepath = do
  (scenario, _) <- loadStandaloneScenario scenarioFilepath

  let sMap = scenario ^. scenarioDiagnostic . scenarioStructureMap
      aMap = scenario ^. scenarioLandscape . scenarioCosmetics

  sendIO $ do
    g <-
      renderStructuresGraph (ImgRendering 8 DiagonalIndicators) $
        M.map (applyStructureColors aMap) sMap
    putStrLn $ "Rendering to path: " ++ outputFilepath
    renderRasterific outputFilepath (mkWidth 1600) g
    putStrLn "Finished rendering."
