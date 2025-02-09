{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Render.Structures where

import Codec.Picture as JP
import Control.Carrier.Throw.Either
import Control.Effect.Lift
import Data.GraphViz (GraphvizParams (..))
import Data.GraphViz qualified as GV
import Data.GraphViz.Attributes.Complete as GVA
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Diagrams.Backend.Rasterific
import Diagrams.Prelude as DP hiding (p2)
import Diagrams.TwoD.GraphViz
import Diagrams.TwoD.Image
import Swarm.Failure (SystemFailure)
import Swarm.Game.Entity.Cosmetic
import Swarm.Game.Scenario
import Swarm.Game.Scenario.Topography.Cell (Cell)
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Scenario.Topography.Structure.Assembly
import Swarm.Game.Scenario.Topography.Structure.Named
import Swarm.Game.Scenario.Topography.WorldDescription
import Swarm.Game.Scenario.Topography.WorldPalette
import Swarm.Render.Image
import Swarm.Util.Content (getTerrainEntityColor)

instance IsName StructureName

renderStructuresGraph ::
  ImgRendering ->
  Map StructureName (NamedStructure (Maybe PreservableColor)) ->
  IO (Diagram B)
renderStructuresGraph imgRendering sMap = do
  g' <- layoutGraph' params Dot g

  putStrLn "Structure keys:"
  print $ M.keys sMap
  putStrLn "Edge list:"
  print edgeList

  putStrLn "------"
  putStrLn . LT.unpack . GV.printDotGraph $ GV.graphToDot params g
  let drawing =
        drawGraph
          (place . maybe mempty (scale 0.75 . fst) . (`M.lookup` nodeDiagrams))
          -- (\_ p1 _ p2 _ p -> arrowBetween' (opts p) p1 p2 # lc DP.blue # opacity 0.8)
          (\_ _ _ _ _ _ -> mempty)
          g'
      -- opts p = with & gaps .~ 16 & arrowShaft .~ (unLoc . head $ pathTrails p)

      -- Ignores any graphviz-generated arrow path, and instead uses
      -- connectors from `diagrams`.
      drawingWithEdges = foldr (\(n1, n2) d -> connectOutsideBeneath n1 n2 d) drawing edgeList
      finalDrawing = drawingWithEdges # lw 5 # lcA (DP.darkslateblue `withOpacity` 0.8)

  return $ finalDrawing # frame 5
 where
  params :: GraphvizParams Int StructureName e () StructureName
  params =
    GV.defaultParams
      { globalAttributes =
          [ GV.NodeAttrs
              [ GV.shape GVA.BoxShape
              , GVA.Label $ GVA.StrLabel ""
              -- , FixedSize SetNodeSize
              ]
          , GV.GraphAttrs
              [ Overlap ScaleOverlaps
              , Splines SplineEdges
              -- , DPI 96
              ]
          ]
      , -- , fmtEdge = const [arrowTo noArrow]
        fmtNode = nodeFmt
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
      [ Width $ w / 96
      , Height $ h / 96
      -- , GVA.Image $ LT.fromStrict $ "blarg/" <> getStructureName s <> ".png"
      ]

  nodeDiagrams = M.fromList $ map (\n -> (n, drawNode n)) nodeList

  -- The diagram shall consist of
  -- parts that are considered in the graphviz layout and parts that are not.
  -- In particular, the label text underneath the image thumbnail is not
  -- included in the bounding box measurement for layout.
  drawNode n =
    (d, b)
   where
    d =
      vsep
        15
        [ measuredBox # named n
        , scale 15 $ text (T.unpack nameText)
        ]

    measuredBox = structureThumbnail

    b = boxExtents $ boundingBox measuredBox

    nameText = getStructureName n
    structureThumbnail = maybe defaultDiagram getImg $ M.lookup n sMap

    defaultDiagram =
      scale 10 $
        vsep
          1
          [ text "Not found"
          , text "World"
          ]

  getImg x = image . embeddedImage . ImageRGBA8 $ i
   where
    i = genStructureImage imgRendering sMap x

  gEdges = makeStructureGraphEdges $ M.elems sMap

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
      sMapWorld =
        view worldStructureMap . worldDiagnostic . NE.head $
          scenario ^. scenarioLandscape . scenarioWorlds
      aMap = scenario ^. scenarioLandscape . scenarioCosmetics

  sendIO $ do
    g <-
      renderStructuresGraph (ImgRendering 8 DiagonalIndicators) $
        M.map
          (applyStructureColors aMap)
          -- sMap
          sMapWorld
    putStrLn $ "Rendering to path: " ++ outputFilepath
    renderRasterific outputFilepath (mkWidth 2000) g
    putStrLn "Finished rendering."

-- * Utils

-- | Clone of 'connectOutside' but using 'beneath' instead of 'atop'
connectOutsideBeneath ::
  (TypeableFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2) =>
  n1 ->
  n2 ->
  QDiagram b V2 n Any ->
  QDiagram b V2 n Any
connectOutsideBeneath = connectOutsideBeneath' def

-- | Clone of 'connectOutside'' but using 'beneath' instead of 'atop'
connectOutsideBeneath' ::
  (TypeableFloat n, Renderable (Path V2 n) b, IsName n1, IsName n2) =>
  ArrowOpts n ->
  n1 ->
  n2 ->
  QDiagram b V2 n Any ->
  QDiagram b V2 n Any
connectOutsideBeneath' opts n1 n2 =
  withName n1 $ \b1 ->
    withName n2 $ \b2 ->
      let v = location b2 .-. location b1
          midpoint = location b1 .+^ (v ^/ 2)
          s' = fromMaybe (location b1) $ traceP midpoint (negated v) b1
          e' = fromMaybe (location b2) $ traceP midpoint v b2
       in beneath (arrowBetween' opts s' e')
