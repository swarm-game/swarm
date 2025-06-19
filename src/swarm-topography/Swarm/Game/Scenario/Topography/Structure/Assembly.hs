{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definitions of "structures" for use within a map
-- as well as logic for combining them.
module Swarm.Game.Scenario.Topography.Structure.Assembly (
  assembleStructure,
  assembleStructures,

  -- * Exposed for unit tests:
  foldLayer,
)
where

import Control.Arrow (first, left, (&&&))
import Control.Monad (when)
import Data.Coerce
import Data.Foldable (foldl', foldlM, traverse_)
import Data.List (singleton, uncons)
import Data.Map.Lazy qualified as ML
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Linear.Affine
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Grid (Grid (..))
import Swarm.Game.Scenario.Topography.Navigation.Waypoint
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Scenario.Topography.Structure.Named
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Swarm.Game.Scenario.Topography.Structure.Recognition.Static
import Swarm.Language.Syntax.Direction (directionJsonModifier)
import Swarm.Util (commaList, quote)
import Swarm.Util.Graph (failOnCyclicGraph)

-- | /Uniquely/ identifies a structure in the graph
type PathToRoot = [StructureName]

-- | Converts a path to the root into a fully qualified name
showPath :: PathToRoot -> Text
showPath [] = "root structure"
showPath xs = T.intercalate "." . coerce . reverse $ xs

-- | Like placement, but instead of storing the name of the structure to place,
--   stores the path of names from that structure up to the root.
--   This allows us disambiguate different structures which share the same name.
data PathPlacement = PathPlacement
  { pathToPlacement :: PathToRoot
  , placementPose :: Pose
  }

-- | This augments a named structure with its list of edges (placements). PathPlacement is used to allow us to disambiguate
--   different structures which share the same name.
data AnnotatedStructure a = AnnotatedStructure
  { annPathToRoot :: PathToRoot
  , pathPlacements :: [PathPlacement]
  , namedStructure :: NamedStructure a
  }

-- | This function constructs a graph from the base structure.
--   The PathToRoot of the base structure is the empty list.
--   The nodes in the graph are ''AnnotatedStructure'\'s. Each node is uniquely identified by the
--   path of structure names from the structure it contains to the root. Each node contains its list of edges.
mkGraph ::
  forall a.
  PStructure (Maybe a) ->
  Either Text (M.Map PathToRoot (AnnotatedStructure (Maybe a)))
mkGraph baseStructure = go True mempty [] mempty baseNamed
 where
  go ::
    Bool ->
    -- Knowledge inherited from parent, allows us to find the full path for a placement
    M.Map StructureName PathToRoot ->
    -- Path from parent to root
    [StructureName] ->
    M.Map PathToRoot (AnnotatedStructure (Maybe a)) ->
    NamedStructure (Maybe a) ->
    Either Text (M.Map PathToRoot (AnnotatedStructure (Maybe a)))
  go isBase inheritedKnowledge parentToRootPath !acc !namedStruct = do
    let struct = structure namedStruct
        substructures = structures struct
        structName = name namedStruct
        structPlacements = placements struct
        structPath = if isBase then [] else structName : parentToRootPath
        knowledgeOfChildren = M.fromList $ map ((id &&& (: structPath)) . name) substructures
        -- Deeper structure definitions supersede more shallow ones, so we pass knowledgeOfChildren first as HM.union is left-biased.
        knowledge = M.union knowledgeOfChildren inheritedKnowledge
        f placement = case M.lookup (src placement) knowledge of
          Nothing ->
            Left $
              T.unwords
                ["Within", showPath structPath <> ":", "Could not look up structure", quote . getStructureName . src $ placement]
          Just path -> pure $ PathPlacement path (structurePose placement)

    structPathPlacements <- traverse f structPlacements
    let annotatedStruct = AnnotatedStructure structPath structPathPlacements namedStruct
        !acc' = M.insert structPath annotatedStruct acc
    foldlM (go False knowledge structPath) acc' substructures
  baseNamed = NamedArea (StructureName "") mempty Nothing baseStructure

basePathToRoot :: PathToRoot
basePathToRoot = []

-- | Destructively overlays one already-merged direct child structure
--   upon the input merged structure.
overlaySingleStructure ::
  MergedStructure (Maybe a) ->
  (MergedStructure (Maybe a), Pose) ->
  MergedStructure (Maybe a)
overlaySingleStructure (MergedStructure inputArea inputPlacements inputWaypoints) (MergedStructure overlayArea overlayPlacements overlayWaypoints, pose@(Pose loc orientation)) = MergedStructure mergedArea mergedPlacements mergedWaypoints
 where
  mergedWaypoints = inputWaypoints <> map (fmap $ placeOnArea overlayArea) overlayWaypoints
  mergedPlacements = inputPlacements <> map (placeOnArea overlayArea) overlayPlacements
  mergedArea = overlayGridExpanded inputArea pose overlayArea
  placeOnArea (PositionedGrid _ overArea) =
    offsetLoc (coerce loc)
      . modifyLoc (reorientLandmark orientation $ getGridDimensions overArea)

-- | Given the structure graph, ensure that the graph is not acyclic and that all placements are valid
validateGraph :: M.Map PathToRoot (AnnotatedStructure (Maybe a)) -> Either Text (M.Map PathToRoot (AnnotatedStructure (Maybe a)))
validateGraph graph = do
  validated <- M.traverseWithKey go graph
  failOnCyclicGraph "Structure" (showPath . annPathToRoot) [(ann, path, map pathToPlacement (pathPlacements ann)) | (path, ann) <- M.toList validated]
  pure validated
 where
  go :: PathToRoot -> AnnotatedStructure (Maybe a) -> Either Text (AnnotatedStructure (Maybe a))
  go path ann = do
    let toPlace = pathPlacements ann
        f (PathPlacement p pose) = (graph M.! p, pose)
    left (elaboratePlacement path <>) $ traverse_ (uncurry validatePlacement . first namedStructure . f) toPlace
    pure ann

-- | Given a graph constructed via 'mkGraph',this function assembles all the structures in the graph, taking care to avoid redundant work.
--   The assembled structure for an identifier can be found by looking up the identifier in the Map that this function returns.
mergeStructures :: M.Map PathToRoot (AnnotatedStructure (Maybe a)) -> M.Map PathToRoot (MergedStructure (Maybe a))
mergeStructures graph = M.fromList . ML.toList $ mergedMap
 where
  mergedMap = ML.fromList [(p, toMerged p ann) | (p, ann) <- M.toList graph]

  placementToLocated (Placement sn pose) = LocatedStructure (OrientedStructure sn (up $ orient pose)) (offset pose)
  computeInitialOverlays = map (placementToLocated . uncurry Placement . first (name . namedStructure)) . filter (isRecognizable . namedStructure . fst) . map (\(PathPlacement p pose) -> (graph M.! p, pose))

  toMerged path ann = foldl' overlaySingleStructure initialMerged toPlaceMerged
   where
    toPlace = pathPlacements ann
    toPlaceMerged = map (\(PathPlacement p pose) -> (mergedMap ML.! p, pose)) toPlace

    parentage = maybe Root (WithParent . fst) (uncons path)
    struct = structure . namedStructure $ ann

    origArea = area struct
    initialWaypoints = map (Originated parentage) . waypoints $ struct
    initialOverlays = computeInitialOverlays toPlace
    initialMerged = MergedStructure origArea initialOverlays initialWaypoints

-- | Given a base structure, this function returns the HashMap of merged structures (in 'Right').
--   If the input is invalid, this functions instead returns an appropriate error message (in 'Left').
assembleStructure' ::
  PStructure (Maybe a) ->
  Either Text (M.Map PathToRoot (MergedStructure (Maybe a)))
assembleStructure' baseStructure = do
  graph <- mkGraph baseStructure
  validatedGraph <- validateGraph graph
  let !mergedMap = mergeStructures validatedGraph
  pure mergedMap

-- | Assembles the given base structure into a 'MergedStructure'
assembleStructure ::
  PStructure (Maybe a) ->
  Either Text (MergedStructure (Maybe a))
assembleStructure baseStructure = do
  mergedMap <- assembleStructure' baseStructure
  case M.lookup basePathToRoot mergedMap of
    Nothing -> Left "Unable to find root structure in graph" -- This path this should not be taken unless there is a bug in the implementation
    Just merged -> pure merged

packageStructures :: [NamedStructure a] -> PStructure a
packageStructures namedStructs = Structure (PositionedGrid origin EmptyGrid) namedStructs [] []

-- | Assembles the list of named structures into a list of merged structures paired with their corresponding named structures
assembleStructures ::
  [NamedStructure (Maybe a)] ->
  Either Text [(NamedStructure (Maybe a), MergedStructure (Maybe a))]
assembleStructures namedStructs = do
  mergedMap <- assembleStructure' (packageStructures namedStructs)
  let f namedStruct = do
        let path = singleton . name $ namedStruct
        merged <- case M.lookup path mergedMap of
          Nothing -> Left $ T.unwords ["Unable to find structure", showPath path, "in collection of merged structures"]
          Just x -> pure x
        pure (namedStruct, merged)
  traverse f namedStructs

-- | For use in unit testing
foldLayer ::
  PositionedGrid (Maybe a) ->
  [Placed (Maybe a)] ->
  Either Text (MergedStructure (Maybe a))
foldLayer origArea overlays = assembleStructure struct
 where
  struct = Structure origArea (map getNamedStruct overlays) (map getPlacement overlays) []
  getNamedStruct (Placed _ ns) = ns
  getPlacement (Placed p _) = p

-- * Grid manipulation

overlayGridExpanded ::
  PositionedGrid (Maybe a) ->
  Pose ->
  PositionedGrid (Maybe a) ->
  PositionedGrid (Maybe a)
overlayGridExpanded
  baseGrid
  (Pose yamlPlacementOffset orientation)
  -- The 'childAdjustedOrigin' is the sum of origin adjustments
  -- to completely assemble some substructure.
  (PositionedGrid childAdjustedOrigin overlayArea) =
    baseGrid <> positionedOverlay
   where
    reorientedOverlayCells = applyOrientationTransform orientation overlayArea
    placementAdjustedByOrigin = childAdjustedOrigin .+^ asVector yamlPlacementOffset
    positionedOverlay = PositionedGrid placementAdjustedByOrigin reorientedOverlayCells

-- * Validation

elaboratePlacement :: PathToRoot -> Text
elaboratePlacement p =
  T.unwords
    [ "Within placement of"
    , showPath p <> ":"
    , ""
    ]

validatePlacement ::
  NamedStructure (Maybe a) ->
  Pose ->
  Either Text ()
validatePlacement ns (Pose _ orientation) = do
  let placementDirection = up orientation
      recognizedOrientations = recognize ns
      n = getStructureName . name $ ns

  when (isRecognizable ns) $ do
    when (flipped orientation) $
      Left $
        T.unwords
          [ "Placing recognizable structure"
          , quote n
          , "with flipped orientation is not supported."
          ]

    -- Redundant orientations by rotational symmetry are accounted
    -- for at scenario parse time
    when (Set.notMember placementDirection recognizedOrientations) $
      Left $
        T.unwords
          [ "Placing recognizable structure"
          , quote n
          , "with"
          , renderDir placementDirection
          , "orientation is not supported."
          , "Try"
          , commaList $ map renderDir $ Set.toList recognizedOrientations
          , "instead."
          ]
 where
  renderDir = quote . T.pack . directionJsonModifier . show
