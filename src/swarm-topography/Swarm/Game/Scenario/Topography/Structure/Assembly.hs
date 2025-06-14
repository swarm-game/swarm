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
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (singleton, uncons)
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
import Swarm.Util (brackets, commaList, quote)

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
  { pathPlacements :: [PathPlacement]
  , namedStructure :: NamedStructure a
  }

-- | This function constructs a graph from the base structure.
--   The PathToRoot of the base structure is the empty list.
--   The nodes in the graph are ''AnnotatedStructure'\'s. Each node is uniquely identified by the
--   path of structure names from the structure it contains to the root. Each node contains its list of edges.
mkGraph ::
  forall a.
  PStructure (Maybe a) ->
  Either Text (HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)))
mkGraph baseStructure = go True mempty [] mempty baseNamed
 where
  go ::
    Bool ->
    -- Knowledge inherited from parent, allows us to find the full path for a placement
    HM.HashMap StructureName PathToRoot ->
    -- Path from parent to root
    [StructureName] ->
    HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)) ->
    NamedStructure (Maybe a) ->
    Either Text (HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)))
  go isBase inheritedKnowledge parentToRootPath !acc !namedStruct = do
    let struct = structure namedStruct
        substructures = structures struct
        structName = name namedStruct
        structPlacements = placements struct
        structPath = if isBase then [] else structName : parentToRootPath
        knowledgeOfChildren = HM.fromList $ map ((id &&& (: structPath)) . name) substructures
        -- Deeper structure definitions supersede more shallow ones, so we pass knowledgeOfChildren first as HM.union is left-biased.
        knowledge = HM.union knowledgeOfChildren inheritedKnowledge
        f :: Placement -> Either Text PathPlacement
        f placement = case HM.lookup (src placement) knowledge of
          Nothing ->
            Left $
              T.unwords
                ["Within", showPath structPath <> ":", "Could not look up structure", quote . getStructureName . src $ placement]
          Just path -> pure $ PathPlacement path (structurePose placement)

    structPathPlacements <- traverse f structPlacements
    let annotatedStruct = AnnotatedStructure structPathPlacements namedStruct
        !acc' = HM.insert structPath annotatedStruct acc
    foldlM (go False knowledge structPath) acc' substructures
  baseNamed = NamedArea (StructureName "") mempty Nothing baseStructure

data DFSPath = DFSPath (HS.HashSet PathToRoot) [PathToRoot]
data DFSState = DFSState (HS.HashSet PathToRoot) [PathToRoot]

addToDFSPath :: PathToRoot -> DFSPath -> DFSPath
addToDFSPath pathToRoot (DFSPath pathElems back) = DFSPath (HS.insert pathToRoot pathElems) (pathToRoot : back)

basePathToRoot :: PathToRoot
basePathToRoot = []

cycleError :: PathToRoot -> [PathToRoot] -> Text
cycleError path dfsPath = T.unwords ["Structure graph contains a cycle:", cycleText]
 where
  cyc = (path :) . reverse . (path :) . takeWhile (/= path) $ dfsPath
  cycleText = brackets . T.intercalate " -> " . fmap showPath $ cyc

-- | Given a graph constructed via 'mkGraph', this function does a dfs on the graph to find
--   any cycles in the graph that exist. If such a cycle exists, an appropriate error message is returned (in 'Left').
--   If there are no cycles, what is instead returned is a topologically sorted list of all the identifiers in the graph (in 'Right').
--   The list is sorted such that if structure A places structure B, the identifier for structure B precedes the identifier for structure A.
topSortGraph :: HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)) -> Either Text [PathToRoot]
topSortGraph graph = fmap (reverse . getAcc) . foldlM (go emptyPath) acc0 $ HM.toList graph
 where
  go :: DFSPath -> DFSState -> (PathToRoot, AnnotatedStructure (Maybe a)) -> Either Text DFSState
  go dfsPathOfParent@(DFSPath parentPathMembers parentPath) acc@(DFSState visited _) (!pathToRoot, !annotatedStruct) = do
    if pathToRoot `HS.member` visited
      then pure acc
      else do
        when (pathToRoot `HS.member` parentPathMembers) $ Left $ cycleError pathToRoot parentPath
        let dfsPath = addToDFSPath pathToRoot dfsPathOfParent
            placementPaths = map pathToPlacement $ pathPlacements annotatedStruct
            f acc' path = case HM.lookup path graph of
              Nothing -> Left $ T.unwords ["Could not find structure", showPath path, "in topological sort of graph"] -- As long as mkGraph is correct, this path should not be taken
              Just annotated -> go dfsPath acc' (path, annotated)
        DFSState visited' topSortAcc' <- foldlM f acc placementPaths
        pure $ DFSState (HS.insert pathToRoot visited') (pathToRoot : topSortAcc')
  emptyPath = DFSPath HS.empty []
  acc0 = DFSState HS.empty []
  getAcc (DFSState _ acc) = acc

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

-- | Given a graph constructed via 'mkGraph' and a topologically sorted listed of identifiers constructed via 'topSortGraph',
--   this function assembles all the structures in the graph, taking care to avoid redundant work. The assembled structure for
--   an identifier can be found by looking up the identifier in the HashMap that this function returns.
mergeStructures :: forall a. HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)) -> [PathToRoot] -> Either Text (HM.HashMap PathToRoot (MergedStructure (Maybe a)))
mergeStructures graph = foldlM go mempty
 where
  -- This will not be called unless the graph and topological sort are incorrect
  mkCouldNotFindError path = T.unwords ["Could not look up structure", showPath path, "in mergeStructure"]
  lookupHandling m path f = case HM.lookup path m of
    Nothing -> Left $ mkCouldNotFindError path
    Just x -> pure (f x)

  validatePlacements path toPlace = do
    result <- traverse (\(PathPlacement p pose) -> lookupHandling graph p (,pose)) toPlace
    let result' = map (first namedStructure) result
    left (elaboratePlacement path <>) $ traverse_ (uncurry validatePlacement) result'
    pure result

  placementToLocated (Placement sn pose) = LocatedStructure (OrientedStructure sn (up $ orient pose)) (offset pose)

  computeInitialOverlays = map (placementToLocated . uncurry Placement . first (name . namedStructure)) . filter (isRecognizable . namedStructure . fst)

  go :: HM.HashMap PathToRoot (MergedStructure (Maybe a)) -> PathToRoot -> Either Text (HM.HashMap PathToRoot (MergedStructure (Maybe a)))
  go !alreadyMerged !path = do
    annotatedStruct <- lookupHandling graph path id
    let toPlace = pathPlacements annotatedStruct
    toPlaceAnnotated <- validatePlacements path toPlace
    let f (PathPlacement pathForPlacement pose) = lookupHandling alreadyMerged pathForPlacement (,pose)
    mergedToPlace <- traverse f toPlace
    let parentage = maybe Root (WithParent . fst) (uncons path)
        struct = structure . namedStructure $ annotatedStruct
        origArea = area struct
        initialWaypoints = map (Originated parentage) . waypoints $ struct
        initialOverlays = computeInitialOverlays toPlaceAnnotated
        initialMerged = MergedStructure origArea initialOverlays initialWaypoints
        merged = foldl' overlaySingleStructure initialMerged mergedToPlace
    pure $ HM.insert path merged alreadyMerged

-- | Given a base structure, this function returns the HashMap of merged structures (in 'Right').
--   If the input is invalid, this functions instead returns an appropriate error message (in 'Left').
assembleStructure' ::
  PStructure (Maybe a) ->
  Either Text (HM.HashMap PathToRoot (MergedStructure (Maybe a)))
assembleStructure' baseStructure = do
  graph <- mkGraph baseStructure
  topSorted <- topSortGraph graph
  !mergedMap <- mergeStructures graph topSorted
  pure mergedMap

-- | Assembles the given base structure into a 'MergedStructure'
assembleStructure ::
  PStructure (Maybe a) ->
  Either Text (MergedStructure (Maybe a))
assembleStructure baseStructure = do
  mergedMap <- assembleStructure' baseStructure
  case HM.lookup basePathToRoot mergedMap of
    Nothing -> Left "Unable to find root structure in graph" -- This path this should not be taken unless the implementations of mkGraph, topSortGraph, mergedStructures, and assembleStructure' are correct
    Just merged -> pure merged

packageStructures :: [NamedStructure a] -> PStructure a
packageStructures namedStructs = Structure (PositionedGrid origin EmptyGrid) namedStructs [] []

-- | Assembles the list of named structures into a list of merged structures paired with their corresponding named structures
assembleStructures ::
  [NamedStructure (Maybe a)] ->
  Either Text [(NamedStructure (Maybe a), MergedStructure (Maybe a))]
assembleStructures namedStructs = do
  mergedMap <- assembleStructure' (packageStructures namedStructs)
  let f !namedStruct = do
        let path = singleton . name $ namedStruct
        !merged <- case HM.lookup path mergedMap of
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
