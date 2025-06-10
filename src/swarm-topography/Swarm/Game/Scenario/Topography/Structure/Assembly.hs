{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definitions of "structures" for use within a map
-- as well as logic for combining them.
module Swarm.Game.Scenario.Topography.Structure.Assembly (
  assembleStructure,
  assembleStructures,
  mergeStructures',

  -- * Exposed for unit tests:
  foldLayer,
)
where

import Control.Arrow (first, left, (&&&))
import Control.Monad (when)
import Data.Coerce
import Data.Either.Extra (maybeToEither)
import Data.Foldable (foldlM, traverse_)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List.NonEmpty qualified as NE
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

-- | Destructively overlays one direct child structure
-- upon the input structure.
-- However, the child structure is assembled recursively.
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

-- | Destructively overlays one direct child structure
-- upon the input structure.
-- However, the child structure is assembled recursively.
overlaySingleStructure' ::
  HM.HashMap StructureName (NamedStructure (Maybe a)) ->
  Placed (Maybe a) ->
  MergedStructure (Maybe a) ->
  Either Text (MergedStructure (Maybe a))
overlaySingleStructure'
  inheritedStrucDefs
  (Placed p@(Placement _sName pose@(Pose loc orientation)) ns)
  (MergedStructure inputArea inputPlacements inputWaypoints) = do
    MergedStructure overlayArea overlayPlacements overlayWaypoints <-
      mergeStructures' inheritedStrucDefs (WithParent (src p)) $ structure ns

    let mergedWaypoints = inputWaypoints <> map (fmap $ placeOnArea overlayArea) overlayWaypoints
        mergedPlacements = inputPlacements <> map (placeOnArea overlayArea) overlayPlacements
        mergedArea = overlayGridExpanded inputArea pose overlayArea

    return $ MergedStructure mergedArea mergedPlacements mergedWaypoints
   where
    placeOnArea (PositionedGrid _ overArea) =
      offsetLoc (coerce loc)
        . modifyLoc (reorientLandmark orientation $ getGridDimensions overArea)

makeStructureMap :: [NamedStructure a] -> HM.HashMap StructureName (NamedStructure a)
makeStructureMap = HM.fromList . map (name &&& id)

type GraphEdge a = (NamedStructure a, StructureName, [StructureName])

makeGraphEdges :: [NamedStructure a] -> [GraphEdge a]
makeGraphEdges =
  map makeGraphNodeWithEdges
 where
  makeGraphNodeWithEdges s =
    (s, name s, map src $ placements $ structure s)

-- | Overlays all of the "child placements", such that the children encountered later
-- in the YAML file supersede the earlier ones (dictated by using 'foldl' instead of 'foldr').
mergeStructures' ::
  HM.HashMap StructureName (NamedStructure (Maybe a)) ->
  Parentage StructureName ->
  PStructure (Maybe a) ->
  Either Text (MergedStructure (Maybe a))
mergeStructures' inheritedStrucDefs parentName baseStructure = do
  failOnCyclicGraph "Structure" (getStructureName . name) gEdges

  overlays <-
    left (elaboratePlacement' parentName <>) $
      mapM (validatePlacement' structureMap) subPlacements

  foldLayer structureMap origArea overlays originatedWaypoints
 where
  Structure origArea subStructures subPlacements subWaypoints = baseStructure

  originatedWaypoints = map (Originated parentName) subWaypoints

  -- deeper definitions override the outer (toplevel) ones
  structureMap = HM.union (makeStructureMap subStructures) inheritedStrucDefs
  gEdges = makeGraphEdges $ HM.elems structureMap

-- | NOTE: Each successive overlay may alter the coordinate origin.
-- We make sure this new origin is propagated to subsequent sibling placements.
foldLayer ::
  HM.HashMap StructureName (NamedStructure (Maybe a)) ->
  PositionedGrid (Maybe a) ->
  [Placed (Maybe a)] ->
  [Originated Waypoint] ->
  Either Text (MergedStructure (Maybe a))
foldLayer structureMap origArea overlays originatedWaypoints =
  foldlM
    (flip $ overlaySingleStructure' structureMap)
    (MergedStructure origArea wrappedOverlays originatedWaypoints)
    overlays
 where
  wrappedOverlays =
    map wrapPlacement $
      filter (\(Placed _ ns) -> isRecognizable ns) overlays

  wrapPlacement (Placed z ns) =
    LocatedStructure
      (OrientedStructure (name ns) (up $ orient structPose))
      (offset structPose)
   where
    structPose = structurePose z

type PathToRoot = NE.NonEmpty StructureName

-- | Converts a path to the root into a fully qualified name
-- >>> showPath $ NE.fromList [StructureName ""]
-- >>> showPath $ NE.fromList [StructureName "A", StructureName ""]
-- >>> showPath $ NE.fromList [StructureName "", StructureName ""]
-- >>> showPath $ NE.fromList [StructureName "AB", StructureName "A", StructureName ""]
-- "ROOT"
-- "A"
-- ""
-- "A.AB"
showPath :: PathToRoot -> Text
showPath (_ NE.:| []) = "ROOT"
showPath xs = T.intercalate "." . coerce . NE.tail . NE.reverse $ xs

-- | Like placement, but instead of storing the name of the structure to place, stores the path of names from that structure up to the root.
--   This allows us disambiguate different structures which share the same name.
data PathPlacement = PathPlacement
  { pathToPlacement :: PathToRoot
  , placementPose :: Pose
  }

-- | This essentially augments a named structure with its list of edges (placements). PathPlacement is used to allow us to disambiguate
--   different structures which share the same name
data AnnotatedStructure a = AnnotatedStructure
  { pathPlacements :: [PathPlacement]
  , namedStructure :: NamedStructure a
  }

packageStructures :: [NamedStructure a] -> PStructure a
packageStructures namedStructs = Structure (PositionedGrid origin EmptyGrid) namedStructs [] []

-- | This function constructs from the base structure and initial structure definitions a graph.
--   If the base structure is unnamed, the PathToRoot of the base structure will be the empty list.
--   Otherwise, the PathToRoot of the base structure is the singleton containing just the name of the base structure
--   The nodes in the graph are ''AnnotatedStructure'\'s. Each node is uniquely identified by the
--   path of structure names from the structure it contains to the root. Each node contains its list of edges.
mkGraph ::
  forall a.
  PStructure (Maybe a) ->
  Either Text (HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)))
mkGraph baseStructure = go mempty [] mempty baseNamed
 where
  go ::
    -- Knowledge inherited from parent, allows us to find the full path for a placement
    HM.HashMap StructureName PathToRoot ->
    -- Path from parent to root
    [StructureName] ->
    HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)) ->
    NamedStructure (Maybe a) ->
    Either Text (HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)))
  go inheritedKnowledge parentToRootPath !acc !namedStruct = do
    let struct = structure namedStruct
        substructures = structures struct
        structName = name namedStruct
        structPlacements = placements struct
        structPathNE = structName NE.:| parentToRootPath
        structPath = NE.toList structPathNE
        knowledgeOfChildren = HM.fromList $ map ((id &&& (NE.:| structPath)) . name) substructures
        knowledge = HM.union knowledgeOfChildren inheritedKnowledge
        f :: Placement -> Either Text PathPlacement
        f placement = case HM.lookup (src placement) knowledge of
          Nothing ->
            Left $
              T.unwords
                ["Within", getStructureName structName <> ":", "Could not look up structure", quote . getStructureName . src $ placement]
          Just path -> pure $ PathPlacement path (structurePose placement)

    structPathPlacements <- traverse f $ structPlacements
    let annotatedStruct = AnnotatedStructure structPathPlacements namedStruct
        !acc' = HM.insert structPathNE annotatedStruct acc
    foldlM (go knowledge structPath) acc' substructures
  baseNamed = NamedArea (StructureName "") mempty Nothing baseStructure

data DFSPath = DFSPath (HS.HashSet PathToRoot) [PathToRoot]
data DFSState = DFSState (HS.HashSet PathToRoot) [PathToRoot]

addToDFSPath :: PathToRoot -> DFSPath -> DFSPath
addToDFSPath pathToRoot (DFSPath pathElems back) = DFSPath (HS.insert pathToRoot pathElems) (pathToRoot : back)

basePathToRoot :: PathToRoot
basePathToRoot = NE.singleton (StructureName "")

-- | Given a graph constructed via 'mkGraph', this function does a dfs on the graph to find
--   any cycles in the graph that exist. If such a cycle exists, an appropriate error message is returned (in 'Left').
--   If there are no cycles, what is instead returned is a topologically sorted list of all the identifiers in the graph (in 'Right').
--   The list is such that if structure A places structure B, the identifier for structure B precedes the identifier for structure A.
topSortGraph :: HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)) -> Either Text [PathToRoot]
topSortGraph graph = fmap (reverse . getAcc) . foldlM (go emptyPath) acc0 $ HM.toList graph
 where
  go :: DFSPath -> DFSState -> (PathToRoot, AnnotatedStructure (Maybe a)) -> Either Text DFSState
  go dfsPathOfParent@(DFSPath parentPathMembers _) acc@(DFSState visited _) (!pathToRoot, !annotatedStruct) = do
    if (pathToRoot `HS.member` visited)
      then pure acc
      else do
        when (pathToRoot `HS.member` parentPathMembers) $ Left "TODO PUT CYCLE ERROR HERE"
        let dfsPath = addToDFSPath pathToRoot dfsPathOfParent
            placementPaths = map pathToPlacement $ pathPlacements annotatedStruct
            f acc' path = case HM.lookup path graph of
              Nothing -> Left $ "TODO UNEXPECTED MISSING"
              Just annotated -> go dfsPath acc' (path, annotated)
        DFSState visited' topSortAcc' <- foldlM f acc placementPaths
        pure $ DFSState (HS.insert pathToRoot visited') (pathToRoot : topSortAcc')
  emptyPath = DFSPath HS.empty []
  acc0 = DFSState HS.empty []
  getAcc (DFSState _ acc) = acc

-- | Given a graph constructed via 'mkGraph' and a topologically sorted listed of identifiers constructed via 'topSortGraph',
--   this function assembles all the structures in the graph, taking care to avoid redundant work. The assembled structure for
--   an identifier can be found by looking up the identifier in the HashMap that this function returns.
mergeStructures :: forall a. HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)) -> [PathToRoot] -> Either Text (HM.HashMap PathToRoot (MergedStructure (Maybe a)))
mergeStructures graph topSorted = foldlM go mempty topSorted
 where
  -- This will not be called unless the graph and topological sort are incorrect
  mkCouldNotFindError path = T.unwords ["Could not look up structure", showPath path, "in mergeStructure"]
  lookupHandling m path f = case HM.lookup path m of
    Nothing -> Left $ mkCouldNotFindError path
    Just x -> pure (f x)

  validatePlacements path toPlace = do
    result <- traverse (\(PathPlacement p pose) -> lookupHandling graph p (,pose)) $ toPlace
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
    let parentage = if NE.compareLength path 1 == EQ then Root else WithParent (NE.head path)
        struct = structure . namedStructure $ annotatedStruct
        origArea = area struct
        initialWaypoints = map (Originated parentage) . waypoints $ struct
        initialOverlays = computeInitialOverlays toPlaceAnnotated
        initialMerged = MergedStructure origArea initialOverlays initialWaypoints
        merged = foldl' overlaySingleStructure initialMerged mergedToPlace
    pure $ HM.insert path merged alreadyMerged

-- | Given a base structure, this function returns the HashMap of merged structures (in 'Right')
--   along with the constructed graph of annotated structures.
--   If the input is invalid, this functions instead returns an appropriate error message (in 'Left').
assembleStructure' ::
  PStructure (Maybe a) ->
  Either Text (HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)), HM.HashMap PathToRoot (MergedStructure (Maybe a)))
assembleStructure' baseStructure = do
  !graph <- mkGraph baseStructure
  topSorted <- topSortGraph graph
  !mergedMap <- mergeStructures graph topSorted
  pure (graph, mergedMap)

-- case HM.lookup basePathToRoot mergedMap of
--   Nothing -> Left $ "Unable to find root structure in graph"
--   Just merged -> pure merged

-- | Version of 'assembleStructure'' that discards the map of merged structures
assembleStructure ::
  PStructure (Maybe a) ->
  Either Text (MergedStructure (Maybe a))
assembleStructure baseStructure = do
  mergedMap <- snd <$> assembleStructure' baseStructure
  case HM.lookup basePathToRoot mergedMap of
    Nothing -> Left $ "Unable to find root structure in graph"
    Just merged -> pure merged

-- | Assembles the list of named structures
assembleStructures ::
  [NamedStructure (Maybe a)] ->
  Either Text [(NamedStructure (Maybe a), MergedStructure (Maybe a))]
assembleStructures namedStructs = do
  (graph, mergedMap) <- assembleStructure' (packageStructures namedStructs)
  let paths = map (\namedStruct -> name namedStruct NE.:| NE.toList basePathToRoot) namedStructs
      f path = do
        !annotatedStruct <- case HM.lookup path graph of
          Nothing -> Left $ T.unwords ["Unable to find structure at path", showPath path, "in constructed graph"]
          Just x -> pure x
        !merged <- case HM.lookup path mergedMap of
          Nothing -> Left $ T.unwords ["Unable to find structure at path", showPath path, "in collection of merged structures"]
          Just x -> pure x
        pure (namedStructure annotatedStruct, merged)
  traverse f paths

-- flip evalStateT mempty (traverse f namedStructs)

-- structureMap = makeStructureMap namedStructs
-- f !named = do
--   !cached <- get
--   (result, cached') <- liftEither $ assembleStructure' structureMap cached (Right named)
--   put $! cached'
--   pure (named, result)

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

elaboratePlacement' :: Parentage StructureName -> Text
elaboratePlacement' p =
  T.unwords
    [ "Within"
    , pTxt <> ":"
    , ""
    ]
 where
  pTxt = case p of
    Root -> "root placement"
    WithParent (StructureName sn) ->
      T.unwords
        [ "placement of"
        , quote sn
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

validatePlacement' ::
  HM.HashMap StructureName (NamedStructure (Maybe a)) ->
  Placement ->
  Either Text (Placed (Maybe a))
validatePlacement'
  structureMap
  placement@(Placement sName@(StructureName n) (Pose _ orientation)) = do
    t@(_, ns) <-
      maybeToEither
        (T.unwords ["Could not look up structure", quote n])
        $ sequenceA (placement, HM.lookup sName structureMap)

    let placementDirection = up orientation
        recognizedOrientations = recognize ns

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
    return $ uncurry Placed t
   where
    renderDir = quote . T.pack . directionJsonModifier . show
