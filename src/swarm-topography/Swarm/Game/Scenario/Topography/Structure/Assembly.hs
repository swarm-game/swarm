{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definitions of "structures" for use within a map
-- as well as logic for combining them.
module Swarm.Game.Scenario.Topography.Structure.Assembly (
  mergeStructures,
  makeStructureMap,
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
import Data.List (singleton)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Linear.Affine
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Area
import Swarm.Game.Scenario.Topography.Navigation.Waypoint
import Swarm.Game.Scenario.Topography.Placement
import Swarm.Game.Scenario.Topography.Structure
import Swarm.Game.Scenario.Topography.Structure.Named
import Swarm.Game.Scenario.Topography.Structure.Overlay
import Swarm.Game.Scenario.Topography.Structure.Recognition.Static
import Swarm.Language.Syntax.Direction (directionJsonModifier)
import Swarm.Util (commaList, quote, showT)
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
      mergeStructures inheritedStrucDefs (WithParent p) $ structure ns

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
mergeStructures ::
  HM.HashMap StructureName (NamedStructure (Maybe a)) ->
  Parentage Placement ->
  PStructure (Maybe a) ->
  Either Text (MergedStructure (Maybe a))
mergeStructures inheritedStrucDefs parentPlacement baseStructure = do
  failOnCyclicGraph "Structure" (getStructureName . name) gEdges

  overlays <-
    left (elaboratePlacement' parentPlacement <>) $
      mapM (validatePlacement' structureMap) subPlacements

  foldLayer structureMap origArea overlays originatedWaypoints
 where
  Structure origArea subStructures subPlacements subWaypoints = baseStructure

  originatedWaypoints = map (Originated parentPlacement) subWaypoints

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

type PathToRoot = [StructureName]

-- | Converts a path to the root into a fully qualified name
-- >>> showPath [StructureName ""]
-- ""
-- >>> showPath [StructureName "AB", StructureName "A", StructureName ""]
-- "A.AB"
showPath :: PathToRoot -> Text
showPath = f . T.intercalate "." . reverse . coerce
 where
  f txt = if T.null txt then txt else T.tail txt

data PathPlacement = PathPlacement
  { pathToPlacement :: PathToRoot
  , placementPose :: Pose
  }

data AnnotatedStructure a = AnnotatedStructure
  { pathPlacements :: [PathPlacement]
  -- ^ TODO temp
  , namedStructure :: NamedStructure a
  -- ^ the named structure itself
  }

-- | TODO Only construct subgraph of relevant structures (dependent on baseStructure's placements)
mkGraph ::
  forall a.
  HM.HashMap StructureName (NamedStructure (Maybe a)) ->
  PStructure (Maybe a) ->
  Either Text (HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)))
mkGraph initialStructDefs baseStructure = go initialKnowledge [] acc0 (NamedArea (StructureName "") mempty Nothing baseStructure)
 where
  go ::
    -- \| Knowledge inherited from parent, allows us to find the full path for a placement
    HM.HashMap StructureName PathToRoot ->
    -- \| Path from parent to root
    [StructureName] ->
    HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)) ->
    NamedStructure (Maybe a) ->
    Either Text (HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)))
  go inheritedKnowledge parentToRootPath !acc !struct = do
    let substructures = structures . structure $ struct
        structPlacements = placements . structure $ struct
        structPath = name struct : parentToRootPath
        knowledgeOfChildren = HM.fromList $ map ((id &&& (: structPath)) . name) substructures
        knowledge = HM.union knowledgeOfChildren inheritedKnowledge
        f :: Placement -> Either Text PathPlacement
        f placement = case HM.lookup (src placement) knowledge of
          Nothing -> Left $ T.unwords ["Could not look up structure", quote . getStructureName . src $ placement]
          Just path -> pure $ PathPlacement path (structurePose placement)
    structPathPlacements <- traverse f $ structPlacements
    let annotatedStruct = AnnotatedStructure structPathPlacements struct
        !acc' = HM.insert structPath annotatedStruct acc
    foldlM (go knowledge structPath) acc' substructures
  initialKnowledge = HM.fromList . HM.toList . fmap (singleton . name) $ initialStructDefs
  acc0 = HM.fromList . map (\(structName, namedStruct) -> (singleton structName, AnnotatedStructure [] namedStruct)) . HM.toList $ initialStructDefs

rootPathToRoot :: PathToRoot
rootPathToRoot = [StructureName ""]

data DFSPath = DFSPath (HS.HashSet PathToRoot) [PathToRoot]
data DFSState = DFSState (HS.HashSet PathToRoot) [PathToRoot]

getAcc :: DFSState -> [PathToRoot]
getAcc (DFSState _ acc) = acc

addToDFSPath :: PathToRoot -> DFSPath -> DFSPath
addToDFSPath pathToRoot (DFSPath pathElems back) = DFSPath (HS.insert pathToRoot pathElems) (pathToRoot : back)

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
        let placementPaths = map pathToPlacement $ pathPlacements annotatedStruct
        let f acc' path = case HM.lookup path graph of
              Nothing -> Left $ "TODO UNEXPECTED MISSING"
              Just annotated -> go dfsPath acc' (path, annotated)
        DFSState visited' topSortAcc' <- foldlM f acc placementPaths
        pure $ DFSState (HS.insert pathToRoot visited') (pathToRoot : topSortAcc')
  emptyPath = DFSPath HS.empty []
  acc0 = DFSState HS.empty []

mergeStructure :: forall a. HM.HashMap PathToRoot (AnnotatedStructure (Maybe a)) -> [PathToRoot] -> Either Text (HM.HashMap PathToRoot (MergedStructure (Maybe a)))
mergeStructure graph topSorted = foldlM go mempty topSorted
 where
  mkCouldNotFindError :: PathToRoot -> Text
  mkCouldNotFindError path = T.unwords ["Could not look up structure", showPath path, "in mergeStructure"]
  lookupHandling :: HM.HashMap PathToRoot c -> PathToRoot -> (c -> d) -> Either Text d
  lookupHandling m path f = case HM.lookup path m of
    Nothing -> Left $ mkCouldNotFindError path
    Just x -> pure (f x)
  validatePlacements :: PathToRoot -> [PathPlacement] -> Either Text ()
  validatePlacements path toPlace = do
    result <- traverse (\(PathPlacement p pose) -> lookupHandling graph p (,pose)) $ toPlace
    let result' = map (first namedStructure) result
    left (elaboratePlacement path <>) $ traverse_ (uncurry validatePlacement) result'
  go :: HM.HashMap PathToRoot (MergedStructure (Maybe a)) -> PathToRoot -> Either Text (HM.HashMap PathToRoot (MergedStructure (Maybe a)))
  go alreadyMerged path = do
    annotatedStruct <- lookupHandling graph path id
    let toPlace = pathPlacements annotatedStruct
    validatePlacements path toPlace
    let f (PathPlacement pathForPlacement pose) = lookupHandling alreadyMerged pathForPlacement (,pose)
    mergedToPlace <- traverse f toPlace
    let initialMerged = MergedStructure (area . structure . namedStructure $ annotatedStruct) [] []
        merged = foldl' overlaySingleStructure initialMerged mergedToPlace
    pure $ (HM.insert path merged alreadyMerged)

mergeStructures' ::
  HM.HashMap StructureName (NamedStructure (Maybe a)) ->
  PStructure (Maybe a) ->
  Either Text (MergedStructure (Maybe a))
mergeStructures' inheritedStructDefs baseStructure = do
  graph <- mkGraph inheritedStructDefs baseStructure
  topSorted <- topSortGraph graph
  mergedMap <- mergeStructure graph topSorted
  case HM.lookup rootPathToRoot mergedMap of
    Nothing -> Left $ "Unable to find root structure in graph"
    Just merged -> pure merged

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
    [ "Within"
    , showPath p <> ":"
    , ""
    ]

elaboratePlacement' :: Parentage Placement -> Text
elaboratePlacement' p =
  T.unwords
    [ "Within"
    , pTxt <> ":"
    , ""
    ]
 where
  pTxt = case p of
    Root -> "root placement"
    WithParent (Placement (StructureName sn) (Pose loc _)) ->
      T.unwords
        [ "placement of"
        , quote sn
        , "at"
        , showT loc
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
