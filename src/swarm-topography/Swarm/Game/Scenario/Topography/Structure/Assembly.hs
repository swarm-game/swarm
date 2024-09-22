{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Definitions of "structures" for use within a map
-- as well as logic for combining them.
module Swarm.Game.Scenario.Topography.Structure.Assembly (
  mergeStructures,

  -- * Exposed for unit tests:
  foldLayer,
)
where

import Control.Arrow (left, (&&&))
import Control.Monad (when)
import Data.Coerce
import Data.Either.Extra (maybeToEither)
import Data.Foldable (foldlM)
import Data.Map qualified as M
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

-- | Destructively overlays one direct child structure
-- upon the input structure.
-- However, the child structure is assembled recursively.
overlaySingleStructure ::
  M.Map StructureName (NamedStructure (Maybe a)) ->
  Placed (Maybe a) ->
  MergedStructure (Maybe a) ->
  Either Text (MergedStructure (Maybe a))
overlaySingleStructure
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

-- | Overlays all of the "child placements", such that the children encountered later
-- in the YAML file supersede the earlier ones (dictated by using 'foldl' instead of 'foldr').
mergeStructures ::
  M.Map StructureName (NamedStructure (Maybe a)) ->
  Parentage Placement ->
  PStructure (Maybe a) ->
  Either Text (MergedStructure (Maybe a))
mergeStructures inheritedStrucDefs parentPlacement baseStructure = do
  overlays <-
    left (elaboratePlacement parentPlacement <>) $
      mapM (validatePlacement structureMap) subPlacements

  foldLayer structureMap origArea overlays originatedWaypoints
 where
  Structure origArea subStructures subPlacements subWaypoints = baseStructure

  originatedWaypoints = map (Originated parentPlacement) subWaypoints

  -- deeper definitions override the outer (toplevel) ones
  structureMap =
    M.union
      (M.fromList $ map (name &&& id) subStructures)
      inheritedStrucDefs

-- | NOTE: Each successive overlay may alter the coordinate origin.
-- We make sure this new origin is propagated to subsequent sibling placements.
foldLayer ::
  M.Map StructureName (NamedStructure (Maybe a)) ->
  PositionedGrid (Maybe a) ->
  [Placed (Maybe a)] ->
  [Originated Waypoint] ->
  Either Text (MergedStructure (Maybe a))
foldLayer structureMap origArea overlays originatedWaypoints =
  foldlM
    (flip $ overlaySingleStructure structureMap)
    (MergedStructure origArea wrappedOverlays originatedWaypoints)
    overlays
 where
  wrappedOverlays =
    map wrapPlacement $
      filter (\(Placed _ ns) -> isRecognizable ns) overlays

  wrapPlacement (Placed z ns) =
    LocatedStructure
      (name ns)
      (up $ orient structPose)
      (offset structPose)
   where
    structPose = structurePose z

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
    placementAdjustedByOrigin = (gridPosition baseGrid .+^ asVector yamlPlacementOffset) .-^ asVector childAdjustedOrigin
    positionedOverlay = PositionedGrid placementAdjustedByOrigin reorientedOverlayCells

-- * Validation

elaboratePlacement :: Parentage Placement -> Text
elaboratePlacement p =
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
  M.Map StructureName (NamedStructure (Maybe a)) ->
  Placement ->
  Either Text (Placed (Maybe a))
validatePlacement
  structureMap
  placement@(Placement sName@(StructureName n) (Pose _ orientation)) = do
    t@(_, ns) <-
      maybeToEither
        (T.unwords ["Could not look up structure", quote n])
        $ sequenceA (placement, M.lookup sName structureMap)

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
