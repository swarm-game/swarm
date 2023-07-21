{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Navigation.Portal where

import Control.Arrow ((&&&))
import Control.Lens (view)
import Control.Monad (forM, forM_, unless)
import Data.Aeson
import Data.Bifunctor (first)
import Data.BoolExpr (Signed (..))
import Data.Function (on)
import Data.Functor.Identity
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Linear (V2, negated)
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Navigation.Waypoint
import Swarm.Game.Universe
import Swarm.Util (allEqual, binTuples, both, failT, quote, showT)

type WaypointMap = M.Map WaypointName (NonEmpty Location)

data AnnotatedDestination a = AnnotatedDestination
  { enforceConsistency :: Bool
  , cosmoLocation :: Cosmic a
  }
  deriving (Show, Eq)

-- | Parameterized on waypoint dimensionality ('additionalDimension') and
-- on the portal location specification method ('portalExitLoc').
-- == @additionalDimension@
-- As a member of the 'WorldDescription', waypoints are only known within a
-- a single subworld, so 'additionalDimension' is 'Identity' for the map
-- of waypoint names to planar locations.
-- At the Scenario level, in contrast, we have access to all subworlds, so
-- we nest this map to planar locations in additional mapping layer by subworld.
-- == @portalExitLoc@
-- At the subworld parsing level, we only can obtain the planar location
-- for portal /entrances/, but the /exits/ remain as waypoint names.
-- At the Scenario-parsing level, we finally have
-- access to the waypoints across all subworlds, and can therefore translate
-- the portal exits to concrete planar locations.
data Navigation additionalDimension portalExitLoc = Navigation
  { waypoints :: additionalDimension WaypointMap
  -- ^ Note that waypoints defined at the "root" level are still relative to
  -- the top-left corner of the map rectangle; they are not in absolute world
  -- coordinates (as with applying the "ul" offset).
  , portals :: M.Map (Cosmic Location) (AnnotatedDestination portalExitLoc)
  }

deriving instance (Eq (a WaypointMap), Eq b) => Eq (Navigation a b)
deriving instance (Show (a WaypointMap), Show b) => Show (Navigation a b)

data PortalExit = PortalExit
  { exit :: WaypointName
  , subworldName :: Maybe SubworldName
  -- ^ Note: 'Nothing' indicates that references a waypoint within the same subworld.
  }
  deriving (Show, Eq, Generic, FromJSON)

data Portal = Portal
  { entrance :: WaypointName
  , exitInfo :: PortalExit
  , consistent :: Bool
  }
  deriving (Show, Eq)

instance FromJSON Portal where
  parseJSON = withObject "Portal" $ \v ->
    Portal
      <$> v
        .: "entrance"
      <*> v
        .: "exitInfo"
      <*> v .:? "consistent" .!= False

failUponDuplication ::
  (MonadFail m, Show a, Show b) =>
  T.Text ->
  M.Map a (NonEmpty b) ->
  m ()
failUponDuplication message binnedMap =
  forM_ (listToMaybe $ M.toList duplicated) $ \(pIn, pOuts) ->
    failT
      [ "Waypoint"
      , showT pIn
      , message
      , T.intercalate ", " $ map showT $ NE.toList pOuts
      ]
 where
  duplicated = M.filter ((> 1) . NE.length) binnedMap

failWaypointLookup :: MonadFail m => WaypointName -> Maybe a -> m a
failWaypointLookup (WaypointName rawName) =
  maybe (failT ["No waypoint named", quote rawName]) return

-- |
-- The following constraints must be enforced:
-- * portals based on plural waypoint multiplicity can have multiple entrances but only a single exit
-- * no two portals share the same entrance location
-- * waypoint uniqueness within a subworld when the 'unique' flag is specified
--
-- == Data flow:
--
-- Waypoints are defined within a subworld and are namespaced by it.
-- Optional intra-subworld uniqueness of Waypoints is enforced at WorldDescription
-- parse time.
-- Portals are declared within a subworld. The portal entrance must be a waypoint
-- within this subworld.
-- They can reference waypoints in other subworlds as exits, but these references
-- are not validated until the Scenario parse level.
--
-- * Since portal /entrances/ are specified at the subworld level, validation that
--   no entrances overlap can also be performed at that level.
-- * However, enforcement of single-multiplicity on portal /exits/ must be performed
--   at scenario-parse level, because for a portal exit that references a waypoint in
--   another subworld, we can't know at the single-WorldDescription level whether
--   that waypoint has plural multiplicity.
validatePartialNavigation ::
  (MonadFail m, Traversable t) =>
  SubworldName ->
  Location ->
  [Originated Waypoint] ->
  t Portal ->
  m (Navigation Identity WaypointName)
validatePartialNavigation currentSubworldName upperLeft unmergedWaypoints portalDefs = do
  failUponDuplication "is required to be unique, but is duplicated in:" waypointsWithUniqueFlag

  nestedPortalPairs <- forM portalDefs $ \(Portal entranceName (PortalExit exitName maybeExitSubworldName) isConsistent) -> do
    -- Portals can have multiple entrances but only a single exit.
    -- That is, the pairings of entries to exits must form a proper mathematical "function".
    -- Multiple occurrences of entrance waypoints of a given name will result in
    -- multiple portal entrances.
    entranceLocs <- getLocs entranceName

    let sw = fromMaybe currentSubworldName maybeExitSubworldName
        f = (,AnnotatedDestination isConsistent $ Cosmic sw exitName) . extractLoc
    return $ map f $ NE.toList entranceLocs

  let reconciledPortalPairs = concat nestedPortalPairs

  -- Aside from the enforcement of single-exit per portal, we apply another layer of
  -- enforcement to ensure that no two portals share the same entrance location
  failUponDuplication "has overlapping portal entrances exiting to" $
    binTuples reconciledPortalPairs

  return . Navigation (pure bareWaypoints) . M.fromList $
    map (first $ Cosmic currentSubworldName) reconciledPortalPairs
 where
  getLocs wpWrapper = failWaypointLookup wpWrapper $ M.lookup wpWrapper correctedWaypoints

  extractLoc (Originated _ (Waypoint _ loc)) = loc
  correctedWaypoints =
    binTuples $
      map
        (\x -> (wpName $ wpConfig $ value x, fmap (offsetWaypoint $ upperLeft .-. origin) x))
        unmergedWaypoints
  bareWaypoints = M.map (NE.map extractLoc) correctedWaypoints
  waypointsWithUniqueFlag = M.filter (any $ wpUnique . wpConfig . value) correctedWaypoints

validatePortals ::
  MonadFail m =>
  Navigation (M.Map SubworldName) WaypointName ->
  m (M.Map (Cosmic Location) (AnnotatedDestination Location))
validatePortals (Navigation wpUniverse partialPortals) = do
  portalPairs <- forM (M.toList partialPortals) $ \(portalEntrance, AnnotatedDestination isConsistent portalExit@(Cosmic swName (WaypointName rawExitName))) -> do
    firstExitLoc :| otherExits <- getLocs portalExit
    unless (null otherExits) $
      failT
        [ "Ambiguous exit waypoints named"
        , quote rawExitName
        , "for portal"
        ]
    return (portalEntrance, AnnotatedDestination isConsistent $ Cosmic swName firstExitLoc)

  ensureSpatialConsistency portalPairs

  return $ M.fromList portalPairs
 where
  getLocs (Cosmic swName wpWrapper@(WaypointName exitName)) = do
    subworldWaypoints <- case M.lookup swName wpUniverse of
      Just x -> return x
      Nothing ->
        fail $
          T.unpack $
            T.unwords
              [ "Could not lookup waypoint"
              , quote exitName
              , "for portal exit because subworld"
              , quote $ renderWorldName swName
              , "does not exist"
              ]

    failWaypointLookup wpWrapper $
      M.lookup wpWrapper subworldWaypoints

-- | A portal can be marked as \"consistent\", meaning that it represents
-- a conventional physical passage rather than a \"magical\" teleportation.
--
-- If there exists more than one \"consistent\" portal between the same
-- two subworlds, then the portal locations must be spatially consistent
-- between the two worlds. I.e. the space comprising the two subworlds
-- forms a "conservative vector field".
--
-- Verifying this is simple:
-- For all of the portals between Subworlds A and B:
-- * The coordinates of all \"consistent\" portal locations in Subworld A
--   are subtracted from the corresponding coordinates in Subworld B. It
--   does not matter which are exits vs. entrances.
-- * The resulting \"vector\" from every pair must be equal.
ensureSpatialConsistency ::
  MonadFail m =>
  [(Cosmic Location, AnnotatedDestination Location)] ->
  m ()
ensureSpatialConsistency xs =
  unless (null nonUniform) $
    fail $
      unwords
        [ "Non-uniform portal distances:"
        , show nonUniform
        ]
 where
  consistentPairs :: [(Cosmic Location, Cosmic Location)]
  consistentPairs = map (fmap cosmoLocation) $ filter (enforceConsistency . snd) xs

  interWorldPairs :: [(Cosmic Location, Cosmic Location)]
  interWorldPairs = filter (uncurry ((/=) `on` view subworld)) consistentPairs

  normalizedOrdering :: [Signed (Cosmic Location, Cosmic Location)]
  normalizedOrdering = map normalizePairOrder interWorldPairs

  normalizePairOrder :: (Cosmic a, Cosmic a) -> Signed (Cosmic a, Cosmic a)
  normalizePairOrder pair =
    if uncurry ((>) `on` view subworld) pair
      then Negative $ swap pair
      else Positive pair

  tuplify :: (Cosmic a, Cosmic a) -> ((SubworldName, SubworldName), (a, a))
  tuplify = both (view subworld) &&& both (view planar)

  getSigned :: Signed (V2 Int32) -> V2 Int32
  getSigned = \case
    Positive x -> x
    Negative x -> negated x

  groupedBySubworldPair ::
    Map (SubworldName, SubworldName) (NonEmpty (Signed (Location, Location)))
  groupedBySubworldPair = binTuples $ map (sequenceSigned . fmap tuplify) normalizedOrdering

  vectorized :: Map (SubworldName, SubworldName) (NonEmpty (V2 Int32))
  vectorized = M.map (NE.map (getSigned . fmap (uncurry (.-.)))) groupedBySubworldPair

  nonUniform :: Map (SubworldName, SubworldName) (NonEmpty (V2 Int32))
  nonUniform = M.filter ((not . allEqual) . NE.toList) vectorized

-- |
-- An implementation of 'sequenceA' for 'Signed' that does not
-- require an 'Applicative' instance for the inner 'Functor'.
--
-- == Discussion
-- Compare to the 'Traversable' instance of 'Signed':
-- @
-- instance Traversable Signed where
--   traverse f (Positive x) = Positive <$> f x
--   traverse f (Negative x) = Negative <$> f x
-- @
--
-- if we were to substitute 'id' for f:
-- @
--   traverse id (Positive x) = Positive <$> id x
--   traverse id (Negative x) = Negative <$> id x
-- @
-- our implementation essentially becomes @traverse id@.
--
-- However, we cannot simply write our implementation as @traverse id@, because
-- the 'traverse' function has an 'Applicative' constraint, which is superfluous
-- for our purpose.
--
-- Perhaps there is an opportunity to invent a typeclass for datatypes which
-- consist exclusively of unary (or more ambitiously, non-nullary?) data constructors,
-- for which a less-constrained 'sequence' function could be automatically derived.
-- Compare to the 'Comonad' class and its 'extract' function.
sequenceSigned ::
  Functor f =>
  Signed (f a) ->
  f (Signed a)
sequenceSigned = \case
  Positive x -> Positive <$> x
  Negative x -> Negative <$> x
