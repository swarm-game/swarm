{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Scenario.Topography.Navigation.Portal where

import Control.Monad (forM, forM_, unless)
import Data.Aeson (FromJSON)
import Data.Int (Int32)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Linear (V2)
import Swarm.Game.Location
import Swarm.Game.Scenario.Topography.Navigation.Waypoint
import Swarm.Util (binTuples, quote)

-- | Note: The primary overworld shall use
-- the reserved name \"root\".
newtype SubworldName = SubworldName Text
  deriving (Show, Eq, Ord, Generic, FromJSON)

data Navigation = Navigation
  { waypoints :: M.Map WaypointName (NonEmpty Location)
  -- ^ Note that waypoints defined at the "root" level are still relative to
  -- the top-left corner of the map rectangle; they are not in absolute world
  -- coordinates (as with applying the "ul" offset).
  , portals :: M.Map Location Location
  }
  deriving (Eq, Show)

data PortalExit = PortalExit
  { exit :: WaypointName
  , subworldName :: Maybe SubworldName
  -- ^ Note: 'Nothing' indicates that references a waypoint within the same subworld.
  }
  deriving (Show, Eq, Generic, FromJSON)

data Portal = Portal
  { entrance :: WaypointName
  , exitInfo :: PortalExit
  }
  deriving (Show, Eq, Generic, FromJSON)

failUponDuplication ::
  (MonadFail m, Show a, Show b) =>
  String ->
  M.Map a (NonEmpty b) ->
  m ()
failUponDuplication message binnedMap =
  forM_ (listToMaybe $ M.toList duplicated) $ \(pIn, pOuts) ->
    fail $
      unwords
        [ "Waypoint"
        , show pIn
        , message
        , intercalate ", " $ map show $ NE.toList pOuts
        ]
 where
  duplicated = M.filter ((> 1) . NE.length) binnedMap

-- | Enforces the following constraints:
-- * portals can have multiple entrances but only a single exit
-- * no two portals share the same entrance location
-- * global waypoint uniqueness when the "unique" flag is specified
validateNavigation ::
  (MonadFail m, Traversable t) =>
  V2 Int32 ->
  [Originated Waypoint] ->
  t Portal ->
  m Navigation
validateNavigation upperLeft unmergedWaypoints portalDefs = do
  failUponDuplication "is required to be unique, but is duplicated in:" waypointsWithUniqueFlag

  -- TODO(#144) Currently ignores subworld references
  nestedPortalPairs <- forM portalDefs $ \(Portal entranceName (PortalExit exitName@(WaypointName rawExitName) _)) -> do
    -- Portals can have multiple entrances but only a single exit.
    -- That is, the pairings of entries to exits must form a proper mathematical "function".
    -- Multiple occurrences of entrance waypoints of a given name will replicate portal entrances.
    entranceLocs <- getLocs entranceName
    firstExitLoc :| otherExits <- getLocs exitName
    unless (null otherExits)
      . fail
      . T.unpack
      $ T.unwords ["Ambiguous exit waypoints named", quote rawExitName, "for portal"]
    return $ map ((,extractLoc firstExitLoc) . extractLoc) $ NE.toList entranceLocs

  let reconciledPortalPairs = concat nestedPortalPairs

  -- Aside from the enforcement of single-exit per portal, we apply another layer of
  -- enforcement to ensure that no two portals share the same entrance location
  failUponDuplication "has overlapping portal entrances exiting to" $
    binTuples reconciledPortalPairs

  return $ Navigation bareWaypoints $ M.fromList reconciledPortalPairs
 where
  getLocs wpWrapper@(WaypointName rawName) = case M.lookup wpWrapper correctedWaypoints of
    Nothing ->
      fail $
        T.unpack $
          T.unwords
            [ "No waypoint named"
            , quote rawName
            ]
    Just xs -> return xs

  extractLoc (Originated _ (Waypoint _ loc)) = loc
  correctedWaypoints =
    binTuples $
      map
        (\x -> (wpName $ wpConfig $ value x, fmap (offsetWaypoint upperLeft) x))
        unmergedWaypoints
  bareWaypoints = M.map (NE.map extractLoc) correctedWaypoints

  waypointsWithUniqueFlag = M.filter (any $ wpUnique . wpConfig . value) correctedWaypoints
