{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- The view center state combines the focused robot, viewed location and rule for which to follow.
module Swarm.Game.State.ViewCenter.Internal (
  -- * View center rule
  ViewCenterRule (..),

  -- * View center state
  ViewCenter,
  defaultViewCenter,

  -- ** Lenses
  viewCenterRule,
  viewCenterLocation,
  viewRobotID,

  -- ** Updates
  syncViewCenter,
  modifyViewCenter,
  unfocus,
) where

import Control.Lens (Lens', (%~), (&), (.~), (^.))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.Universe as U
import Swarm.Util.Lens (makeLensesNoSigs)

-- | The 'ViewCenterRule' specifies how to determine the center of the
--   world viewport.
data ViewCenterRule
  = -- | The view should be centered on an absolute position.
    VCLocation (Cosmic Location)
  | -- | The view should be centered on a certain robot.
    VCRobot RID
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- | The collection of view center state, which should be kept synchronised.
--
-- The player is either looking at a location or following a robot.
-- But the game needs to at all times show some location and information
-- about a robot.
data ViewCenter = ViewCenter
  { _viewCenterRule :: ViewCenterRule
  , _viewCenterLocation :: Cosmic Location
  , _viewRobotID :: RID
  }

-- | The default view center at the initial base robot and default starting location.
defaultViewCenter :: ViewCenter
defaultViewCenter =
  ViewCenter
    { _viewCenterRule = VCRobot 0
    , _viewCenterLocation = defaultCosmicLocation
    , _viewRobotID = 0
    }

makeLensesNoSigs ''ViewCenter

-- | The current rule for determining the center of the world view.
viewCenterRule :: Lens' ViewCenter ViewCenterRule

-- | The current center of the world view. Note that this shouldn't be
--   modified directly, since it is calculated automatically from the
--   'viewCenterRule'.  To modify the view center, either set the
--   'viewCenterRule', or use 'modifyViewCenter'.
viewCenterLocation :: Lens' ViewCenter (Cosmic Location)

-- | The current robot in focus.
--
-- This value should be updated only when
-- the 'viewCenterRule' is specified to be a robot.
--
-- Technically it's the last robot ID specified by 'viewCenterRule',
-- but that robot may not be alive anymore - to be safe use 'focusedRobot'.
viewRobotID :: Lens' ViewCenter RID

-- | Synchronise the view center state based on current rule.
--
-- If the robot does not exist, this does not 'unfocus' as it would
-- be annoying in Creative mode.
syncViewCenter ::
  (RID -> Maybe (Cosmic Location)) ->
  ViewCenter ->
  ViewCenter
syncViewCenter getRobotLoc vc = case vc ^. viewCenterRule of
  VCLocation loc -> vc {_viewCenterLocation = loc}
  VCRobot rid -> case getRobotLoc rid of
    Nothing -> vc
    Just loc -> vc {_viewCenterLocation = loc, _viewRobotID = rid}

-- | Modify the 'viewCenter' by applying an arbitrary function to the
--   current value.  Note that this also modifies the 'viewCenterRule'
--   to match.  After calling this function the 'viewCenterRule' will
--   specify a particular location, not a robot.
modifyViewCenter ::
  (RID -> Maybe (Cosmic Location)) ->
  (Cosmic Location -> Cosmic Location) ->
  ViewCenter ->
  ViewCenter
modifyViewCenter getRobotLoc update vc =
  vc
    & viewCenterRule %~ modifyViewCenterRule
    & syncViewCenter getRobotLoc
 where
  lastCenter = vc ^. viewCenterLocation
  modifyViewCenterRule = \case
    VCLocation l -> VCLocation (update l)
    VCRobot _ -> VCLocation (update lastCenter)

-- | "Unfocus" by modifying the view center rule to look at the
--   current location instead of a specific robot, and also set the
--   focused robot ID to an invalid value.  In classic mode this
--   causes the map view to become nothing but static.
unfocus :: (RID -> Maybe (Cosmic Location)) -> ViewCenter -> ViewCenter
unfocus getRobotLoc = (viewRobotID .~ -1000) . syncViewCenter getRobotLoc
