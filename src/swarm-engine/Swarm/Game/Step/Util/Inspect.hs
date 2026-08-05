-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Utilities for querying robots and their neighbors
module Swarm.Game.Step.Util.Inspect where

import Control.Lens hiding (from, use, (%=), (<.>))
import Data.IntMap qualified as IM
import Data.List (find)
import Data.List.Extra (enumerate)
import Data.Text (Text)
import Effectful
import Effectful.State.Static.Local
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.Universe
import Swarm.Language.Syntax (Phase (Instantiated))
import Swarm.Language.Syntax.Direction
import Swarm.Util.Lens

-- * World queries

getNeighborLocs :: Cosmic Location -> [Cosmic Location]
getNeighborLocs loc = map (offsetBy loc . flip applyTurn north . DRelative . DPlanar) enumerate

-- | Get the robot with a given ID.
robotWithID :: (State GameState :> es) => RID -> Eff es (Maybe (Robot Instantiated))
robotWithID rid = use (robotInfo . robotMap . at rid)

-- | Get the robot with a given name.
robotWithName :: (State GameState :> es) => Text -> Eff es (Maybe (Robot Instantiated))
robotWithName rname = use (robotInfo . robotMap . to IM.elems . to (find $ \r -> r ^. robotName == rname))
