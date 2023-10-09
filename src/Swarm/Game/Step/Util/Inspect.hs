-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.Game.Step.Util.Inspect where

import Control.Carrier.State.Lazy
import Control.Effect.Lens
import Control.Lens hiding (from, use, (%=), (<.>))
import Control.Monad (forM, join)
import Data.IntMap qualified as IM
import Data.List (find)
import Data.Map qualified as M
import Data.Text (Text)
import Swarm.Game.Entity
import Swarm.Game.Location
import Swarm.Game.Robot
import Swarm.Game.State
import Swarm.Game.Universe
import Swarm.Game.World qualified as W
import Swarm.Language.Direction
import Swarm.Util (listEnums)

-- * World queries

getNeighborLocs :: Cosmic Location -> [Cosmic Location]
getNeighborLocs loc = map (offsetBy loc . flip applyTurn north . DRelative . DPlanar) listEnums

-- | Perform an action requiring a 'W.World' state component in a
--   larger context with a 'GameState'.
zoomWorld ::
  (Has (State GameState) sig m) =>
  SubworldName ->
  StateC (W.World Int Entity) Identity b ->
  m (Maybe b)
zoomWorld swName n = do
  mw <- use $ landscape . multiWorld
  forM (M.lookup swName mw) $ \w -> do
    let (w', a) = run (runState w n)
    landscape . multiWorld %= M.insert swName w'
    return a

-- | Get the robot with a given ID.
robotWithID :: (Has (State GameState) sig m) => RID -> m (Maybe Robot)
robotWithID rid = use (robotMap . at rid)

-- | Get the robot with a given name.
robotWithName :: (Has (State GameState) sig m) => Text -> m (Maybe Robot)
robotWithName rname = use (robotMap . to IM.elems . to (find $ \r -> r ^. robotName == rname))

-- | Get the entity (if any) at a given location.
entityAt :: (Has (State GameState) sig m) => Cosmic Location -> m (Maybe Entity)
entityAt (Cosmic subworldName loc) =
  join <$> zoomWorld subworldName (W.lookupEntityM @Int (W.locToCoords loc))
