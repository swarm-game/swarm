-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Captures the various possibilities of cell
-- modification as a sum type for use by the structure recognizer
-- (see 'Swarm.Game.Scenario.Topography.Structure.Recognition.Tracking.entityModified').
module Swarm.Game.World.Modify where

import Control.Lens (view)
import Data.Function (on)
import Swarm.Game.Entity (Entity, entityHash)

-- | Compare to 'WorldUpdate' in "Swarm.Game.World"
data CellUpdate e
  = NoChange (Maybe e)
  | Modified (CellModification e)

getModification :: CellUpdate e -> Maybe (CellModification e)
getModification (NoChange _) = Nothing
getModification (Modified x) = Just x

data CellModification e
  = -- | Fields represent what existed in the cell "before" and "after", in that order.
    Swap e e
  | Remove e
  | Add e

classifyModification ::
  -- | before
  Maybe Entity ->
  -- | after
  Maybe Entity ->
  CellUpdate Entity
classifyModification Nothing Nothing = NoChange Nothing
classifyModification Nothing (Just x) = Modified $ Add x
classifyModification (Just x) Nothing = Modified $ Remove x
classifyModification (Just x) (Just y) =
  if ((/=) `on` view entityHash) x y
    then NoChange $ Just x
    else Modified $ Swap x y
