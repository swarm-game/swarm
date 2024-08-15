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
import Swarm.Game.Scenario.Topography.Terraform

-- | Compare to 'WorldUpdate' in "Swarm.Game.World"
data CellUpdate e
  = NoChange (Maybe e)
  | Modified (CellModification e)

getModification :: CellUpdate e -> Maybe (CellModification e)
getModification (NoChange _) = Nothing
getModification (Modified x) = Just x

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
    then Modified $ Swap x y
    else NoChange $ Just x
