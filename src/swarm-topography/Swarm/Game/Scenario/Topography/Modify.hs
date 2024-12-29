-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Captures the various possibilities of cell
-- modification as a sum type for use by the structure recognizer
-- (see 'Swarm.Game.Scenario.Topography.Structure.Recognition.Tracking.entityModified').
module Swarm.Game.Scenario.Topography.Modify where

import Data.Function (on)
import Swarm.Game.Scenario.Topography.Terraform

-- | Compare to 'WorldUpdate' in "Swarm.Game.World"
data CellUpdate e
  = NoChange (Maybe e)
  | Modified (CellModification e)

getModification :: CellUpdate e -> Maybe (CellModification e)
getModification (NoChange _) = Nothing
getModification (Modified x) = Just x

classifyModification ::
  Eq b =>
  (a -> b) ->
  -- | before
  Maybe a ->
  -- | after
  Maybe a ->
  CellUpdate a
classifyModification _ Nothing Nothing = NoChange Nothing
classifyModification _ Nothing (Just x) = Modified $ Add x
classifyModification _ (Just x) Nothing = Modified $ Remove x
classifyModification f (Just x) (Just y) =
  if ((/=) `on` f) x y
    then Modified $ Swap x y
    else NoChange $ Just x
