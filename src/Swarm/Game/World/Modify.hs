
-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Modifying the world
module Swarm.Game.World.Modify where

import Data.Function (on)
import Control.Lens
import Swarm.Game.Entity (Entity, entityHash)

-- | Compare to 'WorldUpdate' in "Swarm.Game.World"
data CellUpdate e
  = NoChange (Maybe e)
  | Modified (CellModification e)

getModification :: CellUpdate e -> Maybe (CellModification e)
getModification (NoChange _) = Nothing
getModification (Modified x) = Just x

data CellModification e
  = Swap
      e -- ^ before
      e -- ^ after
  | Remove e
  | Add e

classifyModification
  :: Maybe Entity -- ^ before
  -> Maybe Entity -- ^ after
  -> CellUpdate Entity
classifyModification Nothing Nothing = NoChange Nothing
classifyModification Nothing (Just x) = Modified $ Add x
classifyModification (Just x) Nothing = Modified $ Remove x
classifyModification (Just x) (Just y) = if ((/=) `on` view entityHash) x y
  then NoChange $ Just x
  else Modified $ Swap x y
  