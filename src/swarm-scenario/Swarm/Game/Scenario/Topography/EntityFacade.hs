{-# LANGUAGE DerivingVia #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Stand-in type for an "Entity" for purposes
-- that do not require carrying around the entire state
-- of an Entity.
--
-- Useful for simplified serialization, debugging,
-- and equality checking, particularly for the World Editor.
module Swarm.Game.Scenario.Topography.EntityFacade where

import Control.Lens ((^.))
import Data.Yaml as Y (ToJSON (toJSON))
import Swarm.Game.Cosmetic.Display
import Swarm.Game.Entity qualified as E
import Swarm.Game.Location (toDirection)
import Swarm.Language.Syntax.Direction (Direction)

-- | This datatype is a lightweight stand-in for the full-fledged
--   "Entity" type without the baggage of all of its other fields.  It
--   contains the bare minimum display information for rendering.
data EntityFacade = EntityFacade E.EntityName Display (Maybe Direction)
  deriving (Eq)

-- Note: This instance is used only for the purpose of WorldPalette
instance ToJSON EntityFacade where
  toJSON (EntityFacade eName _disp _hdg) = toJSON eName

mkFacade :: E.Entity -> EntityFacade
mkFacade e =
  EntityFacade
    (e ^. E.entityName)
    (e ^. E.entityDisplay)
    ((e ^. E.entityOrientation) >>= toDirection)
