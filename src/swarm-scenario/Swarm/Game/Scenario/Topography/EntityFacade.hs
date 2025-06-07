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
import Swarm.Game.Cosmetic.Display (Attribute)
import Swarm.Game.Cosmetic.Texel (Texel)
import Swarm.Game.Entity qualified as E

-- | This datatype is a lightweight stand-in for the
-- full-fledged "Entity" type without the baggage of all
-- of its other fields.
-- It contains the bare minimum display information
-- for rendering.
data EntityFacade = EntityFacade E.EntityName (Texel Attribute)
  deriving (Eq)

-- Note: This instance is used only for the purpose of WorldPalette
instance ToJSON EntityFacade where
  toJSON (EntityFacade eName _texel) = toJSON eName

mkFacade :: E.Entity -> EntityFacade
mkFacade e =
  EntityFacade
    (e ^. E.entityName)
    (E.renderEntity (const False) e)
