{-# LANGUAGE DerivingVia #-}

module Swarm.Game.Scenario.EntityFacade where

import Control.Lens hiding (from, (.=), (<.>))
import Data.Text (Text)
import Data.Yaml as Y
import Swarm.Game.Display (Display)
import Swarm.Game.Entity qualified as E

type EntityName = Text

data EntityFacade = EntityFacade EntityName Display
  deriving (Eq)

-- Note: This instance is used only for the purpose of WorldPalette
instance ToJSON EntityFacade where
  toJSON (EntityFacade eName _display) = toJSON eName

mkPaint :: E.Entity -> EntityFacade
mkPaint e =
  EntityFacade
    (e ^. E.entityName)
    (e ^. E.entityDisplay)
