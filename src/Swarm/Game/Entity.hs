{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.Entity
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- An /entity/ represents an object that exists in the world.  Each
-- entity has a way to be displayed, some metadata such as a name and
-- description, some properties, and possibly an inventory of other
-- entities.
--
-----------------------------------------------------------------------------

module Swarm.Game.Entity
  ( -- * Properties
    EntityProperty(..)

    -- * Entities
  , Entity(..)
  , displayEntity

    -- ** Lenses
  , entityDisplay, entityName, entityDescription, entityOrientation
  , entityProperties

    -- * Basic entity types and entity map
  , EntityType(..)
  , entityMap

  )
where

import           Control.Lens
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Text          (Text)
import           Linear

import           Brick              (Widget, str, withAttr)

import           Swarm.Game.Display
import           Swarm.TUI.Attr

data EntityProperty
  = Solid          -- ^ Robots can't move onto a cell with this resource.  XXX rename Walkable
  | Harvestable    -- ^ Robots can harvest this.                           XXX rename Grabbable
  -- XXX add more properties
  deriving (Eq, Ord, Show)

-- | A record to hold information about an entity.
data Entity = Entity
  { _entityDisplay     :: Display  -- ^ The way this entity should be displayed
                                   --   on the world map.
  , _entityName        :: Text     -- ^ The name of the entity, used
                                   --   /e.g./ in an inventory display.
  , _entityDescription :: Text     -- ^ A longer-form description.
  , _entityOrientation :: Maybe (V2 Int)
  , _entityProperties  :: [EntityProperty]
  }
  -- Note that an entity does not have a location, because the
  -- location of an entity is implicit in the way it is stored (by
  -- location).

makeLenses ''Entity

-- | Display an entity as a character.
displayEntity :: Entity -> Widget n
displayEntity e
  = withAttr (e ^. entityDisplay . displayAttr)
  $ str [lookupDisplay (e ^. entityOrientation) (e ^. entityDisplay)]

-- | An enumeration of the basic entities in the game.  Note there can
--   easily be other, custom entities besides the ones listed here.
data EntityType
  = TreeE
  | RockE
  deriving (Eq, Ord, Show)

-- | A map containing a default entity record for each basic entity.
entityMap :: Map EntityType Entity
entityMap = M.fromList
  [ (TreeE, treeEntity)
  , (RockE, rockEntity)
  ]

treeEntity :: Entity
treeEntity = Entity
  { _entityDisplay     = defaultEntityDisplay 'T'
                         & displayAttr .~ plantAttr
  , _entityName        = "Tree"
  , _entityDescription = "It is a tree."
  , _entityOrientation = Nothing
  , _entityProperties  = [Harvestable]
  }

rockEntity :: Entity
rockEntity = Entity
  { _entityDisplay     = defaultEntityDisplay '@'
                         & displayAttr .~ rockAttr
  , _entityName        = "Rock"
  , _entityDescription = "A rock."
  , _entityOrientation = Nothing
  , _entityProperties  = [Solid]
  }
