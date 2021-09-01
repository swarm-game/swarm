-----------------------------------------------------------------------------
-- |
-- Module      :  Swarm.Game.Entity
-- Copyright   :  Brent Yorgey
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- An 'Entity' represents an object that exists in the world.  Each
-- entity has a way to be displayed, some metadata such as a name and
-- description, some properties, and possibly an inventory of other
-- entities.
--
-- This module also defines the 'Inventory' type, since the two types
-- are mutually recursive (an inventory contains entities, which can
-- have inventories).
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Swarm.Game.Entity
  ( -- * Properties
    EntityProperty(..)

    -- * Entities
  , Entity, mkEntity
  , displayEntity

    -- ** Lenses
  , entityDisplay, entityName, entityDescription, entityOrientation
  , entityProperties, inventory

    -- ** Basic entity types and entity map
  , EntityType(..)
  , entityMap

    -- * Inventories

  , Inventory, Count
  , empty, insert, lookup, delete, deleteCount, deleteAll, elems

  )
where

import           Control.Lens
import           Data.Bifunctor     (second)
import           Data.Function      (on)
import           Data.Hashable
import           Data.IntMap        (IntMap)
import qualified Data.IntMap        as IM
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import           Linear
import           Prelude            hiding (lookup)

import           Brick              (Widget, str, withAttr)

import           Swarm.Game.Display
import           Swarm.TUI.Attr

------------------------------------------------------------
-- Properties
------------------------------------------------------------

data EntityProperty
  = Solid          -- ^ Robots can't move onto a cell with this resource.  XXX rename Walkable
  | Harvestable    -- ^ Robots can harvest this.                           XXX rename Grabbable
  -- XXX add more properties
  deriving (Eq, Ord, Show, Generic, Hashable)

------------------------------------------------------------
-- Entity
------------------------------------------------------------

-- | A record to hold information about an entity.
--
--   The constructor for 'Entity' is intentionally not exported.  To
--   construct one, use the 'mkEntity' function.
--
--   There are two main constraints on the way entities are stored:
--
--   1. We want to be able to easily modify an entity in one
--      particular cell of the world (for example, painting one
--      tree red).
--   2. In an inventory, we want to store identical entities only
--      once, along with a count.
--
--   We could get (2) nicely by storing only names of entities, and
--   having a global lookup table from names to entity records.
--   However, storing names instead of actual entity records in the
--   world makes (1) more complex: every time we modify an entity we
--   would have to generate a fresh name for the modified entity and
--   add it to the global entity table.  This approach is also
--   annoying because it means we can't just uses lenses to drill down
--   into the properties of an entity in the world or in an inventory,
--   but have to do an intermediate lookup in the global (mutable!)
--   entity table.
--
--   On the other hand, if we just store entity records everywhere,
--   checking them for equality becomes expensive.  Having an
--   inventory be a map with entities themselves as keys sounds awful.
--
--   The solution we adopt here is that every @Entity@ record carries
--   along a hash value of all the other fields.  We just assume that
--   these hashes are unique (a collision is of course possible but
--   extremely unlikely).  Entities can be efficiently compared just
--   by looking at their hashes; they can be stored in a map using
--   hash values as keys; and we provide lenses which automatically
--   recompute the hash value when modifying a field of an entity
--   record.  Note also that world storage is still efficient, too:
--   thanks to referential transparency, in practice most of the
--   entities stored in the world that are the same will literally
--   just be stored as pointers to the same shared record.
data Entity = Entity
  { _entityHash        :: Int              -- ^ A hash value computed from the other fields
  , _entityDisplay     :: Display          -- ^ The way this entity should be displayed
                                           --   on the world map.
  , _entityName        :: Text             -- ^ The name of the entity, used
                                           --   /e.g./ in an inventory display.
  , _entityDescription :: Text             -- ^ A longer-form description.
  , _entityOrientation :: Maybe (V2 Int)   -- ^ The entity's orientation (if it has one).
  , _entityProperties  :: [EntityProperty] -- ^ Properties of the entity.
  , _entityInventory   :: Inventory
  }

  -- Note that an entity does not have a location, because the
  -- location of an entity is implicit in the way it is stored (by
  -- location).

  deriving (Show, Generic)

-- | The @Hashable@ instance for @Entity@ ignores the cached hash
--   value and simply combines the other fields.
instance Hashable Entity where
  hashWithSalt s (Entity _ disp nm descr or props inv)
    = s `hashWithSalt` disp
        `hashWithSalt` nm
        `hashWithSalt` descr
        `hashWithSalt` or
        `hashWithSalt` props
        `hashWithSalt` inv

-- | Entities are compared by hash for efficiency.
instance Eq Entity where
  (==) = (==) `on` _entityHash

-- | Entities are compared by hash for efficiency.
instance Ord Entity where
  compare = compare `on` _entityHash

-- | Recompute an entity's hash value.
rehashEntity :: Entity -> Entity
rehashEntity e = e { _entityHash = hash e }

-- | Create an entity with an empty inventory (automatically filling
--   in the hash value).
mkEntity
  :: Display          -- ^ Display
  -> Text             -- ^ Entity name
  -> Text             -- ^ Entity description
  -> Maybe (V2 Int)   -- ^ Orientation
  -> [EntityProperty] -- ^ Properties
  -> Entity
mkEntity disp nm descr or props = rehashEntity $ Entity 0 disp nm descr or props empty

------------------------------------------------------------
-- Entity lenses
------------------------------------------------------------

-- Our own custom lenses which properly recompute the cached hash
-- value each time something gets updated.

-- | Make a lens for Entity that recomputes the hash after setting.
hashedLens :: (Entity -> a) -> (Entity -> a -> Entity) -> Lens' Entity a
hashedLens get set = lens get (\e a -> rehashEntity $ set e a)

-- | Get the hash of an entity.  Note that this is a getter, not a
--   lens; the "Swarm.Game.Entity" module carefully maintains some
--   internal invariants ensuring that hashes work properly, and by
--   golly, no one else is going to mess that up.
entityHash :: Getter Entity Int
entityHash = to _entityHash

-- | The 'Display' explaining how to draw this entity in the world display.
entityDisplay :: Lens' Entity Display
entityDisplay = hashedLens _entityDisplay (\e x -> e { _entityDisplay = x })

-- | The name of the entity.
entityName :: Lens' Entity Text
entityName = hashedLens _entityName (\e x -> e { _entityName = x })

-- | A longer, free-form description of the entity.
entityDescription :: Lens' Entity Text
entityDescription = hashedLens _entityDescription (\e x -> e { _entityDescription = x })

-- | The direction this entity is facing (if it has one).
entityOrientation :: Lens' Entity (Maybe (V2 Int))
entityOrientation = hashedLens _entityOrientation (\e x -> e { _entityOrientation = x })

-- | The properties enjoyed by this entity.
entityProperties :: Lens' Entity [EntityProperty]
entityProperties = hashedLens _entityProperties (\e x -> e { _entityProperties = x })

-- | The inventory of other entities carried by this entity.
inventory :: Lens' Entity Inventory
inventory = hashedLens _entityInventory (\e x -> e { _entityInventory = x })

-- | Display an entity as a single character.
displayEntity :: Entity -> Widget n
displayEntity e
  = withAttr (e ^. entityDisplay . displayAttr)
  $ str [lookupDisplay (e ^. entityOrientation) (e ^. entityDisplay)]

------------------------------------------------------------
-- Inventory
------------------------------------------------------------

type Count = Int

-- Invariant:
-- | An inventory is really just a bag/multiset of entities.  That is,
--   it contains some entities, along with the number of times each
--   occurs.
newtype Inventory = Inventory { unInventory :: IntMap (Count, Entity) }
  deriving (Show, Generic)

instance Wrapped Inventory where
  type Unwrapped Inventory = IntMap (Count, Entity)

instance Hashable Inventory where
  -- Don't look at Entity records themselves --- just hash their keys,
  -- which are already a hash.
  hashWithSalt = hashUsing (map (second fst) . IM.assocs . unInventory)

-- | The empty inventory.
empty :: Inventory
empty = Inventory IM.empty

-- | Insert an entity into an inventory.  If the inventory already
--   contains this entity, then only its count will be incremented.
insert :: Entity -> Inventory -> Inventory
insert e = _Wrapped' %~ IM.insertWith (\(m,_) (n,_) -> (m+n,e)) (e ^. entityHash) (1,e)

-- | Look up an entity in an inventory, returning the number of copies
--   contained.
lookup :: Entity -> Inventory -> Count
lookup e = maybe 0 fst . IM.lookup (e ^. entityHash) . unInventory

-- | Delete a single copy of a certain entity from an inventory.
delete :: Entity -> Inventory -> Inventory
delete = deleteCount 1

-- | Delete a specified number of copies of an entity from an inventory.
deleteCount :: Count -> Entity -> Inventory -> Inventory
deleteCount n e = _Wrapped' %~ IM.alter (removeCount n) (e ^. entityHash)
  where
    removeCount :: Count -> Maybe (Count, a) -> Maybe (Count, a)
    removeCount _ Nothing       = Nothing
    removeCount k (Just (n, a))
      | k >= n    = Nothing
      | otherwise = Just (n-k, a)

-- | Delete all copies of a certain entity from an inventory.
deleteAll :: Entity -> Inventory -> Inventory
deleteAll e = _Wrapped' %~ IM.alter (const Nothing) (e ^. entityHash)

-- | Get the entities in an inventory and their associated counts.
elems :: Inventory -> [(Count, Entity)]
elems = IM.elems . unInventory

------------------------------------------------------------
-- Basic entities
------------------------------------------------------------

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
treeEntity = mkEntity
  (defaultEntityDisplay 'T' & displayAttr .~ plantAttr)
  "Tree"
  "It is a tree."
  Nothing
  [Harvestable]

rockEntity :: Entity
rockEntity = mkEntity
  (defaultEntityDisplay '@' & displayAttr .~ rockAttr)
  "Rock"
  "A rock."
  Nothing
  [Solid]
