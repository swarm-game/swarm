{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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
module Swarm.Game.Entity (
  -- * Properties
  EntityProperty (..),
  GrowthTime (..),
  defaultGrowthTime,

  -- * Entities
  Entity,
  mkEntity,

  -- ** Lenses
  -- $lenses
  entityDisplay,
  entityName,
  entityPlural,
  entityNameFor,
  entityDescription,
  entityOrientation,
  entityGrowth,
  entityYields,
  entityProperties,
  hasProperty,
  entityCapabilities,
  entityInventory,
  entityHash,

  -- ** Entity map
  EntityMap (..),
  buildEntityMap,
  loadEntities,
  lookupEntityName,
  deviceForCap,

  -- * Inventories
  Inventory,
  Count,
  Number (..),

  -- ** Construction
  empty,
  singleton,
  fromList,
  fromElems,

  -- ** Lookup
  lookup,
  lookupByName,
  countByName,
  contains,
  contains0plus,
  elems,
  isSubsetOf,
  isEmpty,
  inventoryCapabilities,

  -- ** Modification
  insert,
  insertCount,
  delete,
  deleteCount,
  deleteAll,
  union,
  difference,
) where

import Control.Arrow ((&&&))
import Control.Lens (Getter, Lens', lens, to, view, (^.), _2)
import Control.Monad.IO.Class
import Data.Bifunctor (bimap, first)
import Data.Char (toLower)
import Data.Function (on)
import Data.Hashable
import Data.Int (Int64)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Set (Set)
import Data.Set.Lens (setOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml
import GHC.Generics (Generic)
import Linear (V2)
import Swarm.Game.Display
import Swarm.Language.Capability
import Swarm.Language.Number (Number (..))
import Swarm.Util (dataNotFound, getDataFileNameSafe, plural, reflow)
import Swarm.Util.Yaml
import Text.Read (readMaybe)
import Witch
import Prelude hiding (lookup)

------------------------------------------------------------
-- Properties
------------------------------------------------------------

-- | Various properties that an entity can have, which affect how
--   robots can interact with it.
data EntityProperty
  = -- | Robots can't move onto a cell containing this entity.
    Unwalkable
  | -- | Robots can pick this up (via 'Swarm.Language.Syntax.Grab' or 'Swarm.Language.Syntax.Harvest').
    Portable
  | -- | Regrows from a seed after it is harvested.
    Growable
  | -- | Regenerates infinitely when grabbed or harvested.
    Infinite
  | -- | Robots drown if they walk on this without a boat.
    Liquid
  | -- | Robots automatically know what this is without having to scan it.
    Known
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Hashable)

instance ToJSON EntityProperty where
  toJSON = String . from . map toLower . show

instance FromJSON EntityProperty where
  parseJSON = withText "EntityProperty" tryRead
   where
    tryRead :: Text -> Parser EntityProperty
    tryRead t = case readMaybe . from . T.toTitle $ t of
      Just c -> return c
      Nothing -> fail $ "Unknown entity property " ++ from t

-- | How long an entity takes to regrow.  This represents the minimum
--   and maximum amount of time taken by one growth stage (there are
--   two stages).  The actual time for each stage will be chosen
--   uniformly at random between these two values.
newtype GrowthTime = GrowthTime (Integer, Integer)
  deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

defaultGrowthTime :: GrowthTime
defaultGrowthTime = GrowthTime (100, 200)

------------------------------------------------------------
-- Entity
------------------------------------------------------------

-- | A record to hold information about an entity.
--
--   The constructor for 'Entity' is intentionally not exported.  To
--   construct one manually, use the 'mkEntity' function.
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
  { -- | A hash value computed from the other fields
    _entityHash :: Int
  , -- | The way this entity should be displayed on the world map.
    _entityDisplay :: Display
  , -- | The name of the entity, used /e.g./ in an inventory display.
    _entityName :: Text
  , -- | The plural of the entity name, in case it is irregular.  If
    --   this field is @Nothing@, default pluralization heuristics
    --   will be used (see 'plural').
    _entityPlural :: Maybe Text
  , -- | A longer-form description. Each 'Text' value is one
    --   paragraph.
    _entityDescription :: [Text]
  , -- | The entity's orientation (if it has one).  For example, when
    --   a robot moves, it moves in the direction of its orientation.
    _entityOrientation :: Maybe (V2 Int64)
  , -- | If this entity grows, how long does it take?
    _entityGrowth :: Maybe GrowthTime
  , -- | The name of a different entity obtained when this entity is
    -- grabbed.
    _entityYields :: Maybe Text
  , -- | Properties of the entity.
    _entityProperties :: [EntityProperty]
  , -- | Capabilities provided by this entity.
    _entityCapabilities :: [Capability]
  , -- | Inventory of other entities held by this entity.
    _entityInventory :: Inventory
  }
  -- Note that an entity does not have a location, because the
  -- location of an entity is implicit in the way it is stored (by
  -- location).

  deriving (Show, Generic)

-- | The @Hashable@ instance for @Entity@ ignores the cached hash
--   value and simply combines the other fields.
instance Hashable Entity where
  hashWithSalt s (Entity _ disp nm pl descr orient grow yld props caps inv) =
    s `hashWithSalt` disp
      `hashWithSalt` nm
      `hashWithSalt` pl
      `hashWithSalt` descr
      `hashWithSalt` orient
      `hashWithSalt` grow
      `hashWithSalt` yld
      `hashWithSalt` props
      `hashWithSalt` caps
      `hashWithSalt` inv

-- | Entities are compared by hash for efficiency.
instance Eq Entity where
  (==) = (==) `on` _entityHash

-- | Entities are compared by hash for efficiency.
instance Ord Entity where
  compare = compare `on` _entityHash

-- | Recompute an entity's hash value.
rehashEntity :: Entity -> Entity
rehashEntity e = e {_entityHash = hash e}

-- | Create an entity with no orientation, an empty inventory,
--   providing no capabilities (automatically filling in the hash
--   value).
mkEntity ::
  -- | Display
  Display ->
  -- | Entity name
  Text ->
  -- | Entity description
  [Text] ->
  -- | Properties
  [EntityProperty] ->
  -- | Capabilities
  [Capability] ->
  Entity
mkEntity disp nm descr props caps =
  rehashEntity $ Entity 0 disp nm Nothing descr Nothing Nothing Nothing props caps empty

------------------------------------------------------------
-- Entity map
------------------------------------------------------------

-- | An 'EntityMap' is a data structure containing all the loaded
--   entities, allowing them to be looked up either by name or by what
--   capabilities they provide (if any).
data EntityMap = EntityMap
  { entitiesByName :: Map Text Entity
  , entitiesByCap :: Map Capability [Entity]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Semigroup EntityMap where
  EntityMap n1 c1 <> EntityMap n2 c2 = EntityMap (n1 <> n2) (c1 <> c2)

instance Monoid EntityMap where
  mempty = EntityMap M.empty M.empty
  mappend = (<>)

-- | Find an entity with the given name.
lookupEntityName :: Text -> EntityMap -> Maybe Entity
lookupEntityName nm = M.lookup nm . entitiesByName

-- | Find all entities which are devices that provide the given
--   capability.
deviceForCap :: Capability -> EntityMap -> [Entity]
deviceForCap cap = fromMaybe [] . M.lookup cap . entitiesByCap

-- | Build an 'EntityMap' from a list of entities.  The idea is that
--   this will be called once at startup, when loading the entities
--   from a file; see 'loadEntities'.
buildEntityMap :: [Entity] -> EntityMap
buildEntityMap es =
  EntityMap
    { entitiesByName = M.fromList . map (view entityName &&& id) $ es
    , entitiesByCap = M.fromListWith (<>) . concatMap (\e -> map (,[e]) (e ^. entityCapabilities)) $ es
    }

------------------------------------------------------------
-- Serialization
------------------------------------------------------------

instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \v ->
    rehashEntity
      <$> ( Entity 0
              <$> v .: "display"
              <*> v .: "name"
              <*> v .:? "plural"
              <*> (map reflow <$> (v .: "description"))
              <*> v .:? "orientation"
              <*> v .:? "growth"
              <*> v .:? "yields"
              <*> v .:? "properties" .!= []
              <*> v .:? "capabilities" .!= []
              <*> pure empty
          )

-- | If we have access to an 'EntityMap', we can parse the name of an
--   'Entity' as a string and look it up in the map.
instance FromJSONE EntityMap Entity where
  parseJSONE = withTextE "entity name" $ \name ->
    E $ \em -> case lookupEntityName name em of
      Nothing -> fail $ "Unknown entity: " ++ from @Text name
      Just e -> return e

instance ToJSON Entity where
  toJSON e =
    object $
      [ "display" .= (e ^. entityDisplay)
      , "name" .= (e ^. entityName)
      , "description" .= (e ^. entityDescription)
      ]
        ++ ["plural" .= (e ^. entityPlural) | isJust (e ^. entityPlural)]
        ++ ["orientation" .= (e ^. entityOrientation) | isJust (e ^. entityOrientation)]
        ++ ["growth" .= (e ^. entityGrowth) | isJust (e ^. entityGrowth)]
        ++ ["yields" .= (e ^. entityYields) | isJust (e ^. entityYields)]
        ++ ["properties" .= (e ^. entityProperties) | not . null $ e ^. entityProperties]
        ++ ["capabilities" .= (e ^. entityCapabilities) | not . null $ e ^. entityCapabilities]

-- | Load entities from a data file called @entities.yaml@, producing
--   either an 'EntityMap' or a pretty-printed parse error.
loadEntities :: MonadIO m => m (Either Text EntityMap)
loadEntities = liftIO $ do
  let f = "entities.yaml"
  mayFileName <- getDataFileNameSafe f
  case mayFileName of
    Nothing -> Left <$> dataNotFound f
    Just fileName -> bimap (from . prettyPrintParseException) buildEntityMap <$> decodeFileEither fileName

------------------------------------------------------------
-- Entity lenses
------------------------------------------------------------

-- $lenses
-- Our own custom lenses which properly recompute the cached hash
-- value each time something gets updated.  See
-- https://byorgey.wordpress.com/2021/09/17/automatically-updated-cached-views-with-lens/
-- for the approach used here.

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
entityDisplay = hashedLens _entityDisplay (\e x -> e {_entityDisplay = x})

-- | The name of the entity.
entityName :: Lens' Entity Text
entityName = hashedLens _entityName (\e x -> e {_entityName = x})

-- | The irregular plural version of the entity's name, if there is
--   one.
entityPlural :: Lens' Entity (Maybe Text)
entityPlural = hashedLens _entityPlural (\e x -> e {_entityPlural = x})

-- | Get a version of the entity's name appropriate to the
--   number---the singular name for 1, and a plural name for any other
--   number.  The plural name is obtained either by looking it up if
--   irregular, or by applying standard heuristics otherwise.
entityNameFor :: Int -> Getter Entity Text
entityNameFor 1 = entityName
entityNameFor _ = to $ \e ->
  case e ^. entityPlural of
    Just pl -> pl
    Nothing -> plural (e ^. entityName)

-- | A longer, free-form description of the entity.  Each 'Text' value
--   represents a paragraph.
entityDescription :: Lens' Entity [Text]
entityDescription = hashedLens _entityDescription (\e x -> e {_entityDescription = x})

-- | The direction this entity is facing (if it has one).
entityOrientation :: Lens' Entity (Maybe (V2 Int64))
entityOrientation = hashedLens _entityOrientation (\e x -> e {_entityOrientation = x})

-- | How long this entity takes to grow, if it regrows.
entityGrowth :: Lens' Entity (Maybe GrowthTime)
entityGrowth = hashedLens _entityGrowth (\e x -> e {_entityGrowth = x})

-- | The name of a different entity yielded when this entity is
--   grabbed, if any.
entityYields :: Lens' Entity (Maybe Text)
entityYields = hashedLens _entityYields (\e x -> e {_entityYields = x})

-- | The properties enjoyed by this entity.
entityProperties :: Lens' Entity [EntityProperty]
entityProperties = hashedLens _entityProperties (\e x -> e {_entityProperties = x})

-- | Test whether an entity has a certain property.
hasProperty :: Entity -> EntityProperty -> Bool
hasProperty e p = p `elem` (e ^. entityProperties)

-- | The capabilities this entity provides when installed.
entityCapabilities :: Lens' Entity [Capability]
entityCapabilities = hashedLens _entityCapabilities (\e x -> e {_entityCapabilities = x})

-- | The inventory of other entities carried by this entity.
entityInventory :: Lens' Entity Inventory
entityInventory = hashedLens _entityInventory (\e x -> e {_entityInventory = x})

------------------------------------------------------------
-- Inventory
------------------------------------------------------------

type Count = Number

-- | An inventory is really just a bag/multiset of entities.  That is,
--   it contains some entities, along with the number of times each
--   occurs.  Entities can be looked up directly, or by name.
data Inventory = Inventory
  { -- Main map
    counts :: IntMap (Count, Entity)
  , -- Mirrors the main map; just caching the ability to look up by
    -- name.
    byName :: Map Text IntSet
  , -- Cached hash of the inventory, using a homomorphic hashing scheme
    -- (see https://github.com/swarm-game/swarm/issues/229).
    --
    -- Invariant: equal to Sum_{(k,e) \in counts} (k+1) * (e ^. entityHash).
    -- The k+1 is so the hash distinguishes between having a 0 count of something
    -- and not having it as a key in the map at all.
    inventoryHash :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON)

instance Hashable Inventory where
  -- Just return cached hash value.
  hash = inventoryHash
  hashWithSalt s = hashWithSalt s . inventoryHash

-- | Inventories are compared by hash for efficiency.
instance Eq Inventory where
  (==) = (==) `on` hash

-- | Look up an entity in an inventory, returning the number of copies
--   contained.
lookup :: Entity -> Inventory -> Count
lookup e (Inventory cs _ _) = maybe 0 fst $ IM.lookup (e ^. entityHash) cs

-- | Look up an entity by name in an inventory, returning a list of
--   matching entities.  Note, if this returns some entities, it does
--   *not* mean we necessarily have any in our inventory!  It just
--   means we *know about* them.  If you want to know whether you have
--   any, use 'lookup' and see whether the resulting 'Count' is
--   positive, or just use 'countByName' in the first place.
lookupByName :: Text -> Inventory -> [Entity]
lookupByName name (Inventory cs byN _) =
  maybe [] (map (snd . (cs IM.!)) . IS.elems) (M.lookup (T.toLower name) byN)

-- | Look up an entity by name and see how many there are in the
--   inventory.  If there are multiple entities with the same name, it
--   just picks the first one returned from 'lookupByName'.
countByName :: Text -> Inventory -> Count
countByName name inv =
  maybe 0 (`lookup` inv) (listToMaybe (lookupByName name inv))

-- | The empty inventory.
empty :: Inventory
empty = Inventory IM.empty M.empty 0

-- | Create an inventory containing one entity.
singleton :: Entity -> Inventory
singleton = flip insert empty

-- | Insert an entity into an inventory.  If the inventory already
--   contains this entity, then only its count will be incremented.
insert :: Entity -> Inventory -> Inventory
insert = insertCount 1

-- | Create an inventory from a list of entities.
fromList :: [Entity] -> Inventory
fromList = foldl' (flip insert) empty

-- | Create an inventory from a list of entities and their counts.
fromElems :: [(Count, Entity)] -> Inventory
fromElems = insertInventoryList empty

insertInventoryList :: Inventory -> [(Count, Entity)] -> Inventory
insertInventoryList = foldl' (flip (uncurry insertCount))

-- | Internal way to shift numbers to positive integers when hashing.
--
-- THIS DOES NOT FOLLOW ARITHMETIC LAWS, USE EXTREME CAUTION!
hashCount :: Num p => Number -> p
hashCount c = case c of
  PosInfinity -> 1
  Integer a | a >= 0 -> 1 + fromIntegral a
  _neg -> error $ "Count should never be negative! Value: " ++ show c

-- | Insert a certain number of copies of an entity into an inventory.
--   If the inventory already contains this entity, then only its
--   count will be incremented.
insertCount :: Count -> Entity -> Inventory -> Inventory
insertCount k e inv@(Inventory countsMap byNameMap h) =
  if k < 0
    then deleteCount (-k) e inv
    else
      Inventory
        newCountsMap
        newByName
        (h + addedHash) -- homomorphic hashing
 where
  eHash = e ^. entityHash
  sumCounts _key (m, _) (n, _) = (m + n, e)
  (mOrig, newCountsMap) = IM.insertLookupWithKey sumCounts eHash (k, e) countsMap
  nameKey = T.toLower $ e ^. entityName
  newByName = M.insertWith IS.union nameKey (IS.singleton eHash) byNameMap
  -- Include the hash of an entity once just for "knowing about" it;
  -- then include the hash once per actual copy of the entity.  In
  -- other words, having k copies of e in the inventory contributes
  -- (k+1)*(e ^. entityHash) to the inventory hash.  The reason for
  -- doing this is so that the inventory hash changes even when we
  -- insert 0 copies of something, since having 0 copies of something
  -- is different than not having it as a key at all; having 0 copies
  -- signals that we at least "know about" the entity.
  --
  -- EXEPT WE HAVE TO ACCOUNT FOR INFINITY! So we compare the new count.
  addedHash = case fst <$> mOrig of
    Nothing -> (hashCount k + 1) * eHash -- we know the entity was not known
    Just orig -> (hashCount (k + orig) - hashCount orig) * eHash

-- | Check whether an inventory contains at least one of a given entity.
contains :: Inventory -> Entity -> Bool
contains inv e = lookup e inv > 0

-- | Check whether an inventory has an entry for entity (used by robots).
contains0plus :: Entity -> Inventory -> Bool
contains0plus e = isJust . IM.lookup (e ^. entityHash) . counts

-- | Check if the first inventory is a subset of the second.
--   Note that entities with a count of 0 are ignored.
isSubsetOf :: Inventory -> Inventory -> Bool
isSubsetOf inv1 inv2 = all (\(n, e) -> lookup e inv2 >= n) (elems inv1)

-- | Check whether an inventory is empty, meaning that it contains 0
--   total entities (although it may still /know about/ some entities, that
--   is, have them as keys with a count of 0).
isEmpty :: Inventory -> Bool
isEmpty = all ((== 0) . fst) . elems

-- | Compute the set of capabilities provided by the devices in an inventory.
inventoryCapabilities :: Inventory -> Set Capability
inventoryCapabilities = setOf (to elems . traverse . _2 . entityCapabilities . traverse)

-- | Delete a single copy of a certain entity from an inventory.
delete :: Entity -> Inventory -> Inventory
delete = deleteCount 1

-- | Delete a specified number of copies of an entity from an inventory.
deleteCount :: Count -> Entity -> Inventory -> Inventory
deleteCount k e inv@(Inventory countsMap byNameMap h) =
  if k < 0
    then insertCount (-k) e inv
    else
      Inventory
        newCountsMap
        byNameMap
        (h - subtractedHash)
 where
  eHash = e ^. entityHash
  (mOrig, newCountsMap) = IM.updateLookupWithKey removeCount eHash countsMap
  subtractSafe PosInfinity _ = PosInfinity
  subtractSafe m n = max 0 (n - m)
  removeCount :: Int -> (Count, a) -> Maybe (Count, a)
  removeCount _k = Just . first (subtractSafe k)
  subtractedHash :: Int
  subtractedHash = case fst <$> mOrig of
    Nothing -> 0
    Just orig -> (hashCount orig - hashCount (subtractSafe k orig)) * eHash

-- | Delete all copies of a certain entity from an inventory.
deleteAll :: Entity -> Inventory -> Inventory
deleteAll e (Inventory countsMap byNameMap h) =
  Inventory
    newCountsMap
    byNameMap
    (h - subtractedHash)
 where
  eHash = e ^. entityHash
  lookAnsSet0 mo = (mo, Just (0, e))
  (mOrig, newCountsMap) = IM.alterF lookAnsSet0 eHash countsMap
  subtractedHash :: Int
  subtractedHash = case fst <$> mOrig of
    Nothing -> 0
    Just 0 -> 0
    Just orig -> (hashCount orig - hashCount 0) * eHash

-- | Get the entities in an inventory and their associated counts.
elems :: Inventory -> [(Count, Entity)]
elems (Inventory cs _ _) = IM.elems cs

-- | Union two inventories.
union :: Inventory -> Inventory -> Inventory
union i1 i2 = i1 `insertInventoryList` elems i2

-- | Subtract the second inventory from the first.
difference :: Inventory -> Inventory -> Inventory
difference inv1 = foldl' (flip (uncurry deleteCount)) inv1 . elems
