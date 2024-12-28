{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
-- Description: Object that exists in the world
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
  -- * Entity properties
  EntityName,
  EntityProperty (..),
  GrowthTime (..),
  GrowthSpread (..),
  Growth (..),
  defaultGrowth,
  Combustibility (..),
  defaultCombustibility,

  -- * Entities
  Entity,
  mkEntity,

  -- ** Fields
  -- $lenses
  entityDisplay,
  entityName,
  entityPlural,
  entityNameFor,
  entityDescription,
  entityTags,
  entityOrientation,
  entityGrowth,
  entityCombustion,
  entityYields,
  entityProperties,
  hasProperty,
  entityCapabilities,
  entityBiomes,
  entityInventory,
  entityHash,

  -- ** Entity map
  EntityMap (..),
  buildEntityMap,
  lookupEntityE,
  validateEntityAttrRefs,
  loadEntities,
  allEntities,
  lookupEntityName,
  devicesForCap,

  -- * Inventories
  Inventory,

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
  extantElemsWithCapability,
  entitiesByCapability,

  -- ** Modification
  insert,
  insertCount,
  delete,
  deleteCount,
  deleteAll,
  union,
  difference,
) where

import Control.Algebra (Has)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Carrier.Throw.Either (liftEither)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Throw (Throw, throwError)
import Control.Lens (Getter, Lens', lens, to, view, (^.))
import Control.Monad (forM_, unless, (<=<))
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.Either.Extra (maybeToEither)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.Functor.Identity
import Data.Hashable
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (isJust, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set (fromList, member)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml
import GHC.Generics (Generic)
import Swarm.Failure
import Swarm.Game.Device
import Swarm.Game.Display
import Swarm.Game.Entity.Cosmetic (WorldAttr (..))
import Swarm.Game.Entity.Cosmetic.Assignment (worldAttributes)
import Swarm.Game.Ingredients
import Swarm.Game.Location
import Swarm.Game.Terrain (TerrainType)
import Swarm.Language.Capability
import Swarm.Language.Syntax (Syntax)
import Swarm.Language.Text.Markdown (Document, docToText)
import Swarm.ResourceLoading (getDataFileNameSafe)
import Swarm.Util (binTuples, failT, findDup, plural, quote, (?))
import Swarm.Util.Effect (withThrow)
import Swarm.Util.Yaml
import Text.Read (readMaybe)
import Witch
import Prelude hiding (Foldable (..), lookup)

------------------------------------------------------------
-- Properties
------------------------------------------------------------

-- | A type representing entity names, currently a synonym for 'Text'.
--   In the future it is conceivable that it might become more complex.
type EntityName = Text

-- | Various properties that an entity can have, which affect how
--   robots can interact with it.
data EntityProperty
  = -- | Robots can't move onto a cell containing this entity.
    Unwalkable
  | -- | Robots can pick this up (via 'Swarm.Language.Syntax.Grab' or 'Swarm.Language.Syntax.Harvest').
    Pickable
  | -- | Robots can 'Swarm.Language.Syntax.Push' this
    Pushable
  | -- | Obstructs the view of robots that attempt to "scout"
    Opaque
  | -- | Is automatically rendered as a contiguous border
    Boundary
  | -- | Regrows from a seed after it is harvested.
    Growable
  | -- | Can burn when ignited (either via 'Swarm.Language.Syntax.Ignite' or by
    --   an adjacent burning entity).
    Combustible
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
      Nothing -> failT ["Unknown entity property", t]

data GrowthSpread = GrowthSpread
  { spreadRadius :: Int
  -- ^ in terms of manhattan distance
  , spreadDensity :: Float
  -- ^ average number of tiles within the
  -- radius that will be seeded per
  -- growth cycle
  }
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON)

instance FromJSON GrowthSpread where
  parseJSON = withObject "GrowthSpread" $ \v -> do
    spreadRadius <- v .: "radius"
    spreadDensity <- v .: "density"
    pure GrowthSpread {..}

data Growth = Growth
  { maturesTo :: Maybe EntityName
  -- ^ Entity this turns into after growth is complete,
  -- if something different than self
  , growthSpread :: Maybe GrowthSpread
  , growthTime :: GrowthTime
  }
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON)

instance FromJSON Growth where
  parseJSON x =
    (Growth Nothing Nothing <$> parseJSON x)
      <|> parseFullGrowth x
   where
    parseFullGrowth = withObject "Growth" $ \v -> do
      maturesTo <- v .:? "mature"
      growthSpread <- v .:? "spread"
      growthTime <- v .: "duration"
      pure Growth {..}

-- | How long an entity takes to regrow.  This represents the minimum
--   and maximum amount of time taken by one growth stage (there are
--   two stages).  The actual time for each stage will be chosen
--   uniformly at random between these two values.
newtype GrowthTime = GrowthTime (Integer, Integer)
  deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

-- | The default growth time (100, 200) for a growable entity with no
--   growth time specification.
defaultGrowthTime :: GrowthTime
defaultGrowthTime = GrowthTime (100, 200)

defaultGrowth :: Growth
defaultGrowth = Growth Nothing Nothing defaultGrowthTime

-- | Properties of combustion.
data Combustibility = Combustibility
  { ignition :: Double
  -- ^ Rate of ignition by a neighbor, per tick.
  --   If this rate is denoted \(\lambda\), the probability of
  --   ignition over a period of \(t\) ticks is \(1 - e^{-\lambda t}\).
  --   See <https://math.stackexchange.com/a/1243629>.
  , duration :: (Integer, Integer)
  -- ^ min and max tick counts for combustion to persist
  , product :: Maybe EntityName
  -- ^ what entity, if any, is left over after combustion
  }
  deriving (Eq, Ord, Show, Read, Generic, Hashable, FromJSON, ToJSON)

-- | The default combustion specification for a combustible entity
--   with no combustion specification:
--
--   * ignition rate 0.5
--   * duration (100, 200)
--   * product @ash@
defaultCombustibility :: Combustibility
defaultCombustibility = Combustibility 0.5 (100, 200) (Just "ash")

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
  { _entityHash :: Int
  -- ^ A hash value computed from the other fields
  , _entityDisplay :: Display
  -- ^ The way this entity should be displayed on the world map.
  , _entityName :: EntityName
  -- ^ The name of the entity, used /e.g./ in an inventory display.
  , _entityPlural :: Maybe Text
  -- ^ The plural of the entity name, in case it is irregular.  If
  --   this field is @Nothing@, default pluralization heuristics
  --   will be used (see 'plural').
  , _entityDescription :: Document Syntax
  -- ^ A longer-form description. Each 'Text' value is one
  --   paragraph.
  , _entityTags :: Set Text
  -- ^ A set of categories to which the entity belongs
  , _entityOrientation :: Maybe Heading
  -- ^ The entity's orientation (if it has one).  For example, when
  --   a robot moves, it moves in the direction of its orientation.
  , _entityGrowth :: Maybe Growth
  -- ^ If this entity grows, how long does it take?
  , _entityCombustion :: Maybe Combustibility
  -- ^ If this entity is combustible, how spreadable is it?
  , _entityYields :: Maybe Text
  -- ^ The name of a different entity obtained when this entity is
  -- grabbed.
  , _entityProperties :: Set EntityProperty
  -- ^ Properties of the entity.
  , _entityBiomes :: Set TerrainType
  -- ^ Terrain in which growth may occur. Empty means no restrictions.
  , _entityCapabilities :: SingleEntityCapabilities EntityName
  -- ^ Capabilities provided by this entity.
  , _entityInventory :: Inventory
  -- ^ Inventory of other entities held by this entity.
  }
  -- Note that an entity does not have a location, because the
  -- location of an entity is implicit in the way it is stored (by
  -- location).

  deriving (Show, Generic)

-- | The @Hashable@ instance for @Entity@ ignores the cached hash
--   value and simply combines the other fields.
instance Hashable Entity where
  hashWithSalt s (Entity _ disp nm pl descr tags orient grow combust yld props biomes caps inv) =
    s
      `hashWithSalt` disp
      `hashWithSalt` nm
      `hashWithSalt` pl
      `hashWithSalt` docToText descr
      `hashWithSalt` tags
      `hashWithSalt` orient
      `hashWithSalt` grow
      `hashWithSalt` combust
      `hashWithSalt` yld
      `hashWithSalt` props
      `hashWithSalt` biomes
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
  Document Syntax ->
  -- | Properties
  [EntityProperty] ->
  -- | Capabilities
  Set Capability ->
  Entity
mkEntity disp nm descr props caps =
  rehashEntity $
    Entity
      0
      disp
      nm
      Nothing
      descr
      mempty
      Nothing
      Nothing
      Nothing
      Nothing
      (Set.fromList props)
      mempty
      (zeroCostCapabilities caps)
      empty

------------------------------------------------------------
-- Entity map
------------------------------------------------------------

-- | An 'EntityMap' is a data structure containing all the loaded
--   entities, allowing them to be looked up either by name or by what
--   capabilities they provide (if any).
--
--   Also preserves the original definition order from the scenario
--   file and canonical definition list.
--   This enables scenario authors to specify iteration order of
--   the 'Swarm.Language.Syntax.TagMembers' command.
data EntityMap = EntityMap
  { entitiesByName :: Map EntityName Entity
  , entitiesByCap :: MultiEntityCapabilities Entity Entity
  , entityDefinitionOrder :: [Entity]
  }
  deriving (Eq, Show, Generic, ToJSON)

-- | Right-biased union of 'EntityMap's.
--
--   Note that duplicates in a single 'EntityMap' are precluded by the
--   'buildEntityMap' function.  But it is possible for the right-hand
--   'EntityMap' to override members of the left-hand with the same name.
--   For example, this is how custom entities defined in a scenario
--   can override standard entities. This replacement happens
--   automatically with 'Map' (as long as we keep in mind that Map
--   union is *left*-biased), but needs to be explicitly handled for the
--   list concatenation of 'entityDefinitionOrder' (overridden entries
--   are removed from the former 'EntityMap'), and for 'entitiesByCap',
--   which are organized by capability rather than by entity.
instance Semigroup EntityMap where
  EntityMap n1 c1 d1 <> EntityMap n2 c2 d2 =
    EntityMap
      (n2 <> n1)
      (removeOverriddenDevices c1 <> c2)
      (filter notOverridden d1 <> d2)
   where
    notOverridden :: Entity -> Bool
    notOverridden = (`M.notMember` n2) . view entityName
    removeOverriddenDevices (Capabilities m) =
      Capabilities
        . runIdentity
        -- traverseMaybeWithKey allows us in one operation to filter
        -- the NonEmpty list of devices for each capability, turn the
        -- resulting lists into Maybe NonEmpty, and then remove from
        -- the map any that became Nothing.  However, it is more
        -- general than we need: it gives us access to the key which
        -- we don't need (this explains the call to 'const'), and runs
        -- in an arbitrary Applicative which we also don't need (this
        -- explains the Identity).
        . M.traverseMaybeWithKey
          (const (Identity . NE.nonEmpty . NE.filter (notOverridden . device)))
        $ m

instance Monoid EntityMap where
  mempty = EntityMap M.empty mempty []
  mappend = (<>)

-- | Get a list of all the entities in the entity map.
allEntities :: EntityMap -> [Entity]
allEntities (EntityMap _ _ x) = x

-- | Find an entity with the given name.
lookupEntityName :: Text -> EntityMap -> Maybe Entity
lookupEntityName nm = M.lookup nm . entitiesByName

-- | Find all entities which are devices that provide the given
--   capability.
devicesForCap :: Capability -> EntityMap -> [Entity]
devicesForCap cap = maybe [] (NE.toList . NE.map device) . M.lookup cap . getMap . entitiesByCap

-- | Validates references to 'Display' attributes
validateEntityAttrRefs :: Has (Throw LoadingFailure) sig m => Set WorldAttr -> [Entity] -> m ()
validateEntityAttrRefs validAttrs es =
  forM_ namedEntities $ \(eName, ent) ->
    case ent ^. entityDisplay . displayAttr of
      AWorld n ->
        unless (Set.member (WorldAttr $ T.unpack n) validAttrs)
          . throwError
          . CustomMessage
          $ T.unwords
            [ "Nonexistent attribute"
            , quote n
            , "referenced by entity"
            , quote eName
            ]
      _ -> return ()
 where
  namedEntities = map (view entityName &&& id) es

-- | Build an 'EntityMap' from a list of entities.  The idea is that
--   this will be called once at startup, when loading the entities
--   from a file; see 'loadEntities'.
buildEntityMap :: Has (Throw LoadingFailure) sig m => [Entity] -> m EntityMap
buildEntityMap es = do
  case findDup (map fst namedEntities) of
    Nothing -> return ()
    Just duped -> throwError $ Duplicate Entities duped
  case combineEntityCapsM entsByName es of
    Left x -> throwError $ CustomMessage x
    Right ebc ->
      return $
        EntityMap
          { entitiesByName = entsByName
          , entitiesByCap = ebc
          , entityDefinitionOrder = es
          }
 where
  namedEntities = map (view entityName &&& id) es
  entsByName = M.fromList namedEntities

-- Compare to 'combineEntityCapsM'
combineEntityCaps ::
  [Entity] ->
  MultiEntityCapabilities Entity EntityName
combineEntityCaps = mconcat . map mkForEntity
 where
  mkForEntity e = f <$> e ^. entityCapabilities
   where
    f = pure . DeviceUseCost e

lookupEntityE :: Map Text b -> Text -> Either Text b
lookupEntityE em en =
  maybeToEither err $ M.lookup en em
 where
  err = T.unwords [quote en, "is not a valid entity name"]

combineEntityCapsM ::
  Map EntityName Entity ->
  [Entity] ->
  Either Text (MultiEntityCapabilities Entity Entity)
combineEntityCapsM em =
  fmap mconcat . mapM mkForEntity
 where
  transformCaps = (traverse . traverse) (lookupEntityE em)

  mkForEntity e =
    fmap f <$> transformCaps (e ^. entityCapabilities)
   where
    f = pure . DeviceUseCost e

------------------------------------------------------------
-- Serialization
------------------------------------------------------------

instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \v -> do
    let _entityHash = 0
    _entityDisplay <- v .: "display"
    _entityName <- v .: "name"
    _entityPlural <- v .:? "plural"
    _entityDescription <- v .: "description"
    _entityTags <- v .:? "tags" .!= mempty
    _entityOrientation <- v .:? "orientation"
    _entityGrowth <- v .:? "growth"
    _entityCombustion <- v .:? "combustion"
    _entityYields <- v .:? "yields"
    _entityProperties <- v .:? "properties" .!= mempty
    _entityBiomes <- v .:? "biomes" .!= mempty
    _entityCapabilities <- v .:? "capabilities" .!= Capabilities mempty
    let _entityInventory = empty
    pure $ rehashEntity Entity {..}

-- | If we have access to an 'EntityMap', we can parse the name of an
--   'Entity' as a string and look it up in the map.
instance FromJSONE EntityMap Entity where
  parseJSONE = withTextE "entity name" $ \name ->
    E $ \em -> case lookupEntityName name em of
      Nothing -> failT ["Unknown entity:", name]
      Just e -> return e

instance ToJSON Entity where
  toJSON e =
    object $
      [ "display" .= (e ^. entityDisplay)
      , "name" .= (e ^. entityName)
      , "description" .= (e ^. entityDescription)
      , "tags" .= (e ^. entityTags)
      ]
        ++ ["plural" .= (e ^. entityPlural) | isJust (e ^. entityPlural)]
        ++ ["orientation" .= (e ^. entityOrientation) | isJust (e ^. entityOrientation)]
        ++ ["growth" .= (e ^. entityGrowth) | isJust (e ^. entityGrowth)]
        ++ ["yields" .= (e ^. entityYields) | isJust (e ^. entityYields)]
        ++ ["properties" .= (e ^. entityProperties) | not . null $ e ^. entityProperties]
        ++ ["capabilities" .= (e ^. entityCapabilities) | not . M.null . getMap $ e ^. entityCapabilities]

-- | Load entities from a data file called @entities.yaml@, producing
--   either an 'EntityMap' or a parse error.
loadEntities ::
  (Has (Throw SystemFailure) sig m, Has (Lift IO) sig m) =>
  m EntityMap
loadEntities = do
  let entityFile = "entities.yaml"
      entityFailure = AssetNotLoaded (Data Entities) entityFile
  fileName <- getDataFileNameSafe Entities entityFile
  decoded <-
    withThrow (entityFailure . CanNotParseYaml) . (liftEither <=< sendIO) $
      decodeFileEither fileName

  withThrow entityFailure $ validateEntityAttrRefs (M.keysSet worldAttributes) decoded
  withThrow entityFailure $ buildEntityMap decoded

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
entityName :: Lens' Entity EntityName
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
entityDescription :: Lens' Entity (Document Syntax)
entityDescription = hashedLens _entityDescription (\e x -> e {_entityDescription = x})

-- | A set of categories to which the entity belongs
entityTags :: Lens' Entity (Set Text)
entityTags = hashedLens _entityTags (\e x -> e {_entityTags = x})

-- | The direction this entity is facing (if it has one).
entityOrientation :: Lens' Entity (Maybe Heading)
entityOrientation = hashedLens _entityOrientation (\e x -> e {_entityOrientation = x})

-- | How long this entity takes to grow, if it regrows.
entityGrowth :: Lens' Entity (Maybe Growth)
entityGrowth = hashedLens _entityGrowth (\e x -> e {_entityGrowth = x})

-- | Susceptibility to and duration of combustion
entityCombustion :: Lens' Entity (Maybe Combustibility)
entityCombustion = hashedLens _entityCombustion (\e x -> e {_entityCombustion = x})

-- | The name of a different entity yielded when this entity is
--   grabbed, if any.
entityYields :: Lens' Entity (Maybe Text)
entityYields = hashedLens _entityYields (\e x -> e {_entityYields = x})

-- | The properties enjoyed by this entity.
entityProperties :: Lens' Entity (Set EntityProperty)
entityProperties = hashedLens _entityProperties (\e x -> e {_entityProperties = x})

-- | Test whether an entity has a certain property.
hasProperty :: Entity -> EntityProperty -> Bool
hasProperty e p = p `elem` (e ^. entityProperties)

-- | The capabilities this entity provides when equipped.
entityCapabilities :: Lens' Entity (SingleEntityCapabilities EntityName)
entityCapabilities = hashedLens _entityCapabilities (\e x -> e {_entityCapabilities = x})

-- | The inventory of other entities carried by this entity.
entityBiomes :: Lens' Entity (Set TerrainType)
entityBiomes = hashedLens _entityBiomes (\e x -> e {_entityBiomes = x})

-- | The inventory of other entities carried by this entity.
entityInventory :: Lens' Entity Inventory
entityInventory = hashedLens _entityInventory (\e x -> e {_entityInventory = x})

------------------------------------------------------------
-- Inventory
------------------------------------------------------------

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
--   /not/ mean we necessarily have any in our inventory!  It just
--   means we /know about/ them.  If you want to know whether you have
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
fromElems = foldl' (flip (uncurry insertCount)) empty

-- | Insert a certain number of copies of an entity into an inventory.
--   If the inventory already contains this entity, then only its
--   count will be incremented.
insertCount :: Count -> Entity -> Inventory -> Inventory
insertCount k e (Inventory cs byN h) =
  Inventory
    (IM.insertWith (\(m, _) (n, _) -> (m + n, e)) (e ^. entityHash) (k, e) cs)
    (M.insertWith IS.union (T.toLower $ e ^. entityName) (IS.singleton (e ^. entityHash)) byN)
    (h + (k + extra) * (e ^. entityHash)) -- homomorphic hashing
 where
  -- Include the hash of an entity once just for "knowing about" it;
  -- then include the hash once per actual copy of the entity.  In
  -- other words, having k copies of e in the inventory contributes
  -- (k+1)*(e ^. entityHash) to the inventory hash.  The reason for
  -- doing this is so that the inventory hash changes even when we
  -- insert 0 copies of something, since having 0 copies of something
  -- is different than not having it as a key at all; having 0 copies
  -- signals that we at least "know about" the entity.
  extra = if (e ^. entityHash) `IM.member` cs then 0 else 1

-- | Check whether an inventory contains at least one of a given entity.
contains :: Inventory -> Entity -> Bool
contains inv e = lookup e inv > 0

-- | Check whether an inventory has an entry for the given entity,
--   even if there are 0 copies.  In particular this is used to
--   indicate whether a robot "knows about" an entity.
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
inventoryCapabilities :: Inventory -> MultiEntityCapabilities Entity EntityName
inventoryCapabilities = combineEntityCaps . nonzeroEntities

-- | List elements that have at least one copy in the inventory.
nonzeroEntities :: Inventory -> [Entity]
nonzeroEntities = map snd . filter ((> 0) . fst) . elems

-- | List elements that possess a given Capability and
-- exist with nonzero count in the inventory.
extantElemsWithCapability :: Capability -> Inventory -> [Entity]
extantElemsWithCapability cap =
  filter (M.member cap . getMap . (^. entityCapabilities)) . nonzeroEntities

-- | Groups entities by the capabilities they offer.
entitiesByCapability :: Inventory -> Map Capability (NE.NonEmpty Entity)
entitiesByCapability inv =
  binTuples entityCapabilityPairs
 where
  getCaps = M.keys . getMap . (^. entityCapabilities)
  entityCapabilityPairs = concatMap ((\e -> map (,e) $ getCaps e) . snd) $ elems inv

-- | Delete a single copy of a certain entity from an inventory.
delete :: Entity -> Inventory -> Inventory
delete = deleteCount 1

-- | Delete a specified number of copies of an entity from an inventory.
deleteCount :: Count -> Entity -> Inventory -> Inventory
deleteCount k e (Inventory cs byN h) = Inventory cs' byN h'
 where
  m = (fst <$> IM.lookup (e ^. entityHash) cs) ? 0
  cs' = IM.adjust removeCount (e ^. entityHash) cs
  h' = h - min k m * (e ^. entityHash)

  removeCount :: (Count, a) -> (Count, a)
  removeCount (n, a) = (max 0 (n - k), a)

-- | Delete all copies of a certain entity from an inventory.
deleteAll :: Entity -> Inventory -> Inventory
deleteAll e (Inventory cs byN h) =
  Inventory
    (IM.adjust (first (const 0)) (e ^. entityHash) cs)
    byN
    (h - n * (e ^. entityHash))
 where
  n = (fst <$> IM.lookup (e ^. entityHash) cs) ? 0

-- | Get the entities in an inventory and their associated counts.
elems :: Inventory -> [(Count, Entity)]
elems (Inventory cs _ _) = IM.elems cs

-- | Union two inventories.
union :: Inventory -> Inventory -> Inventory
union (Inventory cs1 byN1 h1) (Inventory cs2 byN2 h2) =
  Inventory
    (IM.unionWith (\(c1, e) (c2, _) -> (c1 + c2, e)) cs1 cs2)
    (M.unionWith IS.union byN1 byN2)
    (h1 + h2 - common)
 where
  -- Need to subtract off the sum of the hashes in common, because
  -- of the way each entity with count k contributes (k+1) times its
  -- hash.  So if the two inventories share an entity e, just adding their
  -- hashes would mean e now contributes (k+2) times its hash.
  common = IS.foldl' (+) 0 $ IM.keysSet cs1 `IS.intersection` IM.keysSet cs2

-- | Subtract the second inventory from the first.
difference :: Inventory -> Inventory -> Inventory
difference inv1 = foldl' (flip (uncurry deleteCount)) inv1 . elems
