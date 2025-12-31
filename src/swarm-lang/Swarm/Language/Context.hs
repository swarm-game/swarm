{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Generic contexts (mappings from variables to other things, such as
-- types, values, or capability sets) used throughout the codebase.
-- For example, while typechecking we use a context to store a mapping
-- from variables in scope to their types. As another example, at
-- runtime, robots store an 'Env' which contains several contexts,
-- mapping variables to things like their current value, any
-- requirements associated with using the variable, and so on.
--
-- The implementation here goes to some effort to make it possible to
-- serialize and deserialize contexts so that sharing is preserved and
-- the encoding of serialized contexts does not blow up due to
-- repeated values.
module Swarm.Language.Context where

import Control.Algebra (Has, run)
import Control.Carrier.State.Strict (execState)
import Control.Effect.Reader (Reader, ask, local)
import Control.Effect.State (State, get, modify)
import Control.Lens.Empty (AsEmpty (..))
import Control.Lens.Prism (prism)
import Control.Monad (unless)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, genericParseJSON, genericToJSON, withText)
import Data.Data (Data)
import Data.Function (on)
import Data.Hashable
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Semigroup (Sum (..))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics (Generic)
import Swarm.Pretty (PrettyPrec (..))
import Swarm.Util (failT, showT)
import Swarm.Util.JSON (optionsMinimize)
import Swarm.Util.Yaml (FromJSONE, getE, liftE, parseJSONE)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Prelude hiding (lookup)

------------------------------------------------------------
-- Context hash

-- | A context hash is a hash value used to identify contexts without
--   having to compare them for equality.  Hash values are computed
--   homomorphically, so that two equal contexts will be guaranteed to
--   have the same hash value, even if they were constructed with a
--   different sequence of operations.
--
--   The downside of this approach is that, /in theory/, there could
--   be hash collisions, that is, two different contexts which
--   nonetheless have the same hash value.  However, this is extremely
--   unlikely.  The benefit is that everything can be purely
--   functional, without the need to thread around some kind of
--   globally unique ID generation effect.
newtype CtxHash = CtxHash {getCtxHash :: Int}
  deriving stock (Data)
  deriving newtype (Eq, Ord, Num, ToJSONKey, FromJSONKey)
  deriving (Semigroup, Monoid) via Sum Int

instance Show CtxHash where
  show (CtxHash h) = printf "%016x" h

instance ToJSON CtxHash where
  toJSON h = toJSON (show h)

instance FromJSON CtxHash where
  parseJSON = withText "hash" $ \t -> case readMaybe ("0x" ++ T.unpack t) of
    Nothing -> fail "Could not parse CtxHash"
    Just h -> pure (CtxHash h)

-- | The hash for a single variable -> value binding.
singletonHash :: (Hashable v, Hashable t) => v -> t -> CtxHash
singletonHash x t = CtxHash $ hashWithSalt (hash x) t

-- | The hash for an entire Map's worth of bindings.
mapHash :: (Hashable v, Hashable t) => Map v t -> CtxHash
mapHash = M.foldMapWithKey singletonHash

------------------------------------------------------------
-- Context structure

-- | 'CtxF' represents one level of structure of a context: a context
--   is either empty, a singleton, or built via deletion or union.
data CtxF f v t
  = CtxEmpty
  | CtxSingle v t
  | CtxDelete v t (f v t)
  | CtxUnion (f v t) (f v t)
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Generic)

instance (ToJSON v, ToJSON t, ToJSON (f v t)) => ToJSON (CtxF f v t) where
  toJSON = genericToJSON optionsMinimize

instance (FromJSON v, FromJSON t, FromJSON (f v t)) => FromJSON (CtxF f v t) where
  parseJSON = genericParseJSON optionsMinimize

-- | Map over the recursive structure stored in a 'CtxF'.
restructureCtx :: (f v t -> g v t) -> CtxF f v t -> CtxF g v t
restructureCtx _ CtxEmpty = CtxEmpty
restructureCtx _ (CtxSingle x t) = CtxSingle x t
restructureCtx h (CtxDelete x t f1) = CtxDelete x t (h f1)
restructureCtx h (CtxUnion f1 f2) = CtxUnion (h f1) (h f2)

-- | A 'CtxTree' is one possible representation of a context,
--   consisting of a structured record of the process by which a
--   context was constructed.  This representation would be terrible
--   for doing efficient variable lookups, but it can be used to
--   efficiently serialize/deserialize the context while recovering
--   sharing.
--
--   It stores a top-level hash of the context, along with a recursive
--   tree built via 'CtxF'.
data CtxTree v t = CtxTree CtxHash (CtxF CtxTree v t)
  deriving (Eq, Functor, Foldable, Traversable, Data, Generic, ToJSON, FromJSON, Show)

------------------------------------------------------------
-- Contexts

-- | A context is a mapping from variable names to things.  We store
--   both a 'Map' (for efficient lookup) as well as a 'CtxTree' (for
--   sharing-aware serializing/deserializing).
data Ctx v t = Ctx {unCtx :: Map v t, ctxStruct :: CtxTree v t}
  deriving (Functor, Traversable, Data, Generic)

-- | Get the top-level hash of a context.
ctxHash :: Ctx v t -> CtxHash
ctxHash (Ctx _ (CtxTree h _)) = h

instance Show (Ctx v t) where
  -- An auto-derived, naive Show instance blows up as it loses all
  -- sharing, so have `show` simply output a placeholder.
  show _ = "<Ctx>"

-- | Compare contexts for equality just by comparing their hashes.
instance Eq (Ctx v t) where
  (==) = (==) `on` ctxHash

instance (Hashable v, Hashable t) => Hashable (Ctx v t) where
  hash = getCtxHash . ctxHash
  hashWithSalt s = hashWithSalt s . getCtxHash . ctxHash

instance Foldable (Ctx v) where
  foldMap f = foldMap f . unCtx

-- | Rebuild a complete 'Ctx' from a 'CtxTree'.
ctxFromTree :: Ord v => CtxTree v t -> Ctx v t
ctxFromTree tree = Ctx (varMap tree) tree
 where
  varMap (CtxTree _ s) = case s of
    CtxEmpty -> M.empty
    CtxSingle x t -> M.singleton x t
    CtxDelete x _ s1 -> M.delete x (varMap s1)
    CtxUnion s1 s2 -> varMap s2 `M.union` varMap s1

-- | "Roll up" one level of context structure while building a new
--   top-level Map and computing an appropriate top-level hash.
--
--   In other words, the input of type @CtxF Ctx t@ represents a
--   context where the topmost level of structure is split out by
--   itself as 'CtxF', with the rest of the recursive structure stored
--   in the embedded 'Ctx' values.  'rollCtx' takes the single level
--   of structure with recursive subtrees and "rolls them up" into one
--   recursive tree.
rollCtx :: (Ord v, Hashable v, Hashable t) => CtxF Ctx v t -> Ctx v t
rollCtx s = Ctx m (CtxTree h (restructureCtx ctxStruct s))
 where
  (m, h) = case s of
    CtxEmpty -> (M.empty, 0)
    CtxSingle x t -> (M.singleton x t, singletonHash x t)
    CtxDelete x _ (Ctx m1 (CtxTree h1 _)) -> case M.lookup x m1 of
      Nothing -> (m1, h1)
      Just t' -> (M.delete x m1, h1 - singletonHash x t')
    CtxUnion (Ctx m1 (CtxTree h1 _)) (Ctx m2 (CtxTree h2 _)) -> (m2 `M.union` m1, h')
     where
      -- `Data.Map.intersection l r` returns a map with common keys,
      -- but values from `l`.  The values in m1 are the ones we want
      -- to subtract from the hash, since they are the ones that will
      -- be overwritten.
      overwritten = M.intersection m1 m2
      h' = h1 + h2 - mapHash overwritten

------------------------------------------------------------
-- Context instances

-- | Serialize a context simply as its hash; we assume that a
--   top-level CtxMap has been seralized somewhere, from which we can
--   recover this context by looking it up.
instance ToJSON (Ctx v t) where
  toJSON = toJSON . ctxHash

-- | Deserialize a context.  We expect to see a hash, and look it up
--   in the provided CtxMap.
instance Ord v => FromJSONE (CtxMap CtxTree v t) (Ctx v t) where
  parseJSONE v = do
    h <- liftE $ parseJSON @CtxHash v
    m <- getE
    case getCtx h m of
      Nothing -> failT ["Encountered unknown context hash", showT h]
      Just ctx -> pure ctx

instance (PrettyPrec t) => PrettyPrec (Ctx v t) where
  prettyPrec _ _ = "<Ctx>"

-- | The semigroup operation for contexts is /right/-biased union.
instance (Ord v, Hashable v, Hashable t) => Semigroup (Ctx v t) where
  (<>) = union

instance (Ord v, Hashable v, Hashable t) => Monoid (Ctx v t) where
  mempty = empty
  mappend = (<>)

instance AsEmpty (Ctx v t) where
  _Empty = prism (const empty) isEmpty
   where
    isEmpty c
      | M.null (unCtx c) = Right ()
      | otherwise = Left c

------------------------------------------------------------
-- Context operations

-- | The empty context.
empty :: Ctx v t
-- We could also define empty = rollCtx CtxEmpty but that would introduce an
-- unnecessary Hashable t constraint.
empty = Ctx M.empty (CtxTree mempty CtxEmpty)

-- | A singleton context.
singleton :: (Ord v, Hashable v, Hashable t) => v -> t -> Ctx v t
singleton x t = rollCtx $ CtxSingle x t

-- | Create a 'Ctx' from a 'Map'.
fromMap :: (Ord v, Hashable v, Hashable t) => Map v t -> Ctx v t
fromMap m = case NE.nonEmpty (M.assocs m) of
  Nothing -> empty
  Just ne -> foldr1 union (NE.map (uncurry singleton) ne)

-- | Look up a variable in a context.
lookup :: Ord v => v -> Ctx v t -> Maybe t
lookup x = M.lookup x . unCtx

-- | Look up a variable in a context in an ambient Reader effect.
lookupR :: (Ord v, Has (Reader (Ctx v t)) sig m) => v -> m (Maybe t)
lookupR x = lookup x <$> ask

-- | Delete a variable from a context.
delete :: (Ord v, Hashable v, Hashable t) => v -> Ctx v t -> Ctx v t
delete x ctx@(Ctx m _) = case M.lookup x m of
  Nothing -> ctx
  Just t -> rollCtx $ CtxDelete x t ctx

-- | 'restrict r c' restricts the context c to only those keys contained in r.
restrict :: (Ord v, Hashable v, Hashable t) => Ctx v s -> Ctx v t -> Ctx v t
restrict r ctx = foldr delete ctx keysToDelete
 where
  -- What we really want is ctx ∩ r, but we must implement it in terms
  -- of deletion, which is the primitive operation we have available
  -- for operating on contexts.
  --
  -- Ctx - (Ctx - R)
  --   = Ctx ∩ ~(Ctx ∩ ~R)         { A - B = A ∩ ~B }
  --   = Ctx ∩ (~Ctx ∪ R)          { de Morgan }
  --   = (Ctx ∩ ~Ctx) ∪ (Ctx ∩ R)  { distributivity }
  --   = ∅ ∪ (Ctx ∩ R)             { A ∩ ~A = ∅ }
  --   = Ctx ∩ R                   { ∅ identity for ∪ }
  keysToDelete = M.keysSet (unCtx ctx) `S.difference` M.keysSet (unCtx r)

-- | Get the list of key-value associations from a context.
assocs :: Ctx v t -> [(v, t)]
assocs = M.assocs . unCtx

-- | Get the list of bound variables from a context.
vars :: Ctx v t -> [v]
vars = M.keys . unCtx

-- | Add a key-value binding to a context (overwriting the old one if
--   the key is already present).
addBinding :: (Ord v, Hashable v, Hashable t) => v -> t -> Ctx v t -> Ctx v t
addBinding x t ctx = ctx `union` singleton x t

-- | /Right/-biased union of contexts.
union :: (Ord v, Hashable v, Hashable t) => Ctx v t -> Ctx v t -> Ctx v t
union ctx1 ctx2 = rollCtx $ CtxUnion ctx1 ctx2

-- | Locally extend the context with an additional binding.
withBinding :: (Has (Reader (Ctx v t)) sig m, Ord v, Hashable v, Hashable t) => v -> t -> m a -> m a
withBinding x ty = local (addBinding x ty)

-- | Locally extend the context with an additional context of
--   bindings.
withBindings :: (Has (Reader (Ctx v t)) sig m, Ord v, Hashable v, Hashable t) => Ctx v t -> m a -> m a
withBindings ctx = local (`union` ctx)

------------------------------------------------------------
-- Context serializing/deserializing

-- | A 'CtxMap' maps context hashes to context structures.  Those
--   structures could either be complete context trees, or just a
--   single level of structure containing more hashes.
type CtxMap f v t = Map CtxHash (CtxF f v t)

-- | Reconstitute the context corresponding to a particular hash, by
--   looking it up in a context map.
getCtx :: Ord v => CtxHash -> CtxMap CtxTree v t -> Maybe (Ctx v t)
getCtx h m = ctxFromTree . CtxTree h <$> M.lookup h m

-- | Turn a context into a context map containing every subtree of its
--   structure.
toCtxMap :: Ord v => Ctx v t -> CtxMap CtxTree v t
toCtxMap (Ctx m s) = run $ execState M.empty (buildCtxMap m s)

-- | Build a context map by keeping track of the incrementally built
--   map in a state effect, and traverse the given context structure
--   to add all subtrees to the map---but, of course, stopping without
--   recursing further whenever we see a hash that is already in the
--   map.
buildCtxMap ::
  forall v t m sig.
  (Ord v, Has (State (CtxMap CtxTree v t)) sig m) =>
  Map v t ->
  CtxTree v t ->
  m ()
buildCtxMap m (CtxTree h s) = do
  cm <- get @(CtxMap CtxTree v t)
  unless (h `M.member` cm) $ do
    modify (M.insert h s)
    case s of
      CtxEmpty -> pure ()
      CtxSingle {} -> pure ()
      CtxDelete x t s1 -> buildCtxMap (M.insert x t m) s1
      CtxUnion s1 s2 -> buildCtxMap m s1 *> buildCtxMap m s2

newtype ConstHash v t = ConstHash CtxHash
  deriving newtype (Eq, Show, ToJSON, FromJSON)

-- | "Dehydrate" a context map by replacing the actual context trees
--   with single structure layers containing only hashes.  A
--   dehydrated context map is very suitable for serializing, since it
--   makes sharing completely explicit---even if a given context is
--   referenced multiple times, the references are simply hash values,
--   and the context is stored only once, under its hash.
dehydrate :: CtxMap CtxTree v t -> CtxMap ConstHash v t
dehydrate = M.map (restructureCtx (\(CtxTree h1 _) -> ConstHash h1))

-- | "Rehydrate" a dehydrated context map by replacing every hash with
--   an actual context structure.  We do this by building the result
--   as a lazy, recursive map, replacing each hash by the result we
--   get when looking it up in the map being built.  A context which
--   is referenced multiple times will thus be shared in memory.
rehydrate :: CtxMap ConstHash v t -> CtxMap CtxTree v t
rehydrate m = m'
 where
  m' = M.map (restructureCtx (\(ConstHash h) -> CtxTree h (m' M.! h))) m
