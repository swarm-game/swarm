{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
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
import Data.Functor.Const
import Data.Hashable
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Semigroup (Sum (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Swarm.Pretty (PrettyPrec (..))
import Swarm.Util (failT, showT)
import Swarm.Util.JSON (optionsMinimize)
import Swarm.Util.Yaml (FromJSONE, getE, liftE, parseJSONE)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Prelude hiding (lookup)

-- | We use 'Text' values to represent variables.
type Var = Text

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
  deriving (Eq, Ord, Data, Generic, ToJSONKey, FromJSONKey)
  deriving (Semigroup, Monoid) via Sum Int
  deriving (Num) via Int

instance Show CtxHash where
  show (CtxHash h) = printf "%016x" h

instance ToJSON CtxHash where
  toJSON h = toJSON (show h)

instance FromJSON CtxHash where
  parseJSON = withText "hash" $ \t -> case readMaybe ("0x" ++ T.unpack t) of
    Nothing -> fail "Could not parse CtxHash"
    Just h -> pure (CtxHash h)

-- | The hash for a single variable -> value binding.
singletonHash :: Hashable t => Var -> t -> CtxHash
singletonHash x t = CtxHash $ hashWithSalt (hash x) t

-- | The hash for an entire Map's worth of bindings.
mapHash :: Hashable t => Map Var t -> CtxHash
mapHash = M.foldMapWithKey singletonHash

------------------------------------------------------------
-- Context structure

-- | 'CtxF' represents one level of structure of a context: a context
--   is either empty, a singleton, or built via deletion or union.
data CtxF f t
  = CtxEmpty
  | CtxSingle Var t
  | CtxDelete Var t (f t)
  | CtxUnion (f t) (f t)
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Generic)

instance (ToJSON t, ToJSON (f t)) => ToJSON (CtxF f t) where
  toJSON = genericToJSON optionsMinimize

instance (FromJSON t, FromJSON (f t)) => FromJSON (CtxF f t) where
  parseJSON = genericParseJSON optionsMinimize

-- | Map over the recursive structure stored in a 'CtxF'.
restructureCtx :: (f t -> g t) -> CtxF f t -> CtxF g t
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
data CtxTree t = CtxTree CtxHash (CtxF CtxTree t)
  deriving (Eq, Functor, Foldable, Traversable, Data, Generic, ToJSON, FromJSON, Show)

------------------------------------------------------------
-- Contexts

-- | A context is a mapping from variable names to things.  We store
--   both a 'Map' (for efficient lookup) as well as a 'CtxTree' (for
--   sharing-aware serializing/deserializing).
data Ctx t = Ctx {unCtx :: Map Var t, ctxStruct :: CtxTree t}
  deriving (Functor, Traversable, Data, Generic)

-- | Get the top-level hash of a context.
ctxHash :: Ctx t -> CtxHash
ctxHash (Ctx _ (CtxTree h _)) = h

instance Show (Ctx t) where
  -- An auto-derived, naive Show instance blows up as it loses all
  -- sharing, so have `show` simply output a placeholder.
  show _ = "<Ctx>"

-- | Compare contexts for equality just by comparing their hashes.
instance Eq (Ctx t) where
  (==) = (==) `on` ctxHash

instance Hashable t => Hashable (Ctx t) where
  hash = getCtxHash . ctxHash
  hashWithSalt s = hashWithSalt s . getCtxHash . ctxHash

instance Foldable Ctx where
  foldMap f = foldMap f . unCtx

-- | Rebuild a complete 'Ctx' from a 'CtxTree'.
ctxFromTree :: CtxTree t -> Ctx t
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
rollCtx :: Hashable t => CtxF Ctx t -> Ctx t
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
instance ToJSON (Ctx t) where
  toJSON = toJSON . ctxHash

-- | Deserialize a context.  We expect to see a hash, and look it up
--   in the provided CtxMap.
instance FromJSONE (CtxMap CtxTree t) (Ctx t) where
  parseJSONE v = do
    h <- liftE $ parseJSON @CtxHash v
    m <- getE
    case getCtx h m of
      Nothing -> failT ["Encountered unknown context hash", showT h]
      Just ctx -> pure ctx

instance (PrettyPrec t) => PrettyPrec (Ctx t) where
  prettyPrec _ _ = "<Ctx>"

-- | The semigroup operation for contexts is /right/-biased union.
instance Hashable t => Semigroup (Ctx t) where
  (<>) = union

instance Hashable t => Monoid (Ctx t) where
  mempty = empty
  mappend = (<>)

instance AsEmpty (Ctx t) where
  _Empty = prism (const empty) isEmpty
   where
    isEmpty c
      | M.null (unCtx c) = Right ()
      | otherwise = Left c

------------------------------------------------------------
-- Context operations

-- | The empty context.
empty :: Ctx t
-- We could also define empty = rollCtx CtxEmpty but that would introduce an
-- unnecessary Hashable t constraint.
empty = Ctx M.empty (CtxTree mempty CtxEmpty)

-- | A singleton context.
singleton :: Hashable t => Var -> t -> Ctx t
singleton x t = rollCtx $ CtxSingle x t

-- | Create a 'Ctx' from a 'Map'.
fromMap :: Hashable t => Map Var t -> Ctx t
fromMap m = case NE.nonEmpty (M.assocs m) of
  Nothing -> empty
  Just ne -> foldr1 union (NE.map (uncurry singleton) ne)

-- | Look up a variable in a context.
lookup :: Var -> Ctx t -> Maybe t
lookup x = M.lookup x . unCtx

-- | Look up a variable in a context in an ambient Reader effect.
lookupR :: Has (Reader (Ctx t)) sig m => Var -> m (Maybe t)
lookupR x = lookup x <$> ask

-- | Delete a variable from a context.
delete :: Hashable t => Var -> Ctx t -> Ctx t
delete x ctx@(Ctx m _) = case M.lookup x m of
  Nothing -> ctx
  Just t -> rollCtx $ CtxDelete x t ctx

-- | Get the list of key-value associations from a context.
assocs :: Ctx t -> [(Var, t)]
assocs = M.assocs . unCtx

-- | Get the list of bound variables from a context.
vars :: Ctx t -> [Var]
vars = M.keys . unCtx

-- | Add a key-value binding to a context (overwriting the old one if
--   the key is already present).
addBinding :: Hashable t => Var -> t -> Ctx t -> Ctx t
addBinding x t ctx = ctx `union` singleton x t

-- | /Right/-biased union of contexts.
union :: Hashable t => Ctx t -> Ctx t -> Ctx t
union ctx1 ctx2 = rollCtx $ CtxUnion ctx1 ctx2

-- | Locally extend the context with an additional binding.
withBinding :: (Has (Reader (Ctx t)) sig m, Hashable t) => Var -> t -> m a -> m a
withBinding x ty = local (addBinding x ty)

-- | Locally extend the context with an additional context of
--   bindings.
withBindings :: (Has (Reader (Ctx t)) sig m, Hashable t) => Ctx t -> m a -> m a
withBindings ctx = local (`union` ctx)

------------------------------------------------------------
-- Context serializing/deserializing

-- | A 'CtxMap' maps context hashes to context structures.  Those
--   structures could either be complete context trees, or just a
--   single level of structure containing more hashes.
type CtxMap f t = Map CtxHash (CtxF f t)

-- | Reconstitute the context corresponding to a particular hash, by
--   looking it up in a context map.
getCtx :: CtxHash -> CtxMap CtxTree t -> Maybe (Ctx t)
getCtx h m = ctxFromTree . CtxTree h <$> M.lookup h m

-- | Turn a context into a context map containing every subtree of its
--   structure.
toCtxMap :: Ctx t -> CtxMap CtxTree t
toCtxMap (Ctx m s) = run $ execState M.empty (buildCtxMap m s)

-- | Build a context map by keeping track of the incrementally built
--   map in a state effect, and traverse the given context structure
--   to add all subtrees to the map---but, of course, stopping without
--   recursing further whenever we see a hash that is already in the
--   map.
buildCtxMap ::
  forall t m sig.
  Has (State (CtxMap CtxTree t)) sig m =>
  Map Var t ->
  CtxTree t ->
  m ()
buildCtxMap m (CtxTree h s) = do
  cm <- get @(CtxMap CtxTree t)
  unless (h `M.member` cm) $ do
    modify (M.insert h s)
    case s of
      CtxEmpty -> pure ()
      CtxSingle {} -> pure ()
      CtxDelete x t s1 -> buildCtxMap (M.insert x t m) s1
      CtxUnion s1 s2 -> buildCtxMap m s1 *> buildCtxMap m s2

-- | "Dehydrate" a context map by replacing the actual context trees
--   with single structure layers containing only hashes.  A
--   dehydrated context map is very suitable for serializing, since it
--   makes sharing completely explicit---even if a given context is
--   referenced multiple times, the references are simply hash values,
--   and the context is stored only once, under its hash.
dehydrate :: CtxMap CtxTree t -> CtxMap (Const CtxHash) t
dehydrate = M.map (restructureCtx (\(CtxTree h1 _) -> Const h1))

-- | "Rehydrate" a dehydrated context map by replacing every hash with
--   an actual context structure.  We do this by building the result
--   as a lazy, recursive map, replacing each hash by the result we
--   get when looking it up in the map being built.  A context which
--   is referenced multiple times will thus be shared in memory.
rehydrate :: CtxMap (Const CtxHash) t -> CtxMap CtxTree t
rehydrate m = m'
 where
  m' = M.map (restructureCtx (\(Const h) -> CtxTree h (m' M.! h))) m
