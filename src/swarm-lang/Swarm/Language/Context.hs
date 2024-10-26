{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Generic contexts (mappings from variables to other things, such as
-- types, values, or capability sets) used throughout the codebase.
module Swarm.Language.Context where

import Control.Algebra (Has)
import Control.Effect.Reader (Reader, ask, local)
import Control.Lens.Empty (AsEmpty (..), pattern Empty)
import Control.Lens.Prism (prism)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Data (Data)
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Semigroup (Sum (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (brackets, emptyDoc, hsep, punctuate)
import Swarm.Pretty (PrettyPrec (..), prettyBinding)
import Swarm.Util.JSON (optionsUnwrapUnary)
import Prelude hiding (lookup)

-- | We use 'Text' values to represent variables.
type Var = Text

-- | A data type to record the structure of how a context was built,
--   so that we can later destruct/serialize it effectively.
data CtxStruct t
  = CtxEmpty
  | CtxSingle Var
  | CtxDelete Var t (CtxStruct t)
  | CtxUnion (CtxStruct t) (CtxStruct t)
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Generic, ToJSON, FromJSON)

-- | A context hash is a hash value used to identify contexts without
--   having to compare them for equality.  Hash values are computed in
--   homomorphically, so that two equal contexts will be guaranteed to
--   have the same hash value, even if they were constructed
--   by a different sequence of operations.
--
--   The downside of this approach is that, /in theory/, there could
--   be a hash collision---two different contexts which nonetheless
--   have the same hash value.  However, this is extremely unlikely.
--   The benefit is that everything can be purely functional, without
--   the need to thread around some kind of globally unique ID
--   generation effect.
newtype CtxHash = CtxHash {getCtxHash :: Int}
  deriving (Eq, Show, Data, Generic, ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via Sum Int
  deriving (Num) via Int

-- | A context is a mapping from variable names to things.
data Ctx t = Ctx {unCtx :: Map Var t, ctxHash :: CtxHash, ctxStruct :: CtxStruct t}
  deriving (Eq, Show, Functor, Traversable, Data, Generic)

instance Hashable t => Hashable (Ctx t) where
  hash = getCtxHash . ctxHash
  hashWithSalt s = hashWithSalt s . getCtxHash . ctxHash

instance Foldable Ctx where
  foldMap f = foldMap f . unCtx

-- XXX this instance will have to change!!
instance ToJSON t => ToJSON (Ctx t) where
  toJSON = genericToJSON optionsUnwrapUnary

-- XXX this instance will have to change!!
instance FromJSON t => FromJSON (Ctx t) where
  parseJSON = genericParseJSON optionsUnwrapUnary

-- XXX this instance will have to change!!
instance (PrettyPrec t) => PrettyPrec (Ctx t) where
  prettyPrec _ Empty = emptyDoc
  prettyPrec _ (assocs -> bs) = brackets (hsep (punctuate "," (map prettyBinding bs)))

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

-- | The empty context.
empty :: Ctx t
empty = Ctx M.empty mempty CtxEmpty

-- | The hash for a single variable -> value binding.
singletonHash :: Hashable t => Var -> t -> CtxHash
singletonHash x t = CtxHash $ hashWithSalt (hash x) t

-- | The hash for an entire Map's with of bindings.
mapHash :: Hashable t => Map Var t -> CtxHash
mapHash = M.foldMapWithKey singletonHash

-- | A singleton context.
singleton :: Hashable t => Var -> t -> Ctx t
singleton x t = Ctx (M.singleton x t) (singletonHash x t) (CtxSingle x)

-- | Create a Ctx from a Map.
fromMap :: Hashable t => Map Var t -> Ctx t
fromMap m = Ctx m (mapHash m) (M.foldrWithKey (\x _ -> CtxUnion (CtxSingle x)) CtxEmpty m)

-- | Look up a variable in a context.
lookup :: Var -> Ctx t -> Maybe t
lookup x (Ctx m _ _) = M.lookup x m

-- | Look up a variable in a context in an ambient Reader effect.
lookupR :: Has (Reader (Ctx t)) sig m => Var -> m (Maybe t)
lookupR x = lookup x <$> ask

-- | Delete a variable from a context.
delete :: Hashable t => Var -> Ctx t -> Ctx t
delete x c@(Ctx m h s) = case M.lookup x m of
  Nothing -> c
  Just t -> Ctx (M.delete x m) (h - singletonHash x t) (CtxDelete x t s)

-- | Get the list of key-value associations from a context.
assocs :: Ctx t -> [(Var, t)]
assocs = M.assocs . unCtx

-- | Get the list of bound variables from a context.
vars :: Ctx t -> [Var]
vars = M.keys . unCtx

-- | Add a key-value binding to a context (overwriting the old one if
--   the key is already present).
addBinding :: Hashable t => Var -> t -> Ctx t -> Ctx t
addBinding x t (Ctx m h s) = Ctx (M.insert x t m) h' (CtxUnion s (CtxSingle x))
 where
  h' = case M.lookup x m of
    Nothing -> h + singletonHash x t
    Just t' -> h - singletonHash x t' + singletonHash x t

-- | /Right/-biased union of contexts.
union :: Hashable t => Ctx t -> Ctx t -> Ctx t
union (Ctx m1 h1 s1) (Ctx m2 h2 s2) = Ctx (m2 `M.union` m1) h' (CtxUnion s1 s2)
 where
  -- `Data.Map.intersection l r` returns a map with common keys, but values from `l`
  overwritten = M.intersection m1 m2
  h' = h1 + h2 - mapHash overwritten

-- | Locally extend the context with an additional binding.
withBinding :: (Has (Reader (Ctx t)) sig m, Hashable t) => Var -> t -> m a -> m a
withBinding x ty = local (addBinding x ty)

-- | Locally extend the context with an additional context of
--   bindings.
withBindings :: (Has (Reader (Ctx t)) sig m, Hashable t) => Ctx t -> m a -> m a
withBindings ctx = local (`union` ctx)
