{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Generic contexts (mappings from variables to other things, such as
-- types, values, or capability sets) used throughout the codebase.
module Swarm.Language.Context where

import Control.Algebra (Has)
import Control.Effect.Reader (Reader, ask, local)
import Control.Effect.State (State, get, modify)
import Control.Lens.Empty (AsEmpty (..))
import Control.Lens.Prism (prism)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Data (Data)
import Data.Functor ((<&>))
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Semigroup (Sum (..))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import GHC.Generics (Generic)
import Swarm.Pretty (PrettyPrec (..))
import Swarm.Util.JSON (optionsUnwrapUnary)
import Prelude hiding (lookup)

-- | We use 'Text' values to represent variables.
type Var = Text

-- | A "shadow context" records the hash values and structure of a
--   context but does not record the actual values associated to the
--   variables.
type SCtx t = (CtxHash, CtxStruct t)

-- | A data type to record the structure of how a context was built,
--   so that we can later destruct/serialize it effectively.
data CtxStruct t
  = CtxEmpty
  | CtxSingle Var
  | CtxDelete Var t (SCtx t)
  | CtxUnion (SCtx t) (SCtx t)
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
  deriving (Eq, Ord, Show, Data, Generic, ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via Sum Int
  deriving (Num) via Int

-- | A context is a mapping from variable names to things.
data Ctx t = Ctx {unCtx :: Map Var t, sctx :: SCtx t}
  deriving (Eq, Functor, Traversable, Data, Generic)

ctxHash :: Ctx t -> CtxHash
ctxHash (Ctx _ (h, _)) = h

ctxStruct :: Ctx t -> CtxStruct t
ctxStruct (Ctx _ (_, s)) = s

instance Show (Ctx t) where
  show _ = "<Ctx>"

instance Hashable t => Hashable (Ctx t) where
  hash = getCtxHash . ctxHash
  hashWithSalt s = hashWithSalt s . getCtxHash . ctxHash

instance Foldable Ctx where
  foldMap f = foldMap f . unCtx

-- Fold a context with sharing.  XXX
foldCtx ::
  (Has (State (Set CtxHash)) sig m, Has (Reader (Map Var t)) sig m) =>
  r ->
  (Var -> t -> m r) ->
  (Var -> r -> r) ->
  (r -> r -> r) ->
  (CtxHash -> r) ->
  Ctx t ->
  m r
foldCtx e sg del un sn (Ctx m s) = foldSCtx e sg del un sn m s

-- XXX
foldSCtx ::
  forall sig m t r.
  (Has (State (Set CtxHash)) sig m, Has (Reader (Map Var t)) sig m) =>
  r ->
  (Var -> t -> m r) ->
  (Var -> r -> r) ->
  (r -> r -> r) ->
  (CtxHash -> r) ->
  Map Var t ->
  SCtx t ->
  m r
foldSCtx e sg del un sn m = go
 where
  go :: SCtx t -> m r
  go (h, s) = do
    seen <- get
    case h `S.member` seen of
      True -> pure $ sn h
      False -> do
        modify (S.insert h)
        case s of
          CtxEmpty -> pure e
          CtxSingle x -> sg x (m M.! x)
          CtxDelete x t sc -> local (M.insert x t) (go sc) <&> del x
          CtxUnion sc1 sc2 -> un <$> go sc1 <*> go sc2

-- XXX this instance will have to change!!
instance ToJSON t => ToJSON (Ctx t) where
  toJSON = genericToJSON optionsUnwrapUnary

-- XXX this instance will have to change!!
instance FromJSON t => FromJSON (Ctx t) where
  parseJSON = genericParseJSON optionsUnwrapUnary

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

-- | The empty context.
empty :: Ctx t
empty = Ctx M.empty (mempty, CtxEmpty)

-- | The hash for a single variable -> value binding.
singletonHash :: Hashable t => Var -> t -> CtxHash
singletonHash x t = CtxHash $ hashWithSalt (hash x) t

-- | The hash for an entire Map's with of bindings.
mapHash :: Hashable t => Map Var t -> CtxHash
mapHash = M.foldMapWithKey singletonHash

-- | A singleton context.
singleton :: Hashable t => Var -> t -> Ctx t
singleton x t = Ctx (M.singleton x t) (singletonHash x t, CtxSingle x)

-- | Create a Ctx from a Map.
fromMap :: Hashable t => Map Var t -> Ctx t
fromMap m = Ctx m (M.foldrWithKey (insertSCtx Nothing) (mempty, CtxEmpty) m)

-- | Look up a variable in a context.
lookup :: Var -> Ctx t -> Maybe t
lookup x (Ctx m _) = M.lookup x m

-- | Look up a variable in a context in an ambient Reader effect.
lookupR :: Has (Reader (Ctx t)) sig m => Var -> m (Maybe t)
lookupR x = lookup x <$> ask

-- | Delete a variable from a context.
delete :: Hashable t => Var -> Ctx t -> Ctx t
delete x c@(Ctx m s@(h, _)) = case M.lookup x m of
  Nothing -> c
  Just t -> Ctx (M.delete x m) (h - singletonHash x t, CtxDelete x t s)

-- | Get the list of key-value associations from a context.
assocs :: Ctx t -> [(Var, t)]
assocs = M.assocs . unCtx

-- | Get the list of bound variables from a context.
vars :: Ctx t -> [Var]
vars = M.keys . unCtx

-- | XXX
insertSCtx :: Hashable t => Maybe t -> Var -> t -> SCtx t -> SCtx t
insertSCtx old x new s@(h, _) = (h', CtxUnion s (tHash, CtxSingle x))
 where
  tHash = singletonHash x new
  h' = case old of
    Nothing -> h + tHash
    Just t' -> h - singletonHash x t' + tHash

-- | Add a key-value binding to a context (overwriting the old one if
--   the key is already present).
addBinding :: Hashable t => Var -> t -> Ctx t -> Ctx t
addBinding x t (Ctx m s) = Ctx (M.insert x t m) (insertSCtx (M.lookup x m) x t s)

-- | /Right/-biased union of contexts.
union :: Hashable t => Ctx t -> Ctx t -> Ctx t
union (Ctx m1 s1@(h1, _)) (Ctx m2 s2@(h2, _)) = Ctx (m2 `M.union` m1) (h', CtxUnion s1 s2)
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
