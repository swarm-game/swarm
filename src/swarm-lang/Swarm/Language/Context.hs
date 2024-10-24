{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (brackets, emptyDoc, hsep, punctuate)
import Swarm.Pretty (PrettyPrec (..), prettyBinding)
import Swarm.Util.JSON (optionsUnwrapUnary)
import Prelude hiding (lookup)

-- | We use 'Text' values to represent variables.
type Var = Text

-- | A context is a mapping from variable names to things.
newtype Ctx t = Ctx {unCtx :: Map Var t}
  deriving (Eq, Show, Functor, Foldable, Traversable, Data, Generic)

instance ToJSON t => ToJSON (Ctx t) where
  toJSON = genericToJSON optionsUnwrapUnary

instance FromJSON t => FromJSON (Ctx t) where
  parseJSON = genericParseJSON optionsUnwrapUnary

instance (PrettyPrec t) => PrettyPrec (Ctx t) where
  prettyPrec _ Empty = emptyDoc
  prettyPrec _ (assocs -> bs) = brackets (hsep (punctuate "," (map prettyBinding bs)))

-- | The semigroup operation for contexts is /right/-biased union.
instance Semigroup (Ctx t) where
  (<>) = union

instance Monoid (Ctx t) where
  mempty = empty
  mappend = (<>)

instance AsEmpty (Ctx t) where
  _Empty = prism (const empty) isEmpty
   where
    isEmpty (Ctx c)
      | M.null c = Right ()
      | otherwise = Left (Ctx c)

-- | The empty context.
empty :: Ctx t
empty = Ctx M.empty

-- | A singleton context.
singleton :: Var -> t -> Ctx t
singleton x t = Ctx (M.singleton x t)

-- | Look up a variable in a context.
lookup :: Var -> Ctx t -> Maybe t
lookup x (Ctx c) = M.lookup x c

-- | Look up a variable in a context in an ambient Reader effect.
lookupR :: Has (Reader (Ctx t)) sig m => Var -> m (Maybe t)
lookupR x = lookup x <$> ask

-- | Delete a variable from a context.
delete :: Var -> Ctx t -> Ctx t
delete x (Ctx c) = Ctx (M.delete x c)

-- | Get the list of key-value associations from a context.
assocs :: Ctx t -> [(Var, t)]
assocs = M.assocs . unCtx

-- | Get the list of bound variables from a context.
vars :: Ctx t -> [Var]
vars = M.keys . unCtx

-- | Add a key-value binding to a context (overwriting the old one if
--   the key is already present).
addBinding :: Var -> t -> Ctx t -> Ctx t
addBinding x t (Ctx c) = Ctx (M.insert x t c)

-- | /Right/-biased union of contexts.
union :: Ctx t -> Ctx t -> Ctx t
union (Ctx c1) (Ctx c2) = Ctx (c2 `M.union` c1)

-- | Locally extend the context with an additional binding.
withBinding :: Has (Reader (Ctx t)) sig m => Var -> t -> m a -> m a
withBinding x ty = local (addBinding x ty)

-- | Locally extend the context with an additional context of
--   bindings.
withBindings :: Has (Reader (Ctx t)) sig m => Ctx t -> m a -> m a
withBindings ctx = local (`union` ctx)
