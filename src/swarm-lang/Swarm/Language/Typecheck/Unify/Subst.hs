{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Type inference for the Swarm language.  For the approach used here,
-- see
-- https://byorgey.wordpress.com/2021/09/08/implementing-hindley-milner-with-the-unification-fd-library/ .
module Swarm.Language.Typecheck.Unify.Subst (
  -- * Substitutions
  Substitution (..),
  dom,

  -- ** Subst type class
  Subst (..),

  -- ** Constructing/destructing substitutions
  idS,
  (|->),
  fromList,
  toList,

  -- ** Substitution operations
  (@@),
  compose,
  lookup,
)
where

import Control.Monad.Free
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import Prelude hiding (lookup)

-- | A value of type @Substitution n a@ is a substitution which maps
--   names of type @n@ (the /domain/, see 'dom') to values of type
--   @a@.  Substitutions can be /applied/ to certain terms (see
--   'applySubst'), replacing any free occurrences of names in the
--   domain with their corresponding values.  Thus, substitutions can
--   be thought of as functions of type @Term -> Term@ (for suitable
--   @Term@s that contain names and values of the right type).
--
--   Concretely, substitutions are stored using a @Map@.
newtype Substitution n a = Substitution {getSubst :: Map n a}
  deriving (Eq, Ord, Show)

instance Functor (Substitution n) where
  fmap f (Substitution m) = Substitution (M.map f $ m)

-- instance Pretty a => Pretty (Substitution a) where
--   pretty (Substitution s) = do
--     let es = map (uncurry prettyMapping) (M.assocs s)
--     ds <- punctuate "," es
--     braces (hsep ds)

-- prettyMapping :: (Pretty a, Members '[Reader PA, LFresh] r) => Name a -> a -> Sem r (Doc ann)
-- prettyMapping x a = pretty x <+> "->" <+> pretty a

-- | The domain of a substitution is the set of names for which the
--   substitution is defined.
dom :: Substitution n a -> Set n
dom = M.keysSet . getSubst

-- | The identity substitution, /i.e./ the unique substitution with an
--   empty domain, which acts as the identity function on terms.
idS :: Substitution n a
idS = Substitution M.empty

-- | Construct a singleton substitution, which maps the given name to
--   the given value.
(|->) :: n -> a -> Substitution n a
x |-> t = Substitution (M.singleton x t)

-- | Class of things supporting substitution.  @Subst n b a@ means
--   that we can apply a substitution of type @Substitution n b@ to a
--   value of type @a@, replacing all the free names of type @n@
--   inside the @a@ with values of type @b@, resulting in a new value
--   of type @a@.
class Subst n b a where
  subst :: Substitution n b -> a -> a

instance (Ord n, Functor f) => Subst n (Free f n) (Free f n) where
  subst = (=<<) . (!) . getSubst

-- | Compose two substitutions.  Applying @s1 \@\@ s2@ is the same as
--   applying first @s2@, then @s1@; that is, semantically,
--   composition of substitutions corresponds exactly to function
--   composition when they are considered as functions on terms.
--
--   As one would expect, composition is associative and has 'idS' as
--   its identity.
(@@) :: (Ord n, Subst n a a) => Substitution n a -> Substitution n a -> Substitution n a
(Substitution s1) @@ (Substitution s2) = Substitution ((M.map (subst (Substitution s1))) s2 `M.union` s1)

-- | Compose a whole container of substitutions.  For example,
--   @compose [s1, s2, s3] = s1 \@\@ s2 \@\@ s3@.
compose :: (Ord n, Subst n a a, Foldable t) => t (Substitution n a) -> Substitution n a
compose = foldr (@@) idS

-- | Create a substitution from an association list of names and
--   values.
fromList :: Ord n => [(n, a)] -> Substitution n a
fromList = Substitution . M.fromList

-- | Convert a substitution into an association list.
toList :: Substitution n a -> [(n, a)]
toList = M.assocs . getSubst

-- | Look up the value a particular name maps to under the given
--   substitution; or return @Nothing@ if the name being looked up is
--   not in the domain.
lookup :: Ord n => n -> Substitution n a -> Maybe a
lookup x (Substitution m) = M.lookup x m
