-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Description: Common definitions used in both the naive and fast
-- implementations of unification.
module Swarm.Effect.Unify.Common where

import Control.Algebra
import Control.Effect.State (State, get)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Prelude hiding (lookup)

------------------------------------------------------------
-- Substitutions

-- | A value of type @Subst n a@ is a substitution which maps
--   names of type @n@ (the /domain/, see 'dom') to values of type
--   @a@.  Substitutions can be /applied/ to certain terms (see
--   'subst'), replacing any free occurrences of names in the
--   domain with their corresponding values.  Thus, substitutions can
--   be thought of as functions of type @Term -> Term@ (for suitable
--   @Term@s that contain names and values of the right type).
--
--   Concretely, substitutions are stored using a @Map@.
newtype Subst n a = Subst {getSubst :: Map n a}
  deriving (Eq, Ord, Show, Functor)

-- | The domain of a substitution is the set of names for which the
--   substitution is defined.
dom :: Subst n a -> Set n
dom = M.keysSet . getSubst

-- | The identity substitution, /i.e./ the unique substitution with an
--   empty domain, which acts as the identity function on terms.
idS :: Subst n a
idS = Subst M.empty

-- | Construct a singleton substitution, which maps the given name to
--   the given value.
(|->) :: n -> a -> Subst n a
x |-> t = Subst (M.singleton x t)

-- | Insert a new name/value binding into the substitution.
insert :: Ord n => n -> a -> Subst n a -> Subst n a
insert n a (Subst m) = Subst (M.insert n a m)

-- | Look up the value a particular name maps to under the given
--   substitution; or return @Nothing@ if the name being looked up is
--   not in the domain.
lookup :: Ord n => n -> Subst n a -> Maybe a
lookup x (Subst m) = M.lookup x m

-- | Look up a name in a substitution stored in a state effect.
lookupS :: (Ord n, Has (State (Subst n a)) sig m) => n -> m (Maybe a)
lookupS x = lookup x <$> get
