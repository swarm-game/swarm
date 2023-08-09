-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Custom extension of 'Semigroup' to 'Monoid' that adds identity +
-- annihilator elements.
module Swarm.Util.Erasable where

-- | Extend a semigroup to a monoid by adding an identity ('ENothing') /and/ an
--   annihilator ('EErase').  That is,
--
--   * `ENothing <> e = e <> ENothing = e`
--   * `EErase <> e = e <> EErase = EErase`
--
--   This allows us to "erase" previous values by combining with
--   'EErase'.  The 'erasableToMaybe' function turns an @Erasable@
--   into a @Maybe@ by collapsing 'ENothing' and 'EErase' both back
--   into 'Nothing'.
data Erasable e = ENothing | EErase | EJust e
  deriving (Show, Eq, Ord, Functor)

instance Semigroup e => Semigroup (Erasable e) where
  ENothing <> e = e
  e <> ENothing = e
  EErase <> _ = EErase
  _ <> EErase = EErase
  EJust e1 <> EJust e2 = EJust (e1 <> e2)

instance Semigroup e => Monoid (Erasable e) where
  mempty = ENothing

-- | Generic eliminator for 'Erasable' values.
erasable :: a -> a -> (e -> a) -> Erasable e -> a
erasable x y z = \case
  ENothing -> x
  EErase -> y
  EJust e -> z e

-- | Convert an 'Erasable' value to 'Maybe', turning both 'ENothing'
--   and 'EErase' into 'Nothing'.
erasableToMaybe :: Erasable e -> Maybe e
erasableToMaybe = erasable Nothing Nothing Just

-- | Inject a 'Maybe' value into 'Erasable' using 'ENothing' and
-- 'EJust'.
maybeToErasable :: Maybe e -> Erasable e
maybeToErasable = maybe ENothing EJust
